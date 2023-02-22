include Common

let dtd =
  {|<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">|}

let dtd_signal = `Dtd (Some dtd)
let plist_start_signal = `El_start (("", "plist"), [])
let array_start_signal = `El_start (("", "array"), [])
let data_start_signal = `El_start (("", "data"), [])
let date_start_signal = `El_start (("", "date"), [])
let dict_start_signal = `El_start (("", "dict"), [])
let false_start_signal = `El_start (("", "false"), [])
let integer_start_signal = `El_start (("", "integer"), [])
let key_start_signal = `El_start (("", "key"), [])
let real_start_signal = `El_start (("", "real"), [])
let string_start_signal = `El_start (("", "string"), [])
let true_start_signal = `El_start (("", "true"), [])

type lexeme =
  [ `Array_start
  | `Array_end
  | `Data of string
  | `Date of float * float option
  | `Dict_start
  | `Dict_end
  | `False
  | `Int of int
  | `Key of string
  | `Real of float
  | `String of string
  | `True ]

type signal = [ lexeme | `EOI ]

type error =
  [ `Expected_tag of string
  | `Expected_start
  | `Expected_end
  | `Expected_start_or_end
  | `Expected_data
  | `Malformed_base64 of string
  | `Malformed_date of string
  | `Malformed_int of string
  | `Malformed_real of string
  | `Unknown_tag of string ]

exception Error of (int * int) * error

type decoder = { dec_source : Xmlm.input; dec_sink : signal -> unit }

let error_message = function
  | `Expected_tag tag -> "Expected_tag " ^ tag
  | `Expected_start -> "Expected_start"
  | `Expected_end -> "Expected_end"
  | `Expected_start_or_end -> "Expected_start_or_end"
  | `Expected_data -> "Expected_data"
  | `Malformed_base64 s -> "Malformed_base64 " ^ s
  | `Malformed_date s -> "Malformed_date " ^ s
  | `Malformed_int s -> "Malformed_int " ^ s
  | `Malformed_real s -> "Malformed_real " ^ s
  | `Unknown_tag s -> "Unknown_tag " ^ s

let error decoder error = raise (Error (Xmlm.pos decoder.dec_source, error))

let create_decoder source dec_sink =
  { dec_source = Xmlm.make_input (`Fun source); dec_sink }

let is_whitespace = function
  | ' ' | '\x0C' | '\n' | '\r' | '\t' -> true
  | _ -> false

let all_whitespace = String.for_all is_whitespace

let skip_whitespace decoder =
  match Xmlm.peek decoder.dec_source with
  | `Data data when all_whitespace data ->
    ignore (Xmlm.input decoder.dec_source)
  | _ -> ()

let lose_whitespace str =
  let count =
    String.fold_left
      (fun count ch -> if is_whitespace ch then count else count + 1)
      0 str
  in
  let fixed = Bytes.create count in
  let _ =
    String.fold_left
      (fun count ch ->
        if is_whitespace ch then count
        else (
          Bytes.set fixed count ch;
          count + 1))
      0 str
  in
  Bytes.unsafe_to_string fixed

let close decoder =
  match Xmlm.input decoder.dec_source with
  | `El_end -> ()
  | _ -> error decoder `Expected_end

let data decoder =
  match Xmlm.input decoder.dec_source with
  | `Data data ->
    close decoder;
    data
  | `El_end -> ""
  | _ -> error decoder `Expected_data

let rec decode_tag decoder = function
  | "array" ->
    decoder.dec_sink `Array_start;
    let rec loop () =
      skip_whitespace decoder;
      match Xmlm.input decoder.dec_source with
      | `El_end -> decoder.dec_sink `Array_end
      | `El_start ((_, name), _) ->
        decode_tag decoder name;
        loop ()
      | _ -> error decoder `Expected_start_or_end
    in
    loop ()
  | "data" -> (
    let data = data decoder in
    match Base64.decode (lose_whitespace data) with
    | Error _e -> error decoder (`Malformed_base64 data)
    | Ok data -> decoder.dec_sink (`Data data))
  | "date" ->
    let datetime =
      let data = data decoder in
      try ISO8601.Permissive.datetime_tz ~reqtime:false data
      with Failure _ -> error decoder (`Malformed_date data)
    in
    decoder.dec_sink (`Date datetime)
  | "dict" ->
    decoder.dec_sink `Dict_start;
    let rec loop () =
      skip_whitespace decoder;
      match Xmlm.input decoder.dec_source with
      | `El_end -> decoder.dec_sink `Dict_end
      | `El_start ((_, "key"), _) -> (
        decoder.dec_sink (`Key (data decoder));
        skip_whitespace decoder;
        match Xmlm.input decoder.dec_source with
        | `El_start ((_, name), _) ->
          decode_tag decoder name;
          loop ()
        | _ -> error decoder `Expected_start)
      | `El_start ((_, _), _) -> error decoder (`Expected_tag "key")
      | _ -> error decoder `Expected_start_or_end
    in
    loop ()
  | "false" ->
    close decoder;
    decoder.dec_sink `False
  | "integer" -> (
    let data = data decoder in
    match int_of_string_opt data with
    | None -> error decoder (`Malformed_int data)
    | Some int -> decoder.dec_sink (`Int int))
  | "real" -> (
    let data = data decoder in
    match float_of_string_opt data with
    | None -> error decoder (`Malformed_real data)
    | Some float -> decoder.dec_sink (`Real float))
  | "string" -> decoder.dec_sink (`String (data decoder))
  | "true" ->
    close decoder;
    decoder.dec_sink `True
  | s -> error decoder (`Unknown_tag s)

let decode source sink =
  let decoder = create_decoder source sink in
  match Xmlm.input decoder.dec_source with
  | `Dtd _ -> (
    match Xmlm.input decoder.dec_source with
    | `El_start ((_, "plist"), _) -> (
      skip_whitespace decoder;
      match Xmlm.input decoder.dec_source with
      | `El_start ((_, name), _) -> (
        decode_tag decoder name;
        skip_whitespace decoder;
        match Xmlm.input decoder.dec_source with
        | `El_end -> decoder.dec_sink `EOI
        | _ -> error decoder `Expected_end)
      | _ -> error decoder `Expected_start)
    | _ -> error decoder (`Expected_tag "plist"))
  | _ -> ()

type encoder = { enc_source : unit -> signal; enc_sink : Xmlm.output }

let create_encoder enc_source sink =
  { enc_source; enc_sink = Xmlm.make_output (`Fun sink) }

let encode_lexeme sink = function
  | `Array_start -> Xmlm.output sink array_start_signal
  | `Array_end -> Xmlm.output sink `El_end
  | `Data data ->
    Xmlm.output sink data_start_signal;
    let data = Base64.encode_string data in
    if data <> "" then Xmlm.output sink (`Data data);
    Xmlm.output sink `El_end
  | `Date (datetime, None) ->
    Xmlm.output sink date_start_signal;
    Xmlm.output sink (`Data (ISO8601.Permissive.string_of_datetime datetime));
    Xmlm.output sink `El_end
  | `Date (datetime, Some tz) ->
    Xmlm.output sink date_start_signal;
    Xmlm.output sink
      (`Data (ISO8601.Permissive.string_of_datetimezone (datetime, tz)));
    Xmlm.output sink `El_end
  | `Dict_start -> Xmlm.output sink dict_start_signal
  | `Dict_end -> Xmlm.output sink `El_end
  | `False ->
    Xmlm.output sink false_start_signal;
    Xmlm.output sink `El_end
  | `Int int ->
    Xmlm.output sink integer_start_signal;
    Xmlm.output sink (`Data (string_of_int int));
    Xmlm.output sink `El_end
  | `Key key ->
    Xmlm.output sink key_start_signal;
    if key <> "" then Xmlm.output sink (`Data key);
    Xmlm.output sink `El_end
  | `Real float ->
    Xmlm.output sink real_start_signal;
    Xmlm.output sink (`Data (string_of_float float));
    Xmlm.output sink `El_end
  | `String data ->
    Xmlm.output sink string_start_signal;
    if data <> "" then Xmlm.output sink (`Data data);
    Xmlm.output sink `El_end
  | `True ->
    Xmlm.output sink true_start_signal;
    Xmlm.output sink `El_end

let encode source sink =
  let encoder = create_encoder source sink in
  Xmlm.output encoder.enc_sink dtd_signal;
  Xmlm.output encoder.enc_sink (`El_start (("", "plist"), []));
  let rec loop () =
    match encoder.enc_source () with
    | `EOI -> ()
    | #lexeme as lexeme ->
      encode_lexeme encoder.enc_sink lexeme;
      loop ()
  in
  loop ();
  Xmlm.output encoder.enc_sink `El_end

let token_of_signal = function
  | `Array_start -> Parser.ARRAY_START
  | `Array_end -> Parser.ARRAY_END
  | `Data str -> Parser.DATA str
  | `Date date -> Parser.DATE date
  | `Dict_start -> Parser.DICT_START
  | `Dict_end -> Parser.DICT_END
  | `False -> Parser.FALSE
  | `Int int -> Parser.INT int
  | `Key key -> Parser.KEY key
  | `Real float -> Parser.REAL float
  | `String str -> Parser.STRING str
  | `True -> Parser.TRUE
  | `EOI -> Parser.EOI

module I = Parser.MenhirInterpreter

let parse source =
  let exception E of t in
  let checkpoint = ref (Parser.Incremental.main Lexing.dummy_pos) in
  let sink signal =
    let rec loop = function
      | I.InputNeeded _ as checkpoint' -> checkpoint := checkpoint'
      | (I.Shifting _ | I.AboutToReduce _) as checkpoint ->
        loop (I.resume checkpoint)
      | I.HandlingError _ -> assert false
      | I.Accepted v -> raise (E v)
      | I.Rejected -> assert false
    in
    loop
      (I.offer !checkpoint
         (token_of_signal signal, Lexing.dummy_pos, Lexing.dummy_pos))
  in
  try
    decode source sink;
    assert false
  with E v -> v

let of_channel in_channel = parse (fun () -> input_byte in_channel)

let of_string string =
  let idx = ref 0 in
  let source () =
    let i = !idx in
    if i >= String.length string then raise End_of_file
    else (
      idx := i + 1;
      Char.code string.[i])
  in
  parse source

let rec lexemes sink = function
  | `Bool false -> sink `False
  | `Bool true -> sink `True
  | `Data data -> sink (`Data data)
  | `Date date -> sink (`Date date)
  | `Float float -> sink (`Real float)
  | `Int int -> sink (`Int int)
  | `String string -> sink (`String string)
  | `Array elts ->
    sink `Array_start;
    List.iter (lexemes sink) elts;
    sink `Array_end
  | `Dict kvs ->
    sink `Dict_start;
    List.iter
      (fun (k, v) ->
        sink (`Key k);
        lexemes sink v)
      kvs;
    sink `Dict_end

let print sink t =
  let output = Xmlm.make_output (`Fun sink) in
  Xmlm.output output dtd_signal;
  Xmlm.output output plist_start_signal;
  lexemes (encode_lexeme output) t;
  Xmlm.output output `El_end

let to_buffer buffer = print (Buffer.add_uint8 buffer)
let to_channel out_channel = print (output_byte out_channel)

let to_string t =
  let buffer = Buffer.create 512 in
  to_buffer buffer t;
  Buffer.contents buffer
