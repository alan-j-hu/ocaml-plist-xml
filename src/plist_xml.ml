include Plist_tree
include Token
include Error

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

let error input error = raise (Error (Xmlm.pos input, error))

let is_whitespace = function
  | ' ' | '\x0C' | '\n' | '\r' | '\t' -> true
  | _ -> false

let all_whitespace = String.for_all is_whitespace

let skip_whitespace input =
  match Xmlm.peek input with
  | `Data data when all_whitespace data -> ignore (Xmlm.input input)
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

let close input =
  match Xmlm.input input with
  | `El_end -> ()
  | _ -> error input `Expected_end

let data input =
  match Xmlm.input input with
  | `Data data ->
    close input;
    data
  | `El_end -> ""
  | _ -> error input `Expected_data

let rec decode_tag input sink = function
  | "array" ->
    sink `Array_start;
    let rec loop () =
      skip_whitespace input;
      match Xmlm.input input with
      | `El_end -> sink `Array_end
      | `El_start ((_, name), _) ->
        decode_tag input sink name;
        loop ()
      | _ -> error input `Expected_start_or_end
    in
    loop ()
  | "data" -> (
    let data = data input in
    match Base64.decode (lose_whitespace data) with
    | Error _e -> error input (`Malformed_base64 data)
    | Ok data -> sink (`Data data))
  | "date" ->
    let datetime =
      let data = data input in
      try ISO8601.Permissive.datetime_tz ~reqtime:false data
      with Failure _ -> error input (`Malformed_date data)
    in
    sink (`Date datetime)
  | "dict" ->
    sink `Dict_start;
    let rec loop () =
      skip_whitespace input;
      match Xmlm.input input with
      | `El_end -> sink `Dict_end
      | `El_start ((_, "key"), _) -> (
        sink (`Key (data input));
        skip_whitespace input;
        match Xmlm.input input with
        | `El_start ((_, name), _) ->
          decode_tag input sink name;
          loop ()
        | _ -> error input `Expected_start)
      | `El_start ((_, _), _) -> error input (`Expected_tag "key")
      | _ -> error input `Expected_start_or_end
    in
    loop ()
  | "false" ->
    close input;
    sink `False
  | "integer" -> (
    let data = data input in
    match int_of_string_opt data with
    | None -> error input (`Malformed_int data)
    | Some int -> sink (`Int int))
  | "real" -> (
    let data = data input in
    match float_of_string_opt data with
    | None -> error input (`Malformed_real data)
    | Some float -> sink (`Real float))
  | "string" -> sink (`String (data input))
  | "true" ->
    close input;
    sink `True
  | s -> error input (`Unknown_tag s)

let decode source sink =
  let input = Xmlm.make_input (`Fun source) in
  match Xmlm.input input with
  | `Dtd _ -> (
    match Xmlm.input input with
    | `El_start ((_, "plist"), _) -> (
      skip_whitespace input;
      match Xmlm.input input with
      | `El_start ((_, name), _) -> (
        decode_tag input sink name;
        skip_whitespace input;
        match Xmlm.input input with
        | `El_end -> sink `EOI
        | _ -> error input `Expected_end)
      | _ -> error input `Expected_start)
    | _ -> error input (`Expected_tag "plist"))
  | _ -> ()

let encode_token output = function
  | `Array_start -> Xmlm.output output array_start_signal
  | `Array_end -> Xmlm.output output `El_end
  | `Data data ->
    Xmlm.output output data_start_signal;
    let data = Base64.encode_string data in
    if data <> "" then Xmlm.output output (`Data data);
    Xmlm.output output `El_end
  | `Date (datetime, None) ->
    Xmlm.output output date_start_signal;
    Xmlm.output output (`Data (ISO8601.Permissive.string_of_datetime datetime));
    Xmlm.output output `El_end
  | `Date (datetime, Some tz) ->
    Xmlm.output output date_start_signal;
    Xmlm.output output
      (`Data (ISO8601.Permissive.string_of_datetimezone (datetime, tz)));
    Xmlm.output output `El_end
  | `Dict_start -> Xmlm.output output dict_start_signal
  | `Dict_end -> Xmlm.output output `El_end
  | `False ->
    Xmlm.output output false_start_signal;
    Xmlm.output output `El_end
  | `Int int ->
    Xmlm.output output integer_start_signal;
    Xmlm.output output (`Data (string_of_int int));
    Xmlm.output output `El_end
  | `Key key ->
    Xmlm.output output key_start_signal;
    if key <> "" then Xmlm.output output (`Data key);
    Xmlm.output output `El_end
  | `Real float ->
    Xmlm.output output real_start_signal;
    Xmlm.output output (`Data (string_of_float float));
    Xmlm.output output `El_end
  | `String data ->
    Xmlm.output output string_start_signal;
    if data <> "" then Xmlm.output output (`Data data);
    Xmlm.output output `El_end
  | `True ->
    Xmlm.output output true_start_signal;
    Xmlm.output output `El_end

let encode source sink =
  let output = Xmlm.make_output (`Fun sink) in
  Xmlm.output output dtd_signal;
  Xmlm.output output (`El_start (("", "plist"), []));
  let rec loop () =
    match source () with
    | `EOI -> ()
    | #token as token ->
      encode_token output token;
      loop ()
  in
  loop ();
  Xmlm.output output `El_end

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

let rec tokens sink = function
  | `Bool false -> sink `False
  | `Bool true -> sink `True
  | `Data data -> sink (`Data data)
  | `Date date -> sink (`Date date)
  | `Float float -> sink (`Real float)
  | `Int int -> sink (`Int int)
  | `String string -> sink (`String string)
  | `Array elts ->
    sink `Array_start;
    List.iter (tokens sink) elts;
    sink `Array_end
  | `Dict kvs ->
    sink `Dict_start;
    List.iter
      (fun (k, v) ->
        sink (`Key k);
        tokens sink v)
      kvs;
    sink `Dict_end

let print sink t =
  let output = Xmlm.make_output (`Fun sink) in
  Xmlm.output output dtd_signal;
  Xmlm.output output plist_start_signal;
  tokens (encode_token output) t;
  Xmlm.output output `El_end

let to_buffer buffer = print (Buffer.add_uint8 buffer)
let to_channel out_channel = print (output_byte out_channel)

let to_string t =
  let buffer = Buffer.create 512 in
  to_buffer buffer t;
  Buffer.contents buffer
