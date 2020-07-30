type value =
  [ `Null
  | `Bool of bool
  | `Data of string
  | `Date of string
  | `Float of float
  | `Int of int
  | `String of string
  | `A of value list
  | `O of (string * value) list ]

type t = (string * value) list

exception Parse_error of string

let end_of_doc () = raise (Parse_error "End of document")

let expected_closing () = raise (Parse_error "Expected closing tag")

module type STREAM = sig
  type 'a stream
  type 'a m
  type parser
  val next : 'a stream -> 'a option m
  val peek : 'a stream -> 'a option m
  val parse_xml :
    ?report:(Markup.location -> Markup.Error.t -> unit) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    char stream -> parser
  val signals : parser -> Markup.signal stream
  val content : Markup.signal stream -> Markup.content_signal stream
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
end

module Sync : STREAM
       with type 'a stream = ('a, Markup.sync) Markup.stream
        and type 'a m = 'a
        and type parser = Markup.sync Markup.parser
  = struct
  type 'a stream = ('a, Markup.sync) Markup.stream
  type 'a m = 'a
  type parser = Markup.sync Markup.parser
  let next = Markup.next
  let peek = Markup.peek
  let parse_xml ?report ?encoding ?namespace ?entity ?context s =
    Markup.parse_xml ?report ?encoding ?namespace ?entity ?context s
  let signals = Markup.signals
  let content = Markup.content
  let bind x f = f x
  let return x = x
end

let isn't_whitespace ch =
  ch <> ' ' && ch <> '\t' && ch <> '\r' && ch <> '\n'

let check_whitespace =
  String.iter (fun ch ->
      if isn't_whitespace ch then
        raise (Parse_error ("Unexpected character " ^ (String.make 1 ch)))
    )

module Make (S : STREAM) = struct
  let ( let* ) = S.bind

  type start_or_end =
    [ `End_element
    | `Start_element of Markup.name * (Markup.name * string) list ]

  let rec skip_whitespace stream =
    let* next = S.next stream in
    match next with
    | Some (`Text strs) ->
       List.iter check_whitespace strs;
       skip_whitespace stream
    | Some (#start_or_end as v) -> S.return (Some v)
    | None -> S.return None

  let rec peek_whitespace stream =
    let* peeked = S.peek stream in
    match peeked with
    | Some (`Text strs) ->
       List.iter check_whitespace strs;
       ignore (S.next stream);
       peek_whitespace stream
    | Some (#start_or_end as v) -> S.return (Some v)
    | None -> S.return None

  let decode_base64 strs =
    let module M = Base64_rfc2045 in
    let decoder = M.decoder `Manual in
    let outbuf = Buffer.create 10 in
    let rec loop strs = function
      | `Await ->
         begin match strs with
         | str :: strs ->
            let buf = Buffer.create (String.length str) in
            String.iter (fun ch ->
                if isn't_whitespace ch then
                  Buffer.add_char buf ch
              ) str;
            M.src decoder (Buffer.to_bytes buf) 0 (Buffer.length buf);
            loop strs (M.decode decoder)
         | [] ->
            M.src decoder (Bytes.create 0) 0 0;
            loop [] (M.decode decoder)
         end
      | `End -> Buffer.contents outbuf
      | `Flush str ->
         Buffer.add_string outbuf str;
         loop strs (M.decode decoder)
      | `Malformed str ->
         raise (Parse_error ("Malformed base64: " ^ str))
      | `Wrong_padding ->
         raise (Parse_error ("Base64 wrong padding"))
    in loop strs (M.decode decoder)

  let rec parse_array acc stream =
    let* peeked = peek_whitespace stream in
    match peeked with
    | Some `End_element ->
       (* Munch end element *)
       let* _ = skip_whitespace stream in
       S.return (List.rev acc)
    | _ ->
       let* v = parse_val stream in
       parse_array (v :: acc) stream

  and parse_dict acc stream =
    let* next = skip_whitespace stream in
    match next with
    | Some `End_element -> S.return (List.rev acc)
    | Some (`Start_element((_, "key"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some (`Text [key]) ->
          let* next = S.next stream in
          begin match next with
          | Some `End_element ->
             let* value = parse_val stream in
             parse_dict ((key, value) :: acc) stream
          | _ -> expected_closing ()
          end
       | Some (`Text (_ :: _)) ->
          raise (Parse_error "Key exceeds max string length")
       (* Empty key *)
       | Some `End_element ->
          let* value = parse_val stream in
          parse_dict (("", value) :: acc) stream
       | Some _ -> raise (Parse_error "Expected text inside key")
       | None -> end_of_doc ()
       end
    | Some (`Start_element((_, s), _)) ->
       raise (Parse_error ("Expected key, got " ^ s))
    | None -> end_of_doc ()

  and parse_val stream =
    let* next = skip_whitespace stream in
    match next with
    | Some (`Start_element((_, "array"), _)) ->
       let* arr = parse_array [] stream in
       S.return (`A arr)
    | Some (`Start_element((_, "data"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some (`Text strs) -> S.return (`Data (decode_base64 strs))
       | _ -> raise (Parse_error "Expected base64-encoded data")
       end
    | Some (`Start_element((_, "date"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = S.next stream in
          begin match next with
          | Some `End_element -> S.return (`Date str)
          | _ -> expected_closing ()
          end
       | _ -> raise (Parse_error "Expected date")
       end
    | Some (`Start_element((_, "dict"), _)) ->
       let* dict = parse_dict [] stream in
       S.return (`O dict)
    | Some (`Start_element((_, "false"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some `End_element -> S.return (`Bool true)
       | _ -> expected_closing ()
       end
    | Some (`Start_element((_, "integer"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = S.next stream in
          begin match next with
          | Some `End_element -> S.return (`Int (int_of_string str))
          | _ -> expected_closing ()
          end
       | _ -> raise (Parse_error "Expected int")
       end
    | Some (`Start_element((_, "real"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = S.next stream in
          begin match next with
          | Some `End_element -> S.return (`Float (Float.of_string str))
          | _ -> expected_closing ()
          end
       | _ -> raise (Parse_error "Expected float")
       end
    | Some (`Start_element((_, "string"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = S.next stream in
          begin match next with
          | Some `End_element -> S.return (`String str)
          | _ -> expected_closing ()
          end
       (* Empty string *)
       | Some `End_element -> S.return (`String "")
       | _ -> raise (Parse_error "Expected text inside string")
       end
    | Some (`Start_element((_, "true"), _)) ->
       let* next = S.next stream in
       begin match next with
       | Some `End_element -> S.return (`Bool true)
       | _ -> expected_closing ()
       end
    | Some (`Start_element((_, start), _)) ->
       raise (Parse_error ("Got unknown element " ^ start))
    | Some `End_element -> raise (Parse_error "Got end element")
    | None -> end_of_doc ()

  let parse_dict stream =
    let* next = skip_whitespace stream in
    match next with
    | Some (`Start_element((_, "dict"), _)) -> parse_dict [] stream
    | _ -> raise (Parse_error "Expected dict")

  let plist_of_stream stream =
    let* next = skip_whitespace stream in
    match next with
    | Some (`Start_element((_, "plist"), _)) ->
       let* ret = parse_dict stream in
       let* next = skip_whitespace stream in
       begin match next with
       | Some `End_element -> S.return ret
       | _ -> expected_closing ()
       end
    | _ -> raise (Parse_error "Expected opening plist")

  let plist_of_xml ?report ?encoding ?namespace ?entity ?context s =
    S.parse_xml ?report ?encoding ?namespace ?entity ?context s
    |> S.signals
    |> S.content
    |> plist_of_stream
end
