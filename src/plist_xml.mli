type signal =
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

type decoder

val error_message : error -> string
val create_decoder : (unit -> int) -> (signal -> unit) -> decoder
val from_flow : ?buf_size:int -> #Eio.Flow.source -> decoder
val decode : decoder -> unit

type encoder

val create_encoder : (unit -> signal) -> (int -> unit) -> encoder
val encode : encoder -> signal -> unit
