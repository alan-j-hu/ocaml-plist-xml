include module type of Common

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

val error_message : error -> string
val of_channel : in_channel -> t
val of_string : string -> t
val parse : (unit -> int) -> t

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

val decode : (unit -> int) -> (signal -> unit) -> unit
val encode : (unit -> signal) -> (int -> unit) -> unit
