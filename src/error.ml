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
(** [Error ((line, col), msg)] indicates a syntax error at line number
    [line] and column number [col]. The line and column numbers start from
    [1]. *)
