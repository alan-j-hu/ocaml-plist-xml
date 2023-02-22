type t =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option (** (timestamp, timezone) *)
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of t list (** Array *)
  | `Dict of (string * t) list (** Dictionary *) ]
(** Plist values. *)
