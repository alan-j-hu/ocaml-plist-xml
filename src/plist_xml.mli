type t =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option (** (timestamp, timezone) *)
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of t list (** Array *)
  | `Dict of (string * t) list (** Dictionary *)
  ]
(** Plist values. *)

val signals : t -> (Markup.signal, Markup.sync) Markup.stream
(** Convert a plist into XML signals. *)

exception Parse_error of string

module type IO = sig
  type s
  (** The phantom type for the effect ([Markup.sync] or [Markup.async]). *)

  type _ io
  (** The effect type. *)

  val next : ('a, s) Markup.stream -> 'a option io

  val peek : ('a, s) Markup.stream -> 'a option io

  val parse_xml :
    ?report:(Markup.location -> Markup.Error.t -> unit io) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, s) Markup.stream -> s Markup.parser

  val bind : 'a io -> ('a -> 'b io) -> 'b io
  (** Monadic bind. *)

  val return : 'a -> 'a io
  (** Monadic return. *)
end
(** The module type of synchronous or asynchronous I/O. *)

module IO : IO with type s = Markup.sync and type 'a io = 'a
(** Synchronous I/O under the identity monad. *)

module type S = sig
  type s
  type _ io

  val plist_of_stream_exn : (Markup.content_signal, s) Markup.stream -> t io
  (** Raises [Parse_error] upon failure. *)

  val parse_exn :
    ?report:(Markup.location -> Markup.Error.t -> unit io) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, s) Markup.stream -> t io
  (** Raises [Parse_error] upon failure. See documentation for
      [Markup.parse_xml] for labeled parameter information. *)
end

module Make (IO : IO) : S with type s = IO.s and type 'a io = 'a IO.io

include S with type s = Markup.sync and type 'a io = 'a