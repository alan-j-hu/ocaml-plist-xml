(** Plist values *)
type t =
  [ `Null
  | `Bool of bool
  | `Data of string
  | `Date of float * float option (** (timestamp, timezone) *)
  | `Float of float
  | `Int of int
  | `String of string
  | `A of t list (** Array *)
  | `O of (string * t) list (** Dictionary *)
  ]

exception Parse_error of string

module type STREAM = sig
  type eff
  (** The phantom type for the indexed effect ([Markup.sync] or
      [Markup.async]). *)

  type 'a io
  (** The wrapper type for the effect. *)

  val next : ('a, eff) Markup.stream -> 'a option io

  val peek : ('a, eff) Markup.stream -> 'a option io

  val parse_xml :
    ?report:(Markup.location -> Markup.Error.t -> unit io) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, eff) Markup.stream -> eff Markup.parser

  val bind : 'a io -> ('a -> 'b io) -> 'b io
  (** Monadic bind *)

  val return : 'a -> 'a io
  (** Monadic return *)
end

module Sync : STREAM with type eff = Markup.sync and type 'a io = 'a

module type S = sig
  type eff
  type 'a io

  val plist_of_stream_exn :
    (Markup.content_signal, eff) Markup.stream -> t io
  (** Raises [Parse_error] upon failure. *)

  val parse_plist_exn :
    ?report:(Markup.location -> Markup.Error.t -> unit io) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, eff) Markup.stream -> t io
  (** Raises [Parse_error] upon failure. See documentation for
      [Markup.parse_xml] for labeled parameter information. *)
end

module Make (S : STREAM) : S with type eff = S.eff and type 'a io = 'a S.io

include S with type eff = Markup.sync and type 'a io = 'a
