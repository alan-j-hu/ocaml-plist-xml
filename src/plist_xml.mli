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
  (** Monadic bind *)

  val return : 'a -> 'a m
  (** Monadic return *)
end

module Sync : STREAM
       with type 'a stream = ('a, Markup.sync) Markup.stream
        and type 'a m = 'a
        and type parser = Markup.sync Markup.parser

module Make(S : STREAM) : sig
  val plist_of_stream_exn : Markup.content_signal S.stream -> t S.m
  (** Raises [Parse_error] upon failure. *)

  val plist_of_xml_exn :
    ?report:(Markup.location -> Markup.Error.t -> unit) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    char S.stream -> t S.m
  (** Raises [Parse_error] upon failure. See documentation for
      [Markup.parse_xml] for labeled parameter information. *)
end
