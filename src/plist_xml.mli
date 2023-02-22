include module type of Error

val error_message : error -> string

(** {1 Simple Interface}

    The simple interface provides functions for reading plist documents into
    a tree data structure. *)

include module type of Plist_tree

val parse : (unit -> int) -> t
(** [parse source] reads an XML representation of a plist value by repeatedly
    calling [source], which supplies the next byte of input. The input should
    include the XML header.

    @raise Error For errors pertaining to the plist format.
    @raise Xmlm.Error For underlying XML errors. *)

val of_channel : in_channel -> t
(** [of_channel in_channel] reads a plist from [in_channel]. See
    {!val:parse}.

    @raise Error
    @raise Xmlm.Error *)

val of_string : string -> t
(** [of_string string] reads a plist from [string]. See {!val:parse}.

    @raise Error
    @raise Xmlm.Error *)

val print : (int -> unit) -> t -> unit
(** [print sink t] outputs an XML representation of plist value [t] by
    repeatedly calling [sink] with each byte of output. The output includes the
    XML header and DTD. *)

val to_buffer : Buffer.t -> t -> unit
(** [to_buffer buffer t] outputs [t] to [buffer]. See {!val:print}. *)

val to_channel : out_channel -> t -> unit
(** [to_channel out_channel t] outputs [t] to [out_channel]. See
    {!val:print}. *)

val to_string : t -> string
(** [to_string t] returns the string representation of [t]. See
    {!val:print}. *)

(** {1 Streaming Interface}

    The streaming interface provides lower-level signal parsing and printing
    functions, which do not build up a tree in memory. *)

include module type of Token

val decode : (unit -> int) -> (signal -> unit) -> unit
(** [decode source sink] reads bytes of input by repeatedly calling
    [source] and calls [sink] upon reading enough to determine the next
    signal. The emitted signal sequence is guaranteed to be a well-formed
    document.

    @raise Error For errors pertaining to the plist format.
    @raise Xmlm.Error For underlying XML errors. *)

val encode : (unit -> signal) -> (int -> unit) -> unit
(** [encode source sink] reads signals by repeatedly calling [source] and
    outputs their XML representation to [sink] byte-by-byte. *)

val tokens : (token -> unit) -> t -> unit
(** [tokens sink t] repeatedly calls [sink] with the tokens that
    correspond with [t]. *)
