module Async : Plist_xml.IO
with type s = Markup.async and type 'a io = 'a Lwt.t
(** Asynchronous I/O with Lwt. *)

include Plist_xml.S with type s = Markup.async and type 'a io = 'a Lwt.t
