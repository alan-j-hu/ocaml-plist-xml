module Async : Plist_xml.STREAM
       with type eff = Markup.async and type 'a io = 'a Lwt.t = struct
  type eff = Markup.async
  type 'a io = 'a Lwt.t
  let next = Markup_lwt.next
  let peek = Markup_lwt.peek
  let parse_xml ?report ?encoding ?namespace ?entity ?context =
    Markup_lwt.parse_xml ?report ?encoding ?namespace ?entity ?context
  let bind = Lwt.bind
  let return = Lwt.return
end

include Plist_xml.Make(Async)
