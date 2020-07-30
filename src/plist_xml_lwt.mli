module Async : Plist_xml.STREAM
with type eff = Markup.async and type 'a io = 'a Lwt.t

include Plist_xml.S with type eff = Markup.async and type 'a io = 'a Lwt.t
