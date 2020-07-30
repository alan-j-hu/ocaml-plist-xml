module Plist = Plist_xml.Make(Plist_xml.Sync)

let () =
  prerr_endline "Test pass...";
  let handle = Unix.opendir "pass" in
  Fun.protect (fun () ->
      let rec loop () =
        let str = Unix.readdir handle in
        if str <> Filename.parent_dir_name
           && str <> Filename.current_dir_name then (
          prerr_endline ("Testing " ^ str);
          let chan = open_in ("pass/" ^ str) in
          let stream = Markup.channel chan in
          ignore (Plist.plist_of_xml stream)
        );
        loop ()
      in try loop () with End_of_file -> ()
    ) ~finally:(fun () -> Unix.closedir handle)
