module type EFF = sig
  include Plist_xml.S

  val opendir : string -> Unix.dir_handle io
  val readdir : Unix.dir_handle -> string io
  val closedir : Unix.dir_handle -> unit io

  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io

  val with_stream :
    string -> ((char, eff) Markup.stream -> 'a io) -> 'a io

  val catch : (unit -> 'a io) -> (exn -> 'a io) -> 'a io

  val protect : finally:(unit -> unit io) -> (unit -> 'a io) -> 'a io
end

module Make (Eff : EFF) = struct
  let ( let* ) = Eff.bind

  let test_pass () =
    let* handle = Eff.opendir "pass" in
    Eff.protect (fun () ->
        let rec loop () =
          let* str = Eff.readdir handle in
          let* _ =
            if str <> Filename.parent_dir_name
               && str <> Filename.current_dir_name then (
              prerr_endline ("Testing " ^ str);
              let* _ = Eff.with_stream ("pass/" ^ str) Eff.parse_plist_exn in
              Eff.return ()
            ) else
              Eff.return ()
          in loop ()
        in
        Eff.catch loop (function
            | End_of_file -> Eff.return ()
            | exn -> raise exn
          )
      ) ~finally:(fun () -> Eff.closedir handle)

  let test_fail () =
    let* handle = Eff.opendir "fail" in
    Eff.protect (fun () ->
        let rec loop () =
          let* str = Eff.readdir handle in
          let* () =
            if str <> Filename.parent_dir_name
               && str <> Filename.current_dir_name then (
              prerr_endline ("Testing " ^ str);
              Eff.catch
                (fun () ->
                  let* _ =
                    Eff.with_stream ("fail/" ^ str) Eff.parse_plist_exn
                  in failwith ("Test " ^ str ^ " parsed"))
                (function
                 | Plist_xml.Parse_error _ -> Eff.return ()
                 | exn -> raise exn)
            ) else (
              Eff.return ()
            )
          in loop ()
        in
        Eff.catch loop (function
            | End_of_file -> Eff.return ()
            | exn -> raise exn
          )
      ) ~finally:(fun () -> Eff.closedir handle)
end
