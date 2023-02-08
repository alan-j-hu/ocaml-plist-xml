let ( / ) = Eio.Path.( / )

let run_tests subdir f =
  Eio_main.run @@ fun env ->
  let dir = Eio.Stdenv.cwd env / subdir in
  Eio.Path.with_open_dir dir @@ fun _ ->
  List.iter
    (fun item ->
      Eio.Std.traceln "%s" item;
      let path = dir / item in
      Eio.Path.with_open_in path f)
    (Eio.Path.read_dir dir)

let () =
  try
    run_tests "pass" @@ fun flow ->
    let decoder = Plist_xml.from_flow flow in
    ignore (Plist_xml.decode decoder)
  with
  | Plist_xml.Error ((line, col), e) ->
    Printf.eprintf "(%n, %n): %s" line col (Plist_xml.error_message e)
  | Xmlm.Error ((line, col), e) ->
    Printf.eprintf "(%n, %n): %s" line col (Xmlm.error_message e)

let () =
  run_tests "fail" @@ fun flow ->
  let decoder = Plist_xml.from_flow flow in
  try ignore (Plist_xml.decode decoder) with
  | Plist_xml.Error ((_, _), _) -> ()
  | Xmlm.Error ((_, _), _) -> ()
