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
    let stream = Eio.Stream.create Int.max_int in
    (let in_buf = Eio.Buf_read.of_flow ~max_size:256 flow in
     Plist_xml.decode
       (fun () -> Eio.Buf_read.uint8 in_buf)
       (Eio.Stream.add stream));
    let buffer = Buffer.create 256 in
    let sink = Eio.Flow.buffer_sink buffer in
    ( Eio.Buf_write.with_flow sink @@ fun out_buf ->
      Plist_xml.encode
        (fun () -> Eio.Stream.take stream)
        (Eio.Buf_write.uint8 out_buf) );
    Eio.Std.traceln "%s" (Buffer.contents buffer)
  with
  | Plist_xml.Error ((line, col), e) ->
    Printf.eprintf "(%n, %n): %s" line col (Plist_xml.error_message e)
  | Xmlm.Error ((line, col), e) ->
    Printf.eprintf "(%n, %n): %s" line col (Xmlm.error_message e)

let () =
  run_tests "fail" @@ fun flow ->
  try
    let stream = Eio.Stream.create Int.max_int in
    (let in_buf = Eio.Buf_read.of_flow ~max_size:256 flow in
     Plist_xml.decode
       (fun () -> Eio.Buf_read.uint8 in_buf)
       (Eio.Stream.add stream));
    let buffer = Buffer.create 256 in
    let sink = Eio.Flow.buffer_sink buffer in
    ( Eio.Buf_write.with_flow sink @@ fun out_buf ->
      Plist_xml.encode
        (fun () -> Eio.Stream.take stream)
        (Eio.Buf_write.uint8 out_buf) );
    Eio.Std.traceln "%s" (Buffer.contents buffer)
  with
  | Plist_xml.Error ((_, _), _) -> ()
  | Xmlm.Error ((_, _), _) -> ()
