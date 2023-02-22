let read filename =
  In_channel.with_open_text filename (fun in_channel ->
      Plist_xml.of_channel in_channel)

let test_pass () =
  let files = Sys.readdir "pass" in
  Array.sort compare files;
  Array.iter (fun file -> ignore (read (Filename.concat "pass" file))) files

let test_fail () =
  let files = Sys.readdir "fail" in
  Array.sort compare files;
  Array.iter
    (fun file ->
      try ignore (read (Filename.concat "fail" file)) with
      | Plist_xml.Error ((_, _), _) -> ()
      | Xmlm.Error ((_, _), _) -> ())
    files

let () =
  test_pass ();
  test_fail ();
  assert (
    read "pass/array.plist" = `Dict [ ("mybools", `Array [ `Bool false ]) ]);
  assert (
    read "pass/base64.plist"
    = `Dict
        [
          ( "data",
            `Data
              "Four score and seven years ago our fathers brought forth on \
               this continent, a new nation, conceived in Liberty, and \
               dedicated to the proposition that all men are created equal.\n"
          );
        ]);
  assert (
    read "pass/bool_dict.plist"
    = `Dict [ ("true", `Bool true); ("false", `Bool false) ]);
  assert (read "pass/emptydict.plist" = `Dict []);
  assert (read "pass/emptykey.plist" = `Dict [ ("", `Bool true) ]);
  assert (read "pass/emptystring.plist" = `String "");
  assert (read "pass/false.plist" = `Bool false)
