let () =
  assert (
    Plist_xml.of_string
      {|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
       <dict>
       </dict>
</plist>
|}
    = `Dict []);
  assert (
    Plist_xml.of_string
      {|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><true/></plist>
|}
    = `Bool true)

let () =
  try
    ignore
      (Plist_xml.of_string
         {|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><cool/></plist>
|});
    assert false
  with Plist_xml.Error _ -> ()

let () =
  try
    ignore
      (Plist_xml.of_string
         {|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><true></plist>
|});
    assert false
  with Xmlm.Error _ -> ()

let () = Plist_xml.to_channel stdout (`Dict [])
