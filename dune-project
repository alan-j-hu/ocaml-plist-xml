(lang dune 2.6)
(name plist-xml)
(version 0.1)
(generate_opam_files true)

(license MIT)
(authors "Alan Hu <hu.ala@northeastern.edu>")
(maintainers "Alan Hu <hu.ala@northeastern.edu>")
(source (github dosaylazy/ocaml-plist-xml))

(package
  (name plist-xml)
  (synopsis "Parsing of plist files in the XML format in pure OCaml")
  (description "Parsing of plist files in the XML format in pure OCaml")
  (depends
    (base64 (and (>= 3.4) (< 4.0)))
    (ISO (and (>= 0.2.6) (< 1.0)))
    (markup (and (>= 0.8) (< 0.9)))
    (ocaml (>= 4.08))))
