(lang dune 2.7)
(using menhir 2.0)
(name plist-xml)
(version 0.3.0)
(documentation https://alan-j-hu.github.io/ocaml-plist-xml/)
(generate_opam_files true)

(license MIT)
(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(source (github alan-j-hu/ocaml-plist-xml))

(package
 (name plist-xml)
 (synopsis
  "Reading and writing of plist files in the XML format in pure OCaml")
 (description
  "Reading and writing of plist files in the XML format in pure OCaml.

Implementation of https://www.apple.com/DTDs/PropertyList-1.0.dtd.")
 (depends
  (base64 (>= 3.4))
  (cstruct (>= 6.1))
  (ISO8601 (>= 0.2.6))
  (menhir (>= 20220210))
  (xmlm (>= 1.4))
  (eio (and (>= 0.7) :with-test))
  (eio_main (and (>= 0.7) :with-test))))
