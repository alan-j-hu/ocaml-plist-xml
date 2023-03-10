## 0.5.0 (March 10, 2023)

- Rename `of_channel` to `from_channel` and `of_string` to `from_string`. The
  new names reflect the naming convention across multiple popular OCaml file
  format libraries.

## 0.4.0 (Feb 21, 2023)

Version 0.4.0 is a major rewrite with an API redesign that is not
backwards-compatible. `plist-xml` now uses `xmlm` instead of `markup` for the
underlying XML codec. The package `plist-xml-lwt` no longer exists. In the
new API, `plist-xml` has both a high-level tree interface and a low-level
streaming interface.

## 0.3.0 (Oct 28, 2020)

- `plist_of_stream_exn` now takes a `(Markup.signal, s) Markup.stream` instead
  of a `(Markup.content_signal, s) Markup.stream`, as the
  `Markup.content_signal` type was removed from Markup.ml in version 1.0.0-1.

## 0.2.0 (Aug 11, 2020)

- `signals` now emits `xml` and `doctype` elements.
- `signals` now gives `plist` element the attribute `version="1.0"`.
- Add `?encoding` parameter to `signals` function.

## 0.1 (Aug 5, 2020)

Initial release.
