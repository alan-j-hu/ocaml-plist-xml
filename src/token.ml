type token =
  [ `Array_start
  | `Array_end
  | `Data of string
  | `Date of float * float option
  | `Dict_start
  | `Dict_end
  | `False
  | `Int of int
  | `Key of string
  | `Real of float
  | `String of string
  | `True ]
(** A token of the logical syntax of a plist value. The logical syntax is an
    abstraction around an underlying format, such as XML.

    A token sequence is well-formed iff it parses to a plist nonterminal:
    {v
plist ::= `Array_start plist* `Array_end
        | `Dict_start kv* `Dict_end
        | `False
        | `Data
        | `Date
        | `Int
        | `Real
        | `String
        | `True

kv ::= `Key plist
    v} *)

type signal = [ token | `EOI ]
(** Furthermore, a signal is either a token or an [`EOI] marker. A well-formed
    signal sequence consists is a well-formed token sequence followed by an
    [`EOI] marker, and represents a plist document.
    {v
doc ::= plist `EOI
    v} *)
