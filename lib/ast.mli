type t = Atom of string | Pair of t * t
val of_string : string -> t
val fmt : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]
