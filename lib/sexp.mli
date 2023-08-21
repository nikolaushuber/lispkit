type t = 
  | Sym of string 
  | Num of int 
  | Pair of t ref * t 

val of_string : string -> t

val fmt : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]

val to_string : t -> string 