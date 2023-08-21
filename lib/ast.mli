type t = 
  | Sym of string 
  | Num of int 
  | Pair of t * t 
  | Closure of string list * t * t Env.t 

val of_string : string -> t

val fmt : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]

val to_string : t -> string 