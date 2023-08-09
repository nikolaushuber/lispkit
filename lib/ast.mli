type t = 
  | Atom of atomic 
  | List of t list 

and atomic = 
  | Sym of string 
  | Num of int 

val fmt : Format.formatter -> t -> unit 
[@@ocaml.toplevel_printer]
