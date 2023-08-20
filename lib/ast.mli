type t = 
  | Sym of string 
  | Num of int 
  | T | F 
  | Car | Cdr | Cons 
  | Eq 
  | Atom 
  | Add | Sub | Mul | Div | Rem 
  | If 
  | Lambda  
  | Let | Letrec 
  | Quote 
  | L of t list 
  | Closure of string list * t * t Env.t  

val of_string : string -> t

val fmt : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]
