type t = 
  | Num of int 
  | Sym of string 
  | Quote of t 
  | Add of t * t 
  | Sub of t * t 
  | Mul of t * t 
  | Div of t * t 
  | Rem of t * t 
  | Eq of t * t 
  | Leq of t * t 
  | Car of t 
  | Cdr of t 
  | Cons of t * t 
  | Atom of t 
  | If of t * t * t 
  | Lambda of string list * t 
  | App of t * t list 
  | Let of t * (string * t) list 
  | Letrec of t * (string * t) list 
  | Nil 
  | True 
  | False 

val fmt : Format.formatter -> t -> unit 
[@@ocaml.toplevel_printer]
