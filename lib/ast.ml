type t = 
  | Atom of string 
  | Pair of t * t 

let of_string s = 
  let sexpr = Parsexp.Single.parse_string_exn s in 
  let rec sexpr_to_ast : Sexplib0.Sexp.t -> t = function 
    | Atom s -> Atom s 
    | List (x :: xs) -> Pair(sexpr_to_ast x, sexpr_to_ast (List xs)) 
    | List [] -> Atom "NIL" 
  in 
  sexpr_to_ast sexpr 

open Format 

let rec fmt ppf = function 
  | Atom s -> pp_print_string ppf s 
  | Pair (a, b) -> fprintf ppf "(%a.%a)" fmt a fmt b 