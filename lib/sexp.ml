type t = 
  | Sym of string 
  | Num of int 
  | Pair of t ref * t 

let is_num s = 
  try 
    let _ = int_of_string s in 
    true 
  with 
    _ -> false 

let of_string s = 
  let sexpr = Parsexp.Single.parse_string_exn s in 
  let rec sexpr_to_ast : Sexplib0.Sexp.t -> t = function 
    | Atom s when is_num s -> Num (int_of_string s) 
    | Atom s -> Sym s
    | List (l :: t) -> Pair (ref (sexpr_to_ast l), sexpr_to_ast (List t))
    | List [] -> Sym "NIL" 
  in 
  sexpr_to_ast sexpr 

open Format 

let ident = 2  

let rec fmt ppf = function 
  | Sym s -> pp_print_string ppf s 
  | Num i -> pp_print_int ppf i 
  | Pair (a, Sym "NIL") -> 
      pp_open_box ppf ident; 
      pp_print_string ppf "("; 
      fmt ppf !a; 
      pp_print_string ppf ")"; 
      pp_close_box ppf ()
  | Pair (a, b)  -> 
      pp_open_box ppf ident;
      pp_print_string ppf "(";
      fmt ppf !a;
      fmt_rest ppf b

and fmt_rest ppf = function 
  | Pair (a, b) -> 
    pp_print_space ppf ();
    fmt ppf !a;
    fmt_rest ppf b
  | Sym "NIL" -> 
    pp_print_string ppf ")";
    pp_close_box ppf ()
  | _ as e -> 
      pp_print_string ppf " . "; 
      fmt ppf e; 
      pp_print_string ppf ")"; 
      pp_close_box ppf () 

let to_string = asprintf "%a" fmt 