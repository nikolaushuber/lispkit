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
  | Nil 
  | Pair of t * t 
  | Closure of string list * t * t Env.t 

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
    | Atom "QUOTE" -> Quote 
    | Atom "ADD" -> Add 
    | Atom "SUB" -> Sub 
    | Atom "MUL" -> Mul 
    | Atom "DIV" -> Div 
    | Atom "REM" -> Rem 
    | Atom "CAR" -> Car 
    | Atom "CDR" -> Cdr 
    | Atom "ATOM" -> Atom 
    | Atom "CONS" -> Cons 
    | Atom "EQ" -> Eq 
    | Atom "IF" -> If 
    | Atom "LAMBDA" -> Lambda 
    | Atom "LET" -> Let 
    | Atom "LETREC" -> Letrec 
    | Atom "NIL" -> Nil 
    | Atom "T" -> T 
    | Atom "F" -> F
    | Atom s -> Sym s
    | List (l :: t) -> Pair (sexpr_to_ast l, sexpr_to_ast (List t))
    | List [] -> Nil 
  in 
  sexpr_to_ast sexpr 

open Format 

let ident = 2  

let rec fmt ppf = function 
  | Sym s -> pp_print_string ppf s 
  | Num i -> pp_print_int ppf i 
  | T -> pp_print_string ppf "T" 
  | F -> pp_print_string ppf "F"
  | Car -> pp_print_string ppf "CAR"
  | Cdr -> pp_print_string ppf "CDR" 
  | Cons -> pp_print_string ppf "CONS"
  | Eq -> pp_print_string ppf "EQ"
  | Atom -> pp_print_string ppf "ATOM"
  | Add -> pp_print_string ppf "ADD"
  | Sub -> pp_print_string ppf "SUB"
  | Mul -> pp_print_string ppf "MUL"
  | Div -> pp_print_string ppf "DIV"
  | Rem -> pp_print_string ppf "REM"
  | If -> pp_print_string ppf "IF" 
  | Lambda -> pp_print_string ppf "LAMBDA"
  | Let -> pp_print_string ppf "LET"
  | Letrec -> pp_print_string ppf "LETREC"
  | Quote -> pp_print_string ppf "QUOTE"
  | Pair (a, Nil) -> 
      pp_open_box ppf ident; 
      pp_print_string ppf "("; 
      fmt ppf a; 
      pp_print_string ppf ")"; 
      pp_close_box ppf ()
  | Pair (a, b)  -> 
      pp_open_box ppf ident;
      pp_print_string ppf "(";
      fmt ppf a;
      fmt_rest ppf b
  | Nil -> pp_print_string ppf "NIL"
  | Closure _ -> pp_print_string ppf "#fun"

and fmt_rest ppf = function 
  | Pair (a, b) -> 
    pp_print_space ppf ();
    fmt ppf a;
    fmt_rest ppf b
  | Nil -> 
    pp_print_string ppf ")";
    pp_close_box ppf ()
  | _ as e -> fmt ppf e

let to_string = asprintf "%a" fmt 