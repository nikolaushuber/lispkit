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

(* Inspired by janestreet/sexplib0 *)
open Format 

let space_sep ppf () = pp_print_string ppf " " 

let rec fmt ppf = function 
  | Num i -> pp_print_int ppf i 
  | Sym s -> pp_print_string ppf s 
  | Quote t -> fprintf ppf "(quote %a)" fmt t  
  | Add (t1, t2) -> fmt_binop ppf "add" t1 t2 
  | Sub (t1, t2) -> fmt_binop ppf "sub" t1 t2 
  | Mul (t1, t2) -> fmt_binop ppf "mul" t1 t2 
  | Div (t1, t2) -> fmt_binop ppf "div" t1 t2 
  | Rem (t1, t2) -> fmt_binop ppf "rem" t1 t2 
  | Eq (t1, t2) -> fmt_binop ppf "eq" t1 t2 
  | Leq (t1, t2) -> fmt_binop ppf "leq" t1 t2 
  | Car t -> fprintf ppf "(car %a)" fmt t 
  | Cdr t -> fprintf ppf "(cdr %a)" fmt t 
  | Cons (t1, t2) -> fmt_binop ppf "cons" t1 t2 
  | Atom t -> fprintf ppf "(atom %a)" fmt t 
  | If (c, t, e) -> fprintf ppf "(if %a %a %a)" fmt c fmt t fmt e 
  | Lambda (args, t) -> 
      fprintf ppf "(lambda (%a) %a)" 
      (pp_print_list ~pp_sep:space_sep pp_print_string) args 
      fmt t  
  | App (t, tl) -> 
      fprintf ppf "(%a %a)" fmt t 
      (pp_print_list ~pp_sep:space_sep fmt) tl 
  | Let (t, nl) -> 
      fprintf ppf "(%a %a)" fmt t fmt_namelist nl 
  | Letrec (t, nl) -> 
      fprintf ppf "(%a %a)" fmt t fmt_namelist nl 
  | Nil -> pp_print_string ppf "nil" 
  | True -> pp_print_string ppf "T" 
  | False -> pp_print_string ppf "F"   

and fmt_binop ppf op e1 e2 = 
  pp_print_string ppf ("(" ^ op ); 
  fprintf ppf (" %a %a)") fmt e1 fmt e2 

and fmt_namelist ppf = 
  List.iter (fun (name, t) -> fprintf ppf "(%a %a)" pp_print_string name fmt t) 

(*
let rec fmt_indent indent ppf = function 
  | Atom (Sym s) -> pp_print_string ppf s 
  | Atom (Num i) -> pp_print_int ppf i 
  | Atom Quote -> pp_print_string ppf "quote" 
  | Atom Car -> pp_print_string ppf "car" 
  | Atom Cdr -> pp_print_string ppf "cdr" 
  | Atom IsAtom -> pp_print_string ppf "atom"  
  | List (h :: t) ->  
      pp_open_box ppf indent; 
      pp_print_string ppf "("; 
      fmt_indent indent ppf h; 
      fmt_rest indent ppf t
  | List [] -> pp_print_string ppf "()" 

and fmt_rest indent ppf = function 
  | h :: t -> 
    pp_print_space ppf (); 
    fmt_indent indent ppf h; 
    fmt_rest indent ppf t 
  | [] -> 
    pp_print_string ppf ")"; 
    pp_close_box ppf () 

let fmt = fmt_indent !default_indent
*)
