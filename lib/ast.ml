type t = 
  | Atom of atomic 
  | List of t list 

and atomic = 
  | Sym of string 
  | Num of int 
  
(*
and builtin = 
  | Quote 
  (* Arithmetic expressions *)
  | Add | Sub | Mul | Div | Rem 
  (* Relational expressions *)
  | Eq | Leq 
  (* Structural expressions *)
  | Car | Cdr | Cons | IsAtom
  (* Conditional form *)
  | If 
  (* Lambda expression *)
  | Lambda 
  (* Blocks *)
  | Let | Letrec 
  (* Boolean constants *)
  | T | F 
*)

(* Inspired by janestreet/sexplib0 *)

open Format 

let default_indent = ref 2 

let rec fmt_indent indent ppf = function 
  | Atom (Sym s) -> pp_print_string ppf s 
  | Atom (Num i) -> pp_print_int ppf i 
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
