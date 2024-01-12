(* LISPKIT COMPILER *)
open Lispkit 

let eval = ref false 

let usage_msg = "lispkit <input>" 

let input_file : string option ref = ref None 

let anon_fun name = input_file := Some name 

let arglist = [
  "-e", Arg.Set eval, "Evaluate LISP program";  
]

let () = 
  Arg.parse arglist anon_fun usage_msg; 
  let in_file = match !input_file with 
    | Some f -> f 
    | None -> exit 0 
  in 
  let ic = open_in in_file in 
  let in_str = really_input_string ic (in_channel_length ic) in 
  if !eval then 
    Eval.eval (Sexp.of_string in_str) (Sexp.Sym "NIL") (Sexp.Sym "NIL") 
    |> Sexp.fmt Format.std_formatter
  else
    Compiler.compile in_str |> Sexp.fmt Format.std_formatter
  ; 
  Format.pp_print_flush Format.std_formatter (); 
  print_endline "" 
