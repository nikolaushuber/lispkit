open Lispkit 

let input = ref [] 
let file : string option ref = ref None 

let anon_fun inp = match !file with 
  | None -> file := Some inp 
  | Some _ -> input := inp :: !input 

let usage_msg = "secd <program> <argument>"

let () = 
  Arg.parse [] anon_fun usage_msg; 
  let in_file = match !file with 
    | Some file -> file 
    | None -> exit 0 
  in 
  let stack = Sexp.of_string (!input |> List.rev |> String.concat " ") in 
  let ic = open_in in_file in 
  let prog_str = really_input_string ic (in_channel_length ic) in 
  let prog = Sexp.of_string prog_str in 
  let nil = Sexp.Sym "NIL" in 
  Secd.exec stack nil prog nil |> Sexp.fmt Format.std_formatter; 
  Format.pp_print_newline Format.std_formatter (); 
