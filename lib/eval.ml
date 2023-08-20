open Ast 

let car = function 
  | L (a :: _ ) -> a 
  | _ as e -> failwith ("car error: " ^ Format.asprintf "%a" fmt e)

let cdr = function 
  | L (_ :: b) -> L b 
  | _ as e -> failwith ("cdr error: " ^ Format.asprintf "%a" fmt e)

let eq a b = match a, b with 
  | L [], L [] -> T 
  | L _, _ | _, L _ | Closure _, _ | _, Closure _ -> F 
  | a, b when a = b -> T 
  | _ -> F

let is_atom = function 
  | Closure _ | L _ -> F 
  | _ -> T 

let rec unique = function 
  | x :: xs -> if List.mem x xs then false else unique xs 
  | [] -> true

let extract_names args = 
  let rec extract acc = function 
    | L [] -> List.rev acc 
    | L (Sym n :: t) -> extract (n :: acc) (L t) 
    | _ as e -> failwith ("expected string list, got " ^ Format.asprintf "%a" fmt e) 
  in
  let names = extract [] args in 
  if unique names then names else failwith "lhs must be unique"  

let unzip defs = 
  let distr = function 
    | L (Sym n :: e) -> n, L e
    | _ -> failwith "unzip error" 
  in 
  let list = List.map distr defs in 
  let names, _ = List.split list in 
  if unique names then list else failwith "lhs must be unique" 

let rec eval env = function 
  | Num i -> Num i 
  | Sym s -> Env.lookup env s 
  | L [Quote; e] -> e 
  | L [Car; e] -> car (eval env e) 
  | L [Cdr; e] -> cdr (eval env e) 
  | L [Cons; a; b] -> L ((eval env a) :: [(eval env b)])
  | L [Add; a; b] -> arith ( + ) (eval env a) (eval env b) 
  | L [Sub; a; b] -> arith ( - ) (eval env a) (eval env b) 
  | L [Mul; a; b] -> arith ( * ) (eval env a) (eval env b) 
  | L [Div; a; b] -> arith ( / ) (eval env a) (eval env b) 
  | L [Rem; a; b] -> arith Int.rem (eval env a) (eval env b) 
  | L [If; c; t; e] -> begin 
    match eval env c with 
    | T -> eval env t 
    | F -> eval env e 
    | _ -> failwith "expected boolean" 
  end
  | L [Atom; e] -> is_atom e 
  | L [Eq; a; b] -> eq (eval env a) (eval env b) 
  | L [Lambda; args; e] -> Closure (extract_names args, e, env) 
  | L (Let :: e :: defs) -> 
    let env' = List.fold_left evbind env defs in 
    eval env' e 
  | L (Letrec :: e :: defs) -> 
    let def_list = unzip defs in 
    let names, values = List.split def_list in
    let env' = List.fold_left Env.bind_empty env names in 
    let values' = List.map (eval env') values in 
    let _ = List.iter (fun (n, v) -> Env.set env' n v) (List.combine names values') in 
    eval env' e 
  | L (e :: el) -> eval_call (eval env e) (List.map (eval env) el) 
  | _ as e -> failwith ("Error: " ^ Format.asprintf "%a" fmt e)

and evbind env = function 
  | L (Sym n :: e) -> Env.bind env n (eval env (L e)) 
  | _ -> failwith "expected named definition" 

and arith op e1 e2 = match e1, e2 with 
  | Num n, Num m -> Num (op n m) 
  | _ -> failwith "expected two numbers" 

and eval_call func params = match func with 
  | Closure (args, body, env) -> 
      let env' = List.fold_left2 Env.bind env args params in 
      eval env' body 
  | _ -> failwith "expected closure" 