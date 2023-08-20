open Ast 

let car = function 
  | Pair (a, _) -> a 
  | _ as e -> failwith ("car error: " ^ Format.asprintf "%a" fmt e)

let cdr = function 
  | Pair (_, b) -> b 
  | _ as e -> failwith ("cdr error: " ^ Format.asprintf "%a" fmt e)

let eq e1 e2 = match e1, e2 with 
  | Pair _, _ | _, Pair _ | Closure _, _ | _, Closure _ -> F 
  | a, b when a = b -> T 
  | _ -> F

let is_atom = function 
  | Closure _ | Pair _ -> F 
  | _ -> T 

let rec unique = function 
  | x :: xs -> if List.mem x xs then false else unique xs 
  | [] -> true

let extract_names args = 
  let rec extract acc = function 
    | Nil -> List.rev acc 
    | Pair(Sym n, t) -> extract (n :: acc) t 
    | _ -> failwith ("expected string list, got " ^ Format.asprintf "%a" fmt args) 
  in
  let names = extract [] args in 
  if unique names then names else failwith "lhs must be unique"  

let rec eval env = function 
  | Num i -> Num i 
  | Sym s -> Env.lookup env s 
  | Pair (Quote, e) -> car e 
  | Pair (Car, e) -> car (eval env (car e))
  | Pair (Cdr, e) -> cdr (eval env (car e)) 
  | Pair (Cons, e) -> 
    let a = eval env (car e) in 
    let b = eval env (car (cdr e)) in 
    Pair (a, b) 
  | Pair (Add, e) -> arith env ( + ) e
  | Pair (Sub, e) -> arith env ( - ) e
  | Pair (Mul, e) -> arith env ( * ) e
  | Pair (Div, e) -> arith env ( / ) e
  | Pair (Rem, e) -> arith env Int.rem e
  | Pair (If, e) -> begin 
    let c = car e in 
    let t = car (cdr e) in 
    let e = car (cdr (cdr e)) in 
    match (eval env c) with 
    | T -> eval env t 
    | F -> eval env e 
    | _ as e -> failwith ("expected boolean, got " ^ to_string e)
  end
  | Pair (Atom, e) -> is_atom (eval env (car e))
  | Pair (Eq, e) -> 
    let e1 = car e in 
    let e2 = car (cdr e) in 
    eq (eval env e1) (eval env e2) 
  | Pair (Lambda, e) -> 
    let args = car e in 
    let body = car (cdr e) in 
    Closure (extract_names args, body, env) 
  | Pair (Let, e) -> 
    let body = car e in 
    let defs = cdr e in 
    let vars = vars defs in 
    let exprs = List.map (eval env) (exprs defs) in 
    let env' = List.fold_left2 Env.bind env vars exprs in 
    eval env' body
  | Pair (Letrec, e) -> 
    let body = car (e) in 
    let defs = cdr e in 
    let vars = vars defs in 
    let exprs = exprs defs in 
    let env' = List.fold_left Env.bind_empty env vars in 
    let values' = List.map (eval env') exprs in 
    let _ = List.iter (fun (n, v) -> Env.set env' n v) (List.combine vars values') in 
    eval env' body
  | Pair (e, params) -> 
    let func = eval env e in 
    let args = evlis env params in 
    eval_call func args
  | _ as e -> failwith ("Error: " ^ Format.asprintf "%a" fmt e)

and evlis env = function 
  | Nil -> [] 
  | Pair (a, b) -> eval env a :: evlis env b
  | _ as e -> failwith ("expected list of expressions, got " ^ to_string e)

and vars = function
  | Nil -> []
  | Pair (Pair(Sym s, _), b) -> 
    s :: (vars b)
  | _ as e -> failwith ("expected list of names, got " ^ to_string e) 

and exprs = function 
  | Nil -> [] 
  | Pair (Pair(_, e), b) -> 
    e :: (exprs b) 
  | _ as e -> failwith ("expected list of expressions, got " ^ to_string e) 

and arith env op e = 
  let e1 = eval env (car e) in 
  let e2 = eval env (car (cdr e)) in 
  match e1, e2 with 
  | Num n, Num m -> Num (op n m) 
  | _ -> failwith "expected two numbers" 

and eval_call func params = match func with 
  | Closure (args, body, env) -> 
      let env' = List.fold_left2 Env.bind env args params in 
      eval env' body 
  | _ -> failwith "expected closure" 