open Ast 

let car = function 
  | Pair (a, _) -> a 
  | _ as e -> failwith ("car error: " ^ to_string e)

let cdr = function 
  | Pair (_, b) -> b 
  | _ as e -> failwith ("cdr error: " ^ to_string e)

let eq e1 e2 = match e1, e2 with 
  | Num n, Num m -> if n = m then Sym "T" else Sym "F" 
  | Sym s, Sym t -> if String.equal s t then Sym "T" else Sym "F" 
  | _ -> Sym "F"

let leq e1 e2 = match e1, e2 with 
  | Num n, Num m -> if n <= m then Sym "T" else Sym "F" 
  | _ -> failwith ("leq expects to numbers, got " ^ to_string e1 ^ " , " ^ to_string e2)

let is_atom = function 
  | Closure _ | Pair _ -> Sym "F" 
  | Num _ | Sym _  -> Sym "T" 

let rec unique = function 
  | x :: xs -> if List.mem x xs then false else unique xs 
  | [] -> true

let extract_names args = 
  let rec extract acc = function 
    | Sym "NIL" -> List.rev acc 
    | Pair(Sym n, t) -> extract (n :: acc) t 
    | _ -> failwith ("expected string list, got " ^ to_string args) 
  in
  let names = extract [] args in 
  if unique names then names else failwith "lhs must be unique"  

let rec eval env = function 
  | Num i -> Num i 
  | Sym s -> Env.lookup env s 
  | Pair (Sym "QUOTE", e) -> car e 
  | Pair (Sym "CAR", e) -> car (eval env (car e))
  | Pair (Sym "CDR", e) -> cdr (eval env (car e)) 
  | Pair (Sym "CONS", e) -> 
    let a = eval env (car e) in 
    let b = eval env (car (cdr e)) in 
    Pair (a, b) 
  | Pair (Sym "ADD", e) -> arith env ( + ) e
  | Pair (Sym "SUB", e) -> arith env ( - ) e
  | Pair (Sym "MUL", e) -> arith env ( * ) e
  | Pair (Sym "DIV", e) -> arith env ( / ) e
  | Pair (Sym "REM", e) -> arith env Int.rem e
  | Pair (Sym "IF", e) -> begin 
    let c = car e in 
    let t = car (cdr e) in 
    let e = car (cdr (cdr e)) in 
    match (eval env c) with 
    | Sym "T" -> eval env t 
    | Sym "F" -> eval env e 
    | _ as e -> failwith ("expected boolean, got " ^ to_string e)
  end
  | Pair (Sym "ATOM", e) -> is_atom (eval env (car e))
  | Pair (Sym "EQ", e) -> 
    let e1 = car e in 
    let e2 = car (cdr e) in 
    eq (eval env e1) (eval env e2) 
  | Pair (Sym "LEQ", e) -> 
    let e1 = car e in 
    let e2 = car (cdr e) in 
    leq (eval env e1) (eval env e2) 
  | Pair (Sym "LAMBDA", e) -> 
    let args = car e in 
    let body = car (cdr e) in 
    Closure (extract_names args, body, env) 
  | Pair (Sym "LET", e) -> 
    let body = car e in 
    let defs = cdr e in 
    let vars = vars defs in 
    let exprs = List.map (eval env) (exprs defs) in 
    let env' = List.fold_left2 Env.bind env vars exprs in 
    eval env' body
  | Pair (Sym "LETREC", e) -> 
    let body = car e in 
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
  | _ as e -> failwith ("Error: " ^ to_string e)

and evlis env = function 
  | Sym "NIL" -> [] 
  | Pair (a, b) -> eval env a :: evlis env b
  | _ as e -> failwith ("expected list of expressions, got " ^ to_string e)

and vars = function
  | Sym "NIL" -> []
  | Pair (Pair(Sym s, _), b) -> 
    s :: (vars b)
  | _ as e -> failwith ("expected list of names, got " ^ to_string e) 

and exprs = function 
  | Sym "NIL" -> [] 
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
  | _ -> failwith ("expected closure, got " ^ to_string func) 