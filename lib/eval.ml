open Ast 

type env = (string * t) list 



let rec eval env = function 
  | Num i -> Num i 
  | Sym s -> Sym s 
  | Quote t -> t 
  | Add (t1, t2) -> aeval env ( + ) t1 t2 
  | Sub (t1, t2) -> aeval env ( - ) t1 t2 
  | Mul (t1, t2) -> aeval env ( * ) t1 t2 
  | Div (t1, t2) -> aeval env ( / ) t1 t2 
  | Rem (t1, t2) -> aeval env Int.rem t1 t2 
  | Eq (t1, t2) -> _eq env t1 t2 
  | Leq (t1, t2) -> _leq env t1 t2 
  | Car (Cons (t, _)) -> t 
  | Cdr (Cons (_, t)) -> t 
  | Car _ -> failwith "car error" 
  | Cdr _ -> failwith "cdr error" 
  | Cons (t1, t2) -> Cons (t1, t2) 
  | Atom (Num _) | Atom (Sym _) -> True 
  | Atom _ -> False 
  | If (c, t, e) -> _if env c t e 
  | Lambda _ -> failwith "lambda not yet implemented" 
  | App _ -> failwith "app not yet implemented" 
  | Let _ -> failwith "let not yet implemented" 
  | Letrec _ -> failwith "letrec not yet implemented" 
  | Nil -> Nil 
  | True -> True 
  | False -> False 

and aeval env f e1 e2 = 
  let e1 = eval env e1 in 
  let e2 = eval env e2 in 
  match e1, e2 with 
  | Num n1, Num n2 -> Num (f n1 n2) 
  | _ -> failwith "expected two integers"

and _eq env e1 e2 = 
  let e1 = eval env e1 in 
  let e2 = eval env e2 in 
  match e1, e2 with 
  | Num n1, Num n2 when Int.equal n1 n2 -> True
  | Sym s1, Sym s2 when String.equal s1 s2 -> True 
  | _ -> False

and _leq env e1 e2 = 
  let e1 = eval env e1 in 
  let e2 = eval env e2 in 
  match e1, e2 with 
  | Num n1, Num n2 when n1 <= n2 -> True 
  | _ -> False 

and _if env c t e = 
  match eval env c with 
  | True -> eval env t 
  | False -> eval env e 
  | _ -> failwith "if condition must be boolean"
