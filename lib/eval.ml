open Sexp
open Utils 

let rec eval e n v = match e with 
  | Num i -> Num i 
  | Sym _ -> assoc e n v
  | _ -> begin match car e with 
    | Sym "QUOTE" -> car (cdr e) 
    | Sym "CAR" -> car (eval (car (cdr e)) n v)
    | Sym "CDR" -> cdr (eval (car (cdr e)) n v) 
    | Sym "ATOM" -> atom (eval (car (cdr e)) n v)
    | Sym "CONS" -> 
      let a = eval (car (cdr e)) n v in 
      let b = eval (car (cdr (cdr e))) n v in 
      cons a b 
    | Sym "ADD" -> arith ( + ) e n v
    | Sym "SUB" -> arith ( - ) e n v
    | Sym "MUL" -> arith ( * ) e n v
    | Sym "DIV" -> arith ( / ) e n v
    | Sym "REM" -> arith Int.rem e n v
    | Sym "IF" -> begin 
      let c = car (cdr e) in 
      let t = car (cdr (cdr e)) in 
      let e = car (cdr (cdr (cdr e))) in 
      match (eval c n v) with 
      | Sym "T" -> eval t n v
      | Sym "F" -> eval e n v
      | _ as e -> failwith ("expected boolean, got " ^ to_string e)
    end
    | Sym "EQ" -> 
      let e1 = car (cdr e) in 
      let e2 = car (cdr (cdr e)) in 
      eq (eval e1 n v) (eval e2 n v) 
    | Sym "LEQ" -> 
      let e1 = car (cdr e) in 
      let e2 = car (cdr (cdr e)) in 
      leq (eval e1 n v) (eval e2 n v) 
    | Sym "LAMBDA" -> 
      let args = car (cdr e) in 
      let body = car (cdr (cdr e)) in 
      cons (cons args body) (cons n v)  
    | Sym "LET" -> 
      let y = vars (cdr (cdr (e))) in 
      let z = evlis (exprs (cdr (cdr (e)))) n v in
      eval (car (cdr e)) (cons y n) (cons z v) 
    | Sym "LETREC" -> 
      let y = vars (cdr (cdr e)) in 
      let v' = cons (Sym "PENDING") v in 
      let z = evlis (exprs (cdr (cdr e))) (cons y n) v' in 
      eval (car (cdr e)) (cons y n) (rplaca v' z) 
    | _ -> 
      let c = eval (car e) n v in 
      let z = evlis (cdr e) n v in 
      eval (cdr (car c)) (cons (car (car c)) (car (cdr c))) (cons z (cdr (cdr c))) 
  end

and evlis l n v = match l with  
  | Sym "NIL" -> Sym "NIL" 
  | _ -> cons (eval (car l) n v) (evlis (cdr l) n v)

and vars x = match x with 
  | Sym "NIL" -> Sym "NIL"
  | _ -> cons (car (car x)) (vars (cdr x)) 

and exprs x = match x with  
  | Sym "NIL" -> Sym "NIL"
  | _ -> cons (cdr (car x)) (exprs (cdr x)) 

and arith op e n v = 
  let a = eval (car (cdr e)) n v in 
  let b = eval (car (cdr (cdr e))) n v in 
  match a, b with 
  | Num n, Num m -> Num (op n m) 
  | _ -> failwith "expected two numbers" 