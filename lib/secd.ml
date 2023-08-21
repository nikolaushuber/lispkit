open Sexp 
open Utils 

type instr = 
  | LD | LDC | LDF | AP | RTN | DUM | RAP | SEL | JOIN | CAR | CDR | ATOM 
  | CONS| EQ | ADD | SUB | MUL | DIV | REM | LEQ | STOP 

let to_code = function 
  | Num 1 -> LD 
  | Num 2 -> LDC 
  | Num 3 -> LDF 
  | Num 4 -> AP 
  | Num 5 -> RTN 
  | Num 6 -> DUM 
  | Num 7 -> RAP 
  | Num 8 -> SEL 
  | Num 9 -> JOIN 
  | Num 10 -> CAR 
  | Num 11 -> CDR 
  | Num 12 -> ATOM 
  | Num 13 -> CONS 
  | Num 14 -> EQ 
  | Num 15 -> ADD 
  | Num 16 -> SUB 
  | Num 17 -> MUL 
  | Num 18 -> DIV 
  | Num 19 -> REM 
  | Num 20 -> LEQ 
  | Num 21 -> STOP 
  | _ as e -> failwith ("Unknown instruction: " ^ to_string e)

let rec exec s e c d = match to_code (car c) with 
  | STOP -> car s 
  | ADD -> arith ( + ) s e c d 
  | SUB -> arith ( - ) s e c d 
  | MUL -> arith ( * ) s e c d 
  | DIV -> arith ( / ) s e c d 
  | REM -> arith Int.rem s e c d 
  | EQ -> 
    let a, b, s' = pop2 s in 
    exec (cons (eq a b) s') e (cdr c) d 
  | LEQ -> 
    let a, b, s' = pop2 s in 
    exec (cons (leq a b) s') e (cdr c) d 
  | CONS -> 
    let a, b, s' = pop2 s in 
    exec (cons (cons a b) s') e (cdr c) d 
  | CAR -> 
    let a, _, s' = pop2 s in 
    exec (cons a s') e (cdr c) d 
  | CDR -> 
    let _, b, s' = pop2 s in 
    exec (cons b s') e (cdr c) d 
  | LDC -> 
    let x = car (cdr c) in 
    let c' = cdr (cdr c) in 
    exec (cons x s) e c' d
  | ATOM -> 
    let a, s' = pop s in 
    let _, c' = pop c in 
    exec (cons (atom a) s') e c' d 
  | LD -> 
    let _, i, c' = pop2 c in 
    let x = locate i e in 
    exec (cons x s) e c' d 
  | LDF -> 
    let _, c', c'' = pop2 c in 
    let s' = cons (cons c' e) s in 
    exec s' e c'' d 
  | SEL -> 
    let _, ct, cf, c' = pop3 c in 
    let x, s' = pop s in 
    let cx = begin match x with 
      | Sym "T" -> ct 
      | Sym "F" -> cf 
      | _ -> failwith ("expected boolean, got " ^ to_string x) 
    end in 
    exec s' e cx (cons c' d) 
  | JOIN -> 
    let c', d' = pop d in 
    exec s e c' d' 
  | AP -> 
    let clos, v, s' = pop2 s in 
    let c' = car clos in 
    let e' = cdr clos in 
    let d' = cons s' (cons e (cons (cdr c) d)) in 
    exec (Sym "NIL") (cons v e') c' d' 
  | RTN -> 
    let x = car s in 
    let s', e', c', d' = pop3 d in 
    exec (cons x s') e' c' d' 
  | DUM -> 
    let _, c' = pop c in 
    exec s (cons (Sym "PENDING") e) c' d 
  | RAP -> 
    let clos, v, s' = pop2 s in 
    let c' = car clos in 
    let e' = cdr clos in 
    let d' = cons s' (cons e (cons (cdr c) d)) in 
    exec (Sym "NIL") (rplaca e' v) c' d' 

and arith op s e c d = 
  let a, b, s' = pop2 s in 
  match a, b with 
  | Num n, Num m -> exec (cons (Num (op n m)) s') e (cdr (cdr c)) d 
  | _ -> failwith ("arithmetic error: " ^ to_string a ^ " , " ^ to_string b)

