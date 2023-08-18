open Ast 

let atom = function 
  | Atom _ -> Atom "T" 
  | Pair _ -> Atom "F" 

let bool = function 
  | Atom "T" -> true 
  | Atom "F" -> false 
  | _ -> failwith "T or F expected"

let car = function 
  | Pair (x, _) -> x 
  | Atom _ -> failwith "car of atom"

let cdr = function 
  | Pair (_, x) -> x 
  | Atom _ -> failwith "cdr of atom" 

let cons a b = Pair (a, b) 

let eq a b = match a, b with 
  | Atom x, Atom y when String.equal x y -> Atom "T" 
  | _ -> Atom "F" 

let rec assoc x n v = 
  if member x (car n) |> bool then locate x (car n) (car v) 
  else assoc x (cdr n) (cdr v) 

and locate x l m = 
  if eq x (car l) |> bool then car m 
  else locate x (cdr l) (cdr m) 

and member x l = 
  if eq l (Atom "NIL") |> bool then Atom "F"
  else if eq x (car l) |> bool then Atom "T" 
  else member x (cdr l) 

let rec vars d = 
  if d = Atom "NIL" then Atom "NIL" else cons (car (car d)) (vars (cdr d))

let rec exprs d = 
  if d = Atom "NIL" then Atom "NIL" else cons (cdr (car d)) (exprs (cdr d)) 

let to_int s = 
  let _int s = 
    try int_of_string s with _ -> failwith ("expected a number, got " ^ s) 
  in match s with 
  | Atom s -> _int s 
  | Pair _ -> failwith "expected a number, got a pair" 

let rec eval e n v = 
  if atom e |> bool then assoc e n v 
  else match car e with 
  | Atom "QUOTE" -> car (cdr e) 
  | Atom "ADD" -> arith Int.add e n v 
  | Atom "SUB" -> arith Int.sub e n v 
  | Atom "MUL" -> arith Int.mul e n v 
  | Atom "DIV" -> arith Int.div e n v 
  | Atom "REM" -> arith Int.rem e n v 
  | Atom "CAR" -> car (eval (car (cdr e)) n v) 
  | Atom "CDR" -> cdr (eval (car (cdr e)) n v) 
  | Atom "ATOM" -> atom (eval (car (cdr e)) n v) 
  | Atom "CONS" -> cons (eval (car (cdr e)) n v) (eval (car (cdr (cdr e))) n v) 
  | Atom "EQ" -> eq (eval (car (cdr e)) n v) (eval (car (cdr (cdr e))) n v) 
  | Atom "IF" -> 
      let e1 = car (cdr e) 
      and e2 = car (cdr (cdr e)) 
      and e3 = car (cdr (cdr (cdr e))) in 
      eval (if (eval e1 n v) |> bool then e2 else e3) n v 
  | Atom "LAMBDA" -> cons (cons ((car (cdr e))) (car (cdr (cdr e)))) (cons n v) 
  | Atom "LET" -> 
      let y = vars (cdr (cdr e)) 
      and z = evlis (exprs (cdr (cdr e))) n v in 
      eval (car (cdr e)) (cons y n) (cons z v) 
  | Atom "LETREC" -> 
      let y = vars (cdr (cdr e)) 
      and v' = cons (Atom "PENDING") v in 
      let z = evlis (exprs (cdr (cdr e))) (cons y n) v' in 
      eval (car (cdr e)) (cons y n) (rplaca v' z) 
  | Atom s -> failwith ("Unknown command " ^ s) 
  | Pair _ -> 
      let c = eval (car e) n v 
      and z = evlis (cdr e) n v in 
      eval (cdr (car c)) (cons (car (car c)) (car (cdr c))) (cons z (cdr (cdr c)))

and evlis l n v = 
  if l = Atom "NIL" then Atom "NIL" else cons (eval (car l) n v) (evlis (cdr l) n v)

and rplaca _ _ = failwith "rplaca not yet implemented"

and arith op e n v = 
  let e1 = eval (car (cdr e)) n v in 
  let e2 = eval (car (cdr (cdr e))) n v in 
  let a1 = to_int e1 in 
  let a2 = to_int e2 in 
  Atom (string_of_int (op a1 a2)) 