open Sexp 
let car = function 
  | Pair (a, _) -> !a
  | _ as e -> failwith ("car error: " ^ to_string e)

let cdr = function 
  | Pair (_, b) -> b 
  | _ as e -> failwith ("cdr error: " ^ to_string e)

let cons a b = Pair (ref a, b) 
let atom = function 
  | Pair _ -> Sym "F" 
  | Num _ | Sym _  -> Sym "T" 

let rec unique = function 
  | x :: xs -> if List.mem x xs then false else unique xs 
  | [] -> true

let eq e1 e2 = match e1, e2 with 
  | Num n, Num m -> if n = m then Sym "T" else Sym "F" 
  | Sym s, Sym t -> if String.equal s t then Sym "T" else Sym "F" 
  | _ -> Sym "F"

let leq e1 e2 = match e1, e2 with 
  | Num n, Num m -> if n <= m then Sym "T" else Sym "F" 
  | _ -> failwith ("leq expects to numbers, got " ^ to_string e1 ^ " , " ^ to_string e2)

let rec assoc x n v = match member x (car n) with 
  | Sym "T" -> locate x (car n) (car v) 
  | Sym "F" -> assoc x (cdr n) (cdr v) 
  | _ -> assert false 

and locate x l m = match (eq x (car l)) with 
  | Sym "T" -> car m 
  | Sym "F" -> locate x (cdr l) (cdr m) 
  | _ -> assert false 

and member x l = match eq l (Sym "NIL") with 
  | Sym "T" -> Sym "F" 
  | Sym  "F" -> begin match eq x (car l) with 
    | Sym "T" -> Sym "T" 
    | _ -> member x (cdr l)
  end
  | _ -> assert false 

let rplaca x y = match x with 
  | Pair({contents = Sym "PENDING"} as r, _) -> 
    r := y; x 
  | _ -> failwith ("rplaca error: " ^ to_string x ^ " , " ^ to_string y) 

let rec index n s = match n with 
  | Num 0 -> car s 
  | Num n -> index (Num (n-1)) (car s) 
  | _ -> failwith ("index error: " ^ to_string n ^ " , " ^ to_string s)

let locate i e = 
  let b = car i in 
  let n = cdr i in 
  index n (index b e)

let pop s = 
  car s, cdr s 

let pop2 s = 
  let a = car s in 
  let b = car (cdr s) in 
  a, b, cdr (cdr s) 

let pop3 s = 
  let a = car s in 
  let b = car (cdr s) in 
  let c = car (cdr (cdr s)) in 
  let d = car (cdr (cdr (cdr s))) in 
  a, b, c, d