%{
    open Ast 
%}

%token <string> TK_SYM  
%token <int> TK_INT 
%token TK_LPAREN "(" 
%token TK_RPAREN ")"
%token TK_DOT "." 
%token TK_QUOTE "QUOTE" 
%token TK_ADD "ADD" 
%token TK_SUB "SUB" 
%token TK_MUL "MUL" 
%token TK_DIV "DIV" 
%token TK_REM "REM" 
%token TK_EQ "EQ" 
%token TK_LEQ "LEQ" 
%token TK_CAR "CAR" 
%token TK_CDR "CDR" 
%token TK_CONS "CONS" 
%token TK_ATOM "ATOM" 
%token TK_IF "IF" 
%token TK_LAMBDA "LAMBDA"
%token TK_LET "LET" 
%token TK_LETREC "LETREC" 
%token TK_NIL "NIL" 
%token TK_TRUE "T" 
%token TK_FALSE "F" 
%token TK_EOF "EOF" 

%start <Ast.t> parse 

%% 

parse: e = expr "EOF" { e }

expr: 
    | a = satom { a }
    | "(" l = slist ")" { l }

satom: 
    | n = TK_INT { Num n }
    | s = TK_SYM { Sym s } 
    | "NIL" | "(" ")" { Nil }
    | "T" { True }
    | "F" { False }

slist: 
    | "QUOTE" e = expr { Quote e }
    | "ADD" e1 = expr e2 = expr { Add (e1, e2) }
    | "SUB" e1 = expr e2 = expr { Sub (e1, e2) }
    | "MUL" e1 = expr e2 = expr { Mul (e1, e2) }
    | "DIV" e1 = expr e2 = expr { Div (e1, e2) }
    | "REM" e1 = expr e2 = expr { Rem (e1, e2) }
    | "EQ" e1 = expr e2 = expr { Eq (e1, e2) }
    | "LEQ" e1 = expr e2 = expr { Leq (e1, e2) }
    | "CAR" e = expr { Car e }
    | "CDR" e = expr { Cdr e }
    | "CONS" e1 = expr e2 = expr { Cons (e1, e2) }
    | "ATOM" e = expr { Atom e }
    | "IF" e1 = expr e2 = expr e3 = expr { If (e1, e2, e3) }
    | "LAMBDA" "(" nl = TK_SYM+ ")" e = expr { Lambda (nl, e) } 
    | "LET" e = expr dl = named_expr* { Let (e, dl) }
    | "LETREC" e = expr dl = named_expr* { Letrec (e, dl) }

named_expr:
    | name = TK_SYM e = expr { name, e }
