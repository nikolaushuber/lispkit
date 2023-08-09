%{
    open Ast 
%}

%token <string> TK_STRING 
%token <int> TK_INT 
%token TK_LPAREN "(" 
%token TK_RPAREN ")"
%token TK_DOT "." 
%token TK_EOF "eof" 

%start <Ast.t> parse 

%% 

parse: 
    | s = sexpr "eof" { s }

sexpr: 
    | a = atom { Atom a } 
    | "(" sl = sexprlist ")" { List sl }

atom: 
    | i = TK_INT { (Num i) }
    | str = TK_STRING { (Sym str) }

sexprlist: 
    | s = sexpr { [s] }
    | s1 = sexpr "." s2 = sexpr { [s1; s2] }
    | s = sexpr sl = sexprlist { s :: sl }
