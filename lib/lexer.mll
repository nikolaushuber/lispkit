{
    open Parser 
}

let space = [' ' '\t' '\n'] 
let newline = ('\013'* '\010')
let digit = ['0' - '9']
let alpha = ['a'-'z'] | ['A'-'Z']
let ident = alpha (alpha | digit)* 
let integer = '-'? digit+ 

rule lex = parse 
    | space { lex lexbuf }
    | eof { TK_EOF }
    | ";" { comment lexbuf }
    | "(" { TK_LPAREN }
    | ")" { TK_RPAREN }
    | "." { TK_DOT }
    | "QUOTE" { TK_QUOTE }
    | "ADD" { TK_ADD }
    | "SUB" { TK_SUB }
    | "MUL" { TK_MUL }
    | "DIV" { TK_DIV }
    | "REM" { TK_REM }
    | "EQ" { TK_EQ }
    | "LEQ" { TK_LEQ }
    | "CAR" { TK_CAR }
    | "CDR" { TK_CDR }
    | "CONS" { TK_CONS }
    | "ATOM" { TK_ATOM }
    | "IF" { TK_IF }
    | "LAMBDA" { TK_LAMBDA }
    | "LET" { TK_LET }
    | "LETREC" { TK_LETREC }
    | "NIL" { TK_NIL }
    | "T" { TK_TRUE }
    | "F" { TK_FALSE }
    | integer as i { TK_INT (int_of_string i) }
    | ident as lxm { TK_SYM (String.lowercase_ascii lxm) }
    

and comment = parse 
    | newline { lex lexbuf }
    | _ { comment lexbuf }
