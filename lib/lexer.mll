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
    | integer as i { TK_INT (int_of_string i) }
    | ident as lxm { TK_STRING (String.lowercase_ascii lxm) }

and comment = parse 
    | newline { lex lexbuf }
    | _ { comment lexbuf }
