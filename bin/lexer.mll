{ 
    open Parser 
    open Ast 
}

let digits = (['1'-'9'] ['0'-'9']* | '0')
let extrn = ['a'-'z' 'A'-'Z']+
let string = '"' [^'"']+ '"'
rule lex = parse 
| [' ' '\t' '\n'] { lex lexbuf }
| digits as n { INT (int_of_string n) }
| extrn as f { EXTRN f}
| string as s { STRING s }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { PLUS }
| eof  { EOF }
| _ as p  { fall lexbuf p }
