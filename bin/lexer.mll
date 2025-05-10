{ 
    open Parser 
    open Ast 
}

let digits = (['1'-'9'] ['0'-'9']* | '0')
let extrn = ['a'-'z' 'A'-'Z']+
let string = '"' [^'"']+ '"'
let binop = ['+' '-' '*' '/']
let logicop = "and" | "or" | "=" | "<" | ">" | "<=" | ">="
rule lex = parse 
| "if" { IF }
| "defun" { DEFUN }
| "true" { TRUE }
| "false" { FALSE }
| binop as b { BINOP b }
| logicop as l { LOGICOP l }
| '(' { LPAREN }
| ')' { RPAREN }
| [' ' '\t' '\n'] { lex lexbuf }
| digits as n { INT (int_of_string n) }
| extrn as f { SYMBOL f}
| string as s { STRING (String.sub s 1 (String.length s - 2)) }
| eof  { EOF }
| _ as p  { fall lexbuf p }
