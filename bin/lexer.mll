{ 
    open Parser 
    open Ast 
}
let skip = [' ' '\t' '\n'] | ';'[^'\n']*

let digits = (['1'-'9'] ['0'-'9']* | '0')
let symbol = ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
let string = '"' [^'"']+ '"'
let binop = ['+' '-' '*' '/']
let logicop = "and" | "or" | "=" | "<" | ">" | "<=" | ">="
(*let type = "I32" | "I8" | "String" | "Bool" | "Void" | "..." *)
rule lex = parse 
| "->" { ARROW }
| "if" { IF }
| "defun" { DEFUN }
| "let" { LET }
| "defparameter" { DEFPARAM }
| "true" { TRUE }
| "false" { FALSE }
| "extern" { EXTERN }
| "struct" { STRUCTDEF }
| binop as b { BINOP b }
| logicop as l { LOGICOP l }
| ':' { SEMICOLON }
| '(' { LPAREN }
| ')' { RPAREN }
| skip { lex lexbuf }
| digits as n { INT (int_of_string n) }
| symbol as f { SYMBOL f}
| string as s { STRING (String.sub s 1 (String.length s - 2)) }
| eof  { EOF }
| _ as p  { fall lexbuf p }
