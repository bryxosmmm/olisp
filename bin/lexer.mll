{ open Parser }

let digits = (['1'-'9'] ['0'-'9']* | '0')
rule lex = parse 
| [' ' '\t' '\n'] { lex lexbuf }
| digits as n { INT (int_of_string n) }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { PLUS }
| eof  { EOF }
| _  { failwith "Unkown character"}
