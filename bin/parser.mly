%{
open Ast
%}
%token EOF LPAREN RPAREN
%token <int> INT
%token <string> STRING
%token <string> EXTRN
%token <char> BINOP
(*Boolean stuff*)
%token IF TRUE FALSE
%token <string> LOGICOP
%start <expr> main

%%
main:
| EOF {Int 0 }
| expr EOF { $1 }
| error { failwith "Syntax error" }

expr:
| INT { Int $1 }
| STRING { String $1 }
| binexpr { $1 }
| libcexpr { $1 }
| ifexpr { $1 }
| boolean { $1 }

binexpr:
| LPAREN RPAREN { Int 0 }
| LPAREN BINOP expr expr RPAREN { Binop($2, $3, $4) }

libcexpr:
| LPAREN EXTRN variadic RPAREN { Call($2, $3) }

variadic:
| { [] }
| expr variadic { $1 :: $2 }

boolean:
| TRUE { Bool true }
| FALSE { Bool false }
| LPAREN LOGICOP expr expr RPAREN { Logicop($2, $3, $4) }

ifexpr:
| LPAREN IF expr expr expr RPAREN { If($3, $4, $5) }
