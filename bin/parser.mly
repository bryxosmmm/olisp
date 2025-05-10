%{
open Ast
%}
%token EOF LPAREN RPAREN
%token <int> INT
%token <string> STRING
%token <string> SYMBOL
%token <char> BINOP
%token IF TRUE FALSE
%token <string> LOGICOP
%token DEFUN
%start <expr list> main

%%
main:
| EOF { [] }
| expr_list EOF {$1}
| error { failwith "Syntax error" }

expr_list:
| expr { [$1] }
| expr expr_list { $1 :: $2 }

expr:
| INT { Int $1 }
| STRING { String $1 }
| SYMBOL { Symbol $1 }
| binexpr { $1 }
| callexpr { $1 }
| ifexpr { $1 }
| boolean { $1 }
| defun { $1 }

binexpr:
| LPAREN INT RPAREN { Int $2 }
| LPAREN RPAREN { Int 0 }
| LPAREN BINOP expr expr RPAREN { Binop($2, $3, $4) }

callexpr:
| LPAREN SYMBOL variadic RPAREN { Call($2, $3) }

variadic:
| { [] }
| expr variadic { $1 :: $2 }

boolean:
| TRUE { Bool true }
| FALSE { Bool false }
| LPAREN LOGICOP expr expr RPAREN { Logicop($2, $3, $4) }

ifexpr:
| LPAREN IF expr expr expr RPAREN { If($3, $4, $5) }

defun:
| LPAREN DEFUN SYMBOL funargs expr RPAREN { Defun($3, $4, $5) }

funargs:
| { [] }
| SYMBOL funargs { $1 :: $2 }
| LPAREN funargs RPAREN { $2 }
