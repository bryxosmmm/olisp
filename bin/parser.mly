%{
open Ast
%}
%token EOF LPAREN RPAREN ARROW SEMICOLON
%token <int> INT
%token <string> STRING
%token <string> SYMBOL
%token <char> BINOP
%token IF TRUE FALSE
%token <string> LOGICOP
%token DEFUN DEFPARAM LET
%token <string> TYPE
%start <expr list> main

%%
main:
| EOF { [] }
| expr_list EOF {$1}
| error { failwith "Syntax error" }

expr:
| INT { Int $1 }
| STRING { String $1 }
| SYMBOL { Symbol $1 }
| binexpr { $1 }
| callexpr { $1 }
| ifexpr { $1 }
| boolean { $1 }
| defun { $1 }
| typedef { $1 }
| defparam { $1 }
| defvar { $1 }


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
| LPAREN DEFUN SYMBOL funargs LPAREN expr_list RPAREN RPAREN { Defun($3, $4, Block $6) }

funargs:
| { [] }
| SYMBOL funargs { $1 :: $2 }
| LPAREN funargs RPAREN { $2 }


typedef:
| LPAREN SEMICOLON SYMBOL LPAREN types RPAREN RPAREN { Typedef($3, $5) }

types:
| { [] }
| TYPE { [$1] }
| TYPE ARROW types { $1 :: $3 }

defparam:
| LPAREN DEFPARAM SYMBOL expr RPAREN { Defparam($3, $4) }

defvar:
| LPAREN LET SYMBOL expr RPAREN { Defvar($3, $4) }

expr_list:
| expr { [$1] }
| expr expr_list { $1 :: $2 }
