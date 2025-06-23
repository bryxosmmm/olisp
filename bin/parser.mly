%{
open Ast
%}
%token EOF LPAREN RPAREN ARROW SEMICOLON
%token <int> INT 
%token <string> STRING
%token <string> SYMBOL MSYMBOL
%token <char> BINOP
%token IF TRUE FALSE
%token <string> LOGICOP
%token DEFUN DEFPARAM LET EXTERN STRUCTDEF MACRODEF
%start <expr list> main

%%
main:
| EOF { [] }
| expr_list EOF {$1}
| error { failwith "Syntax error" }

expr:
| intexpr { $1 }
| STRING { String $1 }
| SYMBOL { Symbol $1 }
| binexpr { $1 }
| callexpr { $1 }
| ifexpr { $1 }
| boolean { $1 }
| defun { $1 }
| typedef { $1 }
| externdef { $1 }
| defparam { $1 }
| defvar { $1 }
| structdef { $1 }
| macrodef { $1 }

intexpr:
| INT { Int ($1, "I32") }
| LPAREN INT SYMBOL RPAREN { Int ($2, $3) }


binexpr:
| LPAREN INT RPAREN { Int ($2, "I32") }
| LPAREN RPAREN { Int (0, "I32") }
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
| LPAREN IF expr expr_list expr_list RPAREN { If($3, Block $4, Block $5) }

defun:
| LPAREN DEFUN SYMBOL funargs expr RPAREN { Defun($3, $4, $5) }
| LPAREN DEFUN SYMBOL funargs LPAREN expr_list RPAREN RPAREN { Defun($3, $4, Block $6) }

funargs:
| { [] }
| SYMBOL funargs { $1 :: $2 }
| LPAREN funargs RPAREN { $2 }


typedef:
| LPAREN SEMICOLON SYMBOL LPAREN types RPAREN RPAREN { Typedef($3, $5) }

externdef:
| LPAREN EXTERN SEMICOLON SYMBOL LPAREN types RPAREN RPAREN { Externdef("", $4, $6) }
| LPAREN EXTERN SYMBOL SEMICOLON SYMBOL LPAREN types RPAREN RPAREN { Externdef($3, $5, $7) }

types:
| { [] }
| SYMBOL { [$1] }
| SYMBOL ARROW types { $1 :: $3 }

defparam:
| LPAREN DEFPARAM SYMBOL expr RPAREN { Defparam($3, $4) }

defvar:
| LPAREN LET SYMBOL expr RPAREN { Defvar($3, $4) }

expr_list:
| expr { [$1] }
| expr expr_list { $1 :: $2 }

field:
| LPAREN SYMBOL SYMBOL RPAREN { ($2, $3) }

fields:
| { [] }
| field fields { $1 :: $2 }

structdef:
| LPAREN STRUCTDEF SYMBOL fields RPAREN { Struct($3, $4) }

macrodef:
| LPAREN MACRODEF SYMBOL funargs expr RPAREN { Macrodef($3, $4, $5) }
