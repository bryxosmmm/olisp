%{
open Ast
%}
%token EOF PLUS LPAREN RPAREN
%token <int> INT
%start <out> main


%%
main:
| EOF {Num 0}
| expr EOF {$1}

expr:
| LPAREN RPAREN { Num 0 }
| INT { Num $1 }
| LPAREN PLUS expr expr RPAREN { Add($3, $4) }

