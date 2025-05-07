%{
open Ast
%}
%token EOF PLUS LPAREN RPAREN
%token <int> INT
%token <string> STRING
%token <string> EXTRN
(*Boolean stuff*)
%token EQ 
%start <out> main


%%
main:
| EOF {BinExpr (Num 0)}
| binexpr EOF {BinExpr $1}
| libcexpr EOF {LibcExpr $1}

binexpr:
| LPAREN RPAREN { Num 0 }
| INT { Num $1 }
| LPAREN PLUS binexpr binexpr RPAREN { Add($3, $4) }

libcexpr:
| LPAREN EXTRN variadic RPAREN { ($2, $3) }

variadic:
| { [] }
| STRING variadic { String $1 :: $2 }
| INT variadic { Int $1 :: $2 }
| binexpr variadic { VarBinExpr $1 :: $2 }
