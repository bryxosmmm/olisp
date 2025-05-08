%{
open Ast
%}
%token EOF PLUS LPAREN RPAREN
%token <int> INT
%token <string> STRING
%token <string> EXTRN
(*Boolean stuff*)
%token IF EQ TRUE FALSE
%start <out> main

%%
main:
| EOF {BinExpr (Num 0)}
| ifexpr EOF {IfExpr $1}
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

boolean:
| TRUE { BooleanLiteral true }
| FALSE { BooleanLiteral false }

ifexpr:
| LPAREN IF boolean libcexpr RPAREN { ($3, LibcExpr $4) }
| LPAREN IF boolean binexpr RPAREN { ($3, BinExpr $4) }
