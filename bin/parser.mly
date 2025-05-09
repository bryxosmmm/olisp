%{
open Ast
%}
%token EOF LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token <int> INT
%token <string> STRING
%token <string> EXTRN
(*Boolean stuff*)
%token IF EQ TRUE FALSE
%start <out> main

%%
main:
| EOF {BinExpr (Num 0)}
| expr EOF { $1 }
| error { failwith "Syntax error" }

expr:
| ifexpr {IfExpr $1}
| binexpr {BinExpr $1}
| libcexpr {LibcExpr $1}


binexpr:
| LPAREN RPAREN { Num 0 }
| INT { Num $1 }
| LPAREN PLUS binexpr binexpr RPAREN { Add($3, $4) }
| LPAREN MINUS binexpr binexpr RPAREN { Minus($3, $4) }
| LPAREN TIMES binexpr binexpr RPAREN { Times($3, $4) }
| LPAREN DIV binexpr binexpr RPAREN { Div($3, $4) }

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
| LPAREN IF boolean expr expr RPAREN { ($3, $4, $5) }
