let fall (buf : Lexing.lexbuf) c =
  let p = buf.lex_curr_p in
  Printf.eprintf
    "[ERROR] %d:%d Failed to process the input at %c\n"
    p.pos_lnum
    p.pos_cnum
    c;
  exit 1
;;

type binop =
  | Num of int
  | Add of binop * binop
  | Minus of binop * binop
  | Times of binop * binop
  | Div of binop * binop

type expr =
  | Bool of bool
  | Int of int
  | String of string
  | If of expr * expr * expr
  | Binop of char * expr * expr
  | Logicop of string * expr * expr
  | Call of string * expr list
