let fall (buf : Lexing.lexbuf) c =
  let p = buf.lex_curr_p in
  Printf.eprintf
    "[ERROR] %d:%d Failed to process the input at %c\n"
    p.pos_lnum
    p.pos_cnum
    c;
  exit 1
;;

type binout =
  | Num of int
  | Add of binout * binout

type var =
  | String of string
  | Int of int
  | VarBinExpr of binout
(* TODO: libcexpr *)

type libcout = string * var list

type out =
  | BinExpr of binout
  | LibcExpr of libcout
