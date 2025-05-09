open Parser
open Lexer

let lexbuf s = Lexing.from_string s
let parse buf = buf |> main lex
let chan () = open_out "out/out.ll"

let () =
  let buf = lexbuf "(if false (putchar (+ 34 35)) (putchar 49))" in
  parse buf |> Dragon.program (chan ())
;;
