open Parser
open Lexer

let lexbuf s = Lexing.from_string s
let parse buf = buf |> main lex
let chan () = open_out "out/out.ll"

let () =
  (* let buf = lexbuf "(if (= 100 100) (putchar 69) (putchar 68))" in *)
  let buf = lexbuf "(defun greet (a b) (putchar (+ a b))) (greet 10 59)" in
  parse buf |> Dragon.program (chan ())
;;
