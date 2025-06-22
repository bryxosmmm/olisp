open Parser
open Lexer

let lexbuf s = Lexing.from_channel @@ open_in s
let parse buf = buf |> main lex
let chan () = open_out "out/out.ll"

let () =
  let buf = lexbuf "example.scm" in
  parse buf |> Dragon.program (chan ())
;;
