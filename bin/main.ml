open Parser
open Lexer

let parse s = Lexing.from_string s |> main lex
let chan () = open_out "out.asm"
let () = parse "(+ (+ 17 17) 35)" |> Asm.walk (chan ())
