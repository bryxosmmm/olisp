open Parser
open Lexer

let parse s = Lexing.from_string s |> main lex
let chan () = open_out "out/out.asm"

(* let () = parse "(printf \"Result: %d\n\" (+ (+ 10 24) 35))" |> Asm.program (chan ()) *)
let () = parse "(putchar 69)" |> Asm.program (chan ())
