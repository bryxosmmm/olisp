open Ast
open Printf

let ( <* ) = output_string

let rec walk f = function
  | Num a -> f <* sprintf "mov rax, %d\n" a
  | Add (e1, e2) ->
    walk f e1;
    f <* "push rax\n";
    walk f e2;
    f <* "pop rbx\n";
    f <* "add rax, rbx\n"
;;
