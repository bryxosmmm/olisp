open Ast
open Printf

let ( <* ) = output_string

let dir = function
  | 0 -> "rdi"
  | 1 -> "rsi"
  | 2 -> "rdx"
  | 3 -> "rcx"
  | 4 -> "r8"
  | 5 -> "r9"
  | _ -> failwith "TODO"
;;

let process_string s =
  String.to_seq s
  |> List.of_seq
  |> List.map (fun x ->
    match x with
    | '\n' -> "\",10,\""
    | '\t' -> "\",9,\""
    | n -> String.make 1 n)
  |> String.concat ""
;;

module type WRITEABLE = sig
  type t

  val gen : out_channel -> t -> unit
  val static : out_channel -> unit
  val write : out_channel -> t -> unit
end

module Data : WRITEABLE with type t = libcout = struct
  type t = libcout

  let gen f (_, v) =
    let count = 0 in
    let rec aux c = function
      | [] -> ()
      | [ h ] ->
        (match h with
         | String s -> f <* sprintf "msg%d db %s,0\n" c (process_string s)
         | Int i -> f <* sprintf "msg%d dq %d\n" c i
         | _ -> ())
      | h :: t ->
        aux c [ h ];
        aux (c + 1) t
    in
    aux count v
  ;;

  let static f = f <* sprintf "section \".data\"\n"

  let write f o =
    static f;
    gen f o
  ;;
end

module Extrn : WRITEABLE with type t = libcout = struct
  type t = libcout

  let gen f (e, _) = f <* sprintf "extrn %s\n" e
  let static _ = ()
  let write f o = gen f o
end

let rec walk_binexpr f = function
  | Num a -> f <* sprintf "mov rax, %d\n" a
  | Add (e1, e2) ->
    walk_binexpr f e1;
    f <* "push rax\n";
    walk_binexpr f e2;
    f <* "pop rbx\n";
    f <* "add rax, rbx\n"
;;

let walk_libcexpr f (e, v) =
  let count = List.length v in
  let rec aux c =
    if count = c
    then ()
    else (
      (match List.nth v c with
       | String _ -> f <* sprintf "mov %s, msg%d\n" (dir c) c
       | Int _ -> f <* sprintf "mov %s, [msg%d]\n" (dir c) c
       | VarBinExpr e ->
         walk_binexpr f e;
         f <* sprintf "mov %s, rax\n" (dir c));
      aux (c + 1))
  in
  aux 0;
  f <* sprintf "xor rax, rax\n";
  f <* sprintf "call %s\n" e
;;

let walk f = function
  | BinExpr e -> walk_binexpr f e
  | LibcExpr e -> walk_libcexpr f e
;;

let program f (o : out) =
  f <* "format ELF64\n";
  f <* "section \".text\" executable\n";
  f <* "public main\n";
  (match o with
   | LibcExpr v -> Extrn.write f v
   | _ -> ());
  f <* "main:\n";
  walk f o;
  f <* "mov rax, 0\n";
  f <* "ret\n";
  match o with
  | LibcExpr v -> Data.write f v
  | _ -> ()
;;
