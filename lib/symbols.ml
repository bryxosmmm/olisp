open Llvm

type scopes =
  | Global
  | Local of llbasicblock

type record =
  | Function of lltype * llvalue
  | Variable of llvalue
  | Typeof of lltype list
  | Type of lltype

type t =
  { fn : llvalue
  ; ty : lltype
  ; entry : llbasicblock
  }

type symbol_t = (string, record) Hashtbl.t

let return ty fn entry = { fn; ty; entry }
let insert name v tbl = Hashtbl.add tbl name v
let get name tbl = Hashtbl.find_opt tbl name

let get_or name fn tbl =
  match get name tbl with
  | Some v -> v
  | None ->
    let v = fn () in
    insert name v tbl;
    v
;;
