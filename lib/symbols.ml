open Llvm

type scopes =
  | Global
  | Local of llbasicblock

type record =
  | Function of lltype * llvalue
  | Variable of llvalue
  | Typeof of lltype list

type t =
  { fn : llvalue
  ; ty : lltype
  ; entry : llbasicblock
  }

type symbol_t = (string, record) Hashtbl.t

let return ty fn entry = { fn; ty; entry }
let insert name v tbl = Hashtbl.add tbl name v
let get name tbl = Hashtbl.find_opt tbl name
