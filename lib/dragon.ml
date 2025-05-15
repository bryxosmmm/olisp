open Llvm
open Ast
open Symbols

let ctx = create_context ()
let the_mod = create_module ctx "main_mod"
let i32_t = i32_type ctx
let i8_t = i8_type ctx
let bool_t = i1_type ctx
let main_ty = function_type i32_t [||]
let main_fn = declare_function "main" main_ty the_mod
let main_entry = append_block ctx "entry" main_fn
let bld = builder_at_end ctx main_entry
let symbols : symbol_t = Hashtbl.create 32

let write_mod f m =
  m |> string_of_llmodule |> output_string f;
  close_out f
;;

let variadic_fn name ret_ty arg_tys () =
  let fn_ty = var_arg_function_type ret_ty arg_tys in
  let fn = Llvm.declare_function name fn_ty the_mod in
  fn_ty, fn
;;

let nonvariadic_fn name ret_ty arg_tys the_mod () =
  let fn_ty = function_type ret_ty arg_tys in
  let fn = Llvm.declare_function name fn_ty the_mod in
  fn_ty, fn
;;

let def_printf = variadic_fn "printf" i32_t [| pointer_type2 ctx |]
let def_putchar = nonvariadic_fn "putchar" i32_t [| i32_t |] the_mod

let fn_of_string = function
  | "printf" -> def_printf ()
  | "putchar" -> def_putchar ()
  | _ -> failwith "[TODO:extrn] not processed extern function"
;;

let lltype_of_string = function
  | "Integer" -> i32_t
  | "String" -> pointer_type2 ctx
  | "Bool" -> bool_t
  | _ -> failwith "[TODO:typs] there is no such type yet"
;;

let get_fn s =
  let sym = get s symbols in
  match sym with
  | Some (Function (ty, fn)) -> ty, fn
  | Some _ -> failwith "[ERROR:fn] incorrect symbol"
  | None -> fn_of_string s
;;

let get_var s =
  let sym = get s symbols in
  match sym with
  | Some (Variable v) -> v
  | _ -> failwith "[ERROR:var] incorrect symbol"
;;

let get_type s =
  let sym = get s symbols in
  match sym with
  | Some (Typeof v) -> v
  | _ -> failwith "[ERROR:type] incorrect symbol"
;;

let rec build_binop = function
  | Symbol v -> get_var v
  | Int v -> const_int i32_t v
  | Binop ('+', a, b) ->
    let lhs = build_binop a in
    let rhs = build_binop b in
    build_add lhs rhs "binopout" bld
  | Binop ('-', a, b) ->
    let lhs = build_binop a in
    let rhs = build_binop b in
    build_sub lhs rhs "binopout" bld
  | Binop ('*', a, b) ->
    let lhs = build_binop a in
    let rhs = build_binop b in
    build_mul lhs rhs "binopout" bld
  | Binop ('/', a, b) ->
    let lhs = build_binop a in
    let rhs = build_binop b in
    build_sdiv lhs rhs "binopout" bld
  | Binop _ -> failwith "[TODO:binop] i need to impl it..."
  | _ -> failwith "[ERROR] incorrect value in binary operation"
;;

let build_logicop context f = function
  | Bool true -> const_int bool_t 1
  | Bool false -> const_int bool_t 0
  | Logicop (_, a, b) as v ->
    let lhs = f context a in
    let rhs = f context b in
    (match v with
     | Logicop ("and", _, _) -> build_and lhs rhs "andout" bld
     | Logicop ("or", _, _) -> build_or lhs rhs "orout" bld
     | Logicop ("=", _, _) -> build_icmp Icmp.Eq lhs rhs "eqout" bld
     | Logicop ("<", _, _) -> build_icmp Icmp.Slt lhs rhs "sltout" bld
     | Logicop (">", _, _) -> build_icmp Icmp.Sgt lhs rhs "sgtout" bld
     | Logicop ("<=", _, _) -> build_icmp Icmp.Sle lhs rhs "sleout" bld
     | Logicop (">=", _, _) -> build_icmp Icmp.Sge lhs rhs "sgeout" bld
     | _ -> failwith "[TODO:logic] i need to impl it...")
  | _ -> failwith "[ERROR] incorrect logicop"
;;

let build_call ({ entry; _ } as context) f name l =
  let _ = position_at_end entry bld in
  let args = List.map (f context) l |> Array.of_list in
  let ty, fn = get_fn name in
  let v = build_call2 ty fn args "calltmp" bld in
  v
;;

let build_fn f name args expr =
  let types = get_type name in
  let ty, fn =
    nonvariadic_fn
      name
      (List.hd types)
      (types |> List.tl |> List.rev |> Array.of_list)
      the_mod
      ()
  in
  List.iteri
    (fun i a ->
       let p = param fn i in
       insert a (Variable p) symbols;
       set_value_name a p)
    args;
  Symbols.insert name (Function (ty, fn)) symbols;
  let fn_bb = append_block ctx "entry" fn in
  let context = Symbols.return ty fn fn_bb in
  let _ = position_at_end fn_bb bld in
  let expr_val = f context expr in
  let _ = ignore (build_ret expr_val bld) in
  expr_val
;;

let build_if ({ fn; _ } as context) f cond_expr if_expr else_expr =
  let cond_val = f context cond_expr in
  let zero = const_int bool_t 0 in
  let cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" bld in
  let then_bb = append_block ctx "then" fn in
  let else_bb = append_block ctx "else" fn in
  let merge_bb = append_block ctx "merge" fn in
  ignore (build_cond_br cond_bool then_bb else_bb bld);
  position_at_end then_bb bld;
  let then_val = f { context with entry = then_bb } if_expr in
  ignore (build_br merge_bb bld);
  let then_bb_end = insertion_block bld in
  position_at_end else_bb bld;
  let else_val = f { context with entry = else_bb } else_expr in
  ignore (build_br merge_bb bld);
  let else_bb_end = insertion_block bld in
  position_at_end merge_bb bld;
  let phi = build_phi [ then_val, then_bb_end; else_val, else_bb_end ] "iftmp" bld in
  phi
;;

let proc_typedef name types =
  let lltypes = List.map lltype_of_string types |> List.rev in
  insert name (Typeof lltypes) symbols
;;

let build_defparam context f name e =
  let expr_val = f context e in
  insert name (Variable expr_val) symbols;
  define_global name expr_val the_mod
;;

let build_variable context f name e =
  let expr_val = f context e in
  let expr_type = type_of expr_val in
  let alloc_expr = build_alloca expr_type "allocval" bld in
  let _ = build_store expr_val alloc_expr bld in
  let load_expr = build_load2 expr_type alloc_expr name bld in
  insert name (Variable load_expr) symbols;
  load_expr
;;

let proc_block context f l = List.map (f context) l |> List.rev |> List.hd

let rec walk (context : Symbols.t) = function
  | (Int _ | Binop _) as v -> build_binop v
  | (Bool _ | Logicop _) as v -> build_logicop context walk v
  | String v -> build_global_stringptr (Scanf.unescaped v) "glbstr" bld
  | Symbol v -> get_var v
  | Call (n, l) -> build_call context walk n l
  | If (cond, ifb, elseb) -> build_if context walk cond ifb elseb
  | Defun (name, args, expr) -> build_fn walk name args expr
  | Typedef (name, types) ->
    proc_typedef name types;
    const_int i32_t 0
  | Defparam (name, e) -> build_defparam context walk name e
  | Block v -> proc_block context walk v
  | Defvar (name, e) -> build_variable context walk name e
;;

(* | _ -> failwith "[ERROR] maybe i need to impl it..." *)

let program f o =
  let context = Symbols.return main_ty main_fn main_entry in
  let v = List.map (walk context) o in
  let _ = ignore (build_ret (v |> List.rev |> List.hd) bld) in
  write_mod f the_mod;
  ()
;;
