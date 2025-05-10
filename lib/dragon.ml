open Llvm
open Ast

let ctx = create_context ()
let the_mod = create_module ctx "main_mod"
let i32_t = i32_type ctx
let bool_t = i1_type ctx
let main_ty = function_type i32_t [||]
let main_fn = declare_function "main" main_ty the_mod
let main_entry = append_block ctx "entry" main_fn
let bld = builder_at_end ctx main_entry
let write_mod f m = m |> string_of_llmodule |> output_string f

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

let rec build_binop = function
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

let build_logicop f = function
  | Bool true -> const_int bool_t 1
  | Bool false -> const_int bool_t 0
  | Logicop (_, a, b) as v ->
    let lhs = f a in
    let rhs = f b in
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

let build_call f name l =
  let args = List.map f l |> Array.of_list in
  let ty, fn = fn_of_string name in
  let v = build_call2 ty fn args "calltmp" bld in
  v
;;

let build_if f cond_expr if_expr else_expr =
  let cond_val = f cond_expr in
  let zero = const_int bool_t 0 in
  let cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" bld in
  let then_bb = append_block ctx "then" main_fn in
  let else_bb = append_block ctx "else" main_fn in
  let merge_bb = append_block ctx "merge" main_fn in
  ignore (build_cond_br cond_bool then_bb else_bb bld);
  position_at_end then_bb bld;
  let then_val = f if_expr in
  ignore (build_br merge_bb bld);
  let then_bb_end = insertion_block bld in
  position_at_end else_bb bld;
  let else_val = f else_expr in
  ignore (build_br merge_bb bld);
  let else_bb_end = insertion_block bld in
  position_at_end merge_bb bld;
  let phi = build_phi [ then_val, then_bb_end; else_val, else_bb_end ] "iftmp" bld in
  phi
;;

let rec walk = function
  | (Int _ | Binop _) as v -> build_binop v
  | (Bool _ | Logicop _) as v -> build_logicop walk v
  | String v -> build_global_stringptr v "glbstr" bld
  | Call (n, l) -> build_call walk n l
  | If (cond, ifb, elseb) -> build_if walk cond ifb elseb
;;

(* | _ -> failwith "[ERROR] maybe i need to impl it..." *)

let program f o =
  let v = walk o in
  let _ = ignore (build_ret v bld) in
  write_mod f the_mod;
  ()
;;
