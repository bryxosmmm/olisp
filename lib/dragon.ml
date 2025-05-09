open Llvm
open Ast

let ctx = create_context ()
let the_mod = create_module ctx "main_mod"
let i32_t = i32_type ctx
let bool_t = i1_type ctx
let write_mod f = the_mod |> string_of_llmodule |> output_string f

let main_entry () =
  let main_ty = function_type i32_t [||] in
  let main_fn = declare_function "main" main_ty the_mod in
  let main_entry = append_block ctx "entry" main_fn in
  builder_at_end ctx main_entry, main_ty, main_fn
;;

let main_builder, main_ty, main_fn = main_entry ()

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

let get_fn_vals = function
  | "printf" -> def_printf ()
  | "putchar" -> def_putchar ()
  | _ -> failwith "[TODO] not processed extern function"
;;

let rec construct_binexpr builder = function
  | Num n -> const_int i32_t n
  | Add (a, b) ->
    let lhs = construct_binexpr builder a in
    let rhs = construct_binexpr builder b in
    build_add lhs rhs "resbexpr" builder
  | Minus (a, b) ->
    let lhs = construct_binexpr builder a in
    let rhs = construct_binexpr builder b in
    build_sub lhs rhs "resbexpr" builder
  | Times (a, b) ->
    let lhs = construct_binexpr builder a in
    let rhs = construct_binexpr builder b in
    build_mul lhs rhs "resbexpr" builder
  | Div (a, b) ->
    let lhs = construct_binexpr builder a in
    let rhs = construct_binexpr builder b in
    build_sdiv lhs rhs "resbexpr" builder
;;

let construct_fn_call builder (ty, fn) vars =
  let values =
    List.map
      (fun x ->
         match x with
         | Int i -> const_int i32_t i
         | String s -> build_global_stringptr s "fmt" builder
         | VarBinExpr e -> construct_binexpr builder e)
      vars
  in
  let _ = build_call2 ty fn (Array.of_list values) "calltmp" builder in
  ()
;;

let construct_expr builder bb merge_bb o =
  let _ = position_at_end bb builder in
  (match o with
   | LibcExpr (name, vars) -> construct_fn_call builder (get_fn_vals name) vars
   | BinExpr v ->
     let _ = construct_binexpr builder v in
     ()
   | _ -> failwith "[TODO] not processed expression");
  let _ = build_br merge_bb builder in
  ()
;;

let construct_logic_stmt _ = function
  | BooleanLiteral true -> const_int i32_t 1
  | BooleanLiteral false -> const_int i32_t 0
;;

let construct_if_stmt builder fn (logic, expr_if, expr_else) =
  let then_bb = append_block ctx "then" fn in
  let else_bb = append_block ctx "else" fn in
  let merge_bb = append_block ctx "merge" fn in
  let cond =
    build_icmp
      Icmp.Eq
      (construct_logic_stmt builder logic)
      (const_int i32_t 1)
      "cond"
      builder
  in
  let _ = build_cond_br cond then_bb else_bb builder in
  let _ = construct_expr builder then_bb merge_bb expr_if in
  let _ = construct_expr builder else_bb merge_bb expr_else in
  let _ = position_at_end merge_bb builder in
  let _ = build_ret (const_int i32_t 0) builder in
  ()
;;

let program f o =
  (match o with
   | LibcExpr (name, vars) -> construct_fn_call main_builder (get_fn_vals name) vars
   | BinExpr v ->
     let _ = construct_binexpr main_builder v in
     ()
   | IfExpr v -> construct_if_stmt main_builder main_fn v);
  write_mod f
;;
