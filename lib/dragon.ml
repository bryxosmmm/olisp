open Llvm
open Ast
open Symbols
module StringMap = Map.Make (String)

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

let nonvariadic_fn name ret_ty arg_tys () =
  let fn_ty = function_type ret_ty arg_tys in
  let fn = Llvm.declare_function name fn_ty the_mod in
  fn_ty, fn
;;

let declare_printf () =
  let ty = var_arg_function_type i32_t [| pointer_type ctx |] in
  let fn = declare_function "printf" ty the_mod in
  Symbols.insert "printf" (Function (ty, fn)) symbols;
  Function (ty, fn)
;;

let is_variadic types =
  let rec aux = function
    | [] -> false
    | "..." :: _ -> true
    | _ :: t -> aux t
  in
  aux types
;;

let get_fn s =
  let sym = get s symbols in
  match sym with
  | Some (Function (ty, fn)) -> ty, fn
  | _ -> failwith "[ERROR:fn] incorrect symbol"
;;

let get_macro s =
  let sym = get s symbols in
  match sym with
  | Some (Macro (fields, tree)) -> fields, tree
  | _ -> failwith "[ERROR:macro] incorrect symbol"
;;

let get_var s =
  let sym = get s symbols in
  match sym with
  | Some (Variable v) -> v
  | _ -> failwith (Printf.sprintf "[ERROR:var] incorrect symbol %s" s)
;;

let get_typedef s =
  let sym = get s symbols in
  match sym with
  | Some (Typeof v) -> v
  | _ -> failwith "[ERROR:typedef] incorrect symbol"
;;

let get_type s =
  let sym = get s symbols in
  match sym with
  | Some (Type v) -> v
  | _ -> failwith "[ERROR:type] incorrect symbol"
;;

let lltype_of_string = function
  | "I32" -> i32_t
  | "I8" -> i8_t
  | "String" -> pointer_type ctx
  | "Bool" -> bool_t
  | "Void" -> void_type ctx
  | t -> get_type t
;;

let rec build_binop f = function
  | Binop ('+', a, b) ->
    let lhs = build_binop f a in
    let rhs = build_binop f b in
    build_add lhs rhs "binopout" bld
  | Binop ('-', a, b) ->
    let lhs = build_binop f a in
    let rhs = build_binop f b in
    build_sub lhs rhs "binopout" bld
  | Binop ('*', a, b) ->
    let lhs = build_binop f a in
    let rhs = build_binop f b in
    build_mul lhs rhs "binopout" bld
  | Binop ('/', a, b) ->
    let lhs = build_binop f a in
    let rhs = build_binop f b in
    build_sdiv lhs rhs "binopout" bld
  | Binop _ -> failwith "[TODO:binop] i need to impl it..."
  | v -> f v
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

let build_fn_call ({ entry; _ } as context) f name l =
  let _ = position_at_end entry bld in
  let args = List.map (f context) l |> Array.of_list in
  let ty, fn = get_fn name in
  let v = build_call ty fn args "calltmp" bld in
  v
;;

let build_fn f name args expr =
  let types = get_typedef name in
  let ty, fn =
    nonvariadic_fn name (List.hd types) (types |> List.tl |> List.rev |> Array.of_list) ()
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

let proc_externdef alias name types =
  let filtred = List.filter (fun x -> not (String.equal "..." x)) types in
  let return_t = filtred |> List.rev |> List.hd |> lltype_of_string in
  let body_t =
    filtred
    |> List.rev
    |> List.tl
    |> List.rev
    |> List.map lltype_of_string
    |> Array.of_list
  in
  let ty, fn =
    if is_variadic types
    then variadic_fn name return_t body_t ()
    else nonvariadic_fn name return_t body_t ()
  in
  Symbols.insert
    (if String.equal alias "" then name else alias)
    (Function (ty, fn))
    symbols
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
  let load_expr = build_load expr_type alloc_expr name bld in
  insert name (Variable load_expr) symbols;
  load_expr
;;

let build_structdef name fields =
  let body_t = fields |> List.map (fun (_, x) -> lltype_of_string x) |> Array.of_list in
  let struct_type = named_struct_type ctx name in
  struct_set_body struct_type body_t false;
  let _ =
    if is_opaque struct_type
    then Printf.printf "struct %s is opaque\n" name
    else Printf.printf "struct %s is not opaque\n" name
  in
  Symbols.insert name (Type struct_type) symbols;
  struct_type
;;

let store_fields ty alloca vls =
  List.mapi
    (fun i (name, v) ->
       let ssa = build_struct_gep ty alloca i name bld in
       ssa, v)
    vls
  |> List.iter (fun (ssa, v) -> ignore @@ build_store v ssa bld)
;;

let create_struct struct_t vls =
  Printf.printf "is opaque: %b\n" (is_opaque struct_t);
  let a = build_alloca struct_t "structalloca" bld in
  store_fields struct_t a vls;
  let loaded = build_load struct_t a "structload" bld in
  ignore @@ build_ret loaded bld;
  ()
;;

let make_struct_fn name fields struct_t =
  let fun_name = name ^ "-make" in
  let fty = List.map (fun (_, x) -> x) fields in
  let fnames = List.map (fun (x, _) -> x) fields in
  let ty, fn =
    nonvariadic_fn
      fun_name
      struct_t
      (fty |> List.map lltype_of_string |> Array.of_list)
      ()
  in
  let values =
    List.mapi
      (fun i a ->
         let p = param fn i in
         set_value_name a p;
         a, p)
      fnames
  in
  let bb = append_block ctx "entry" fn in
  position_at_end bb bld;
  create_struct struct_t values;
  Symbols.insert fun_name (Function (ty, fn)) symbols
;;

let get_struct_field struct_v struct_t field_t i name =
  let ptr = build_struct_gep struct_t struct_v i "field_ptr" bld in
  build_load field_t ptr name bld
;;

let get_comp_struct name s_ty f_ty i =
  let ty, fn = nonvariadic_fn name f_ty [| s_ty |] () in
  let bb = append_block ctx "entry" fn in
  position_at_end bb bld;
  let s_val = param fn 0 in
  set_value_name "obj" s_val;
  let s_valptr = build_alloca s_ty "struct_val" bld in
  let _ = build_store s_val s_valptr bld in
  let res = get_struct_field s_valptr s_ty f_ty i "loaded" in
  ignore @@ build_ret res bld;
  insert name (Function (ty, fn)) symbols
;;

let build_getters name fields struct_t =
  List.iteri
    (fun i (field_n, field_st) ->
       let field_t = lltype_of_string field_st in
       get_comp_struct (name ^ "-" ^ field_n) struct_t field_t i)
    fields
;;

let call_printf fmt v =
  let ty, fn =
    get_or "printf" declare_printf symbols
    |> function
    | Function (t, f) -> t, f
    | _ -> failwith "its not gonna happen"
  in
  build_call
    ty
    fn
    (build_global_stringptr fmt "glbstr" bld :: v |> Array.of_list)
    "printf_call"
    bld
;;

let display_struct_fn name fields struct_t =
  let fun_name = name ^ "-show" in
  let ty, fn = nonvariadic_fn fun_name i32_t [| struct_t |] () in
  let bb = append_block ctx "entry" fn in
  position_at_end bb bld;
  let struct_v = build_alloca struct_t "struct_v" bld in
  let param_v = param fn 0 in
  set_value_name "obj" param_v;
  let _ = build_store param_v struct_v bld in
  let vls =
    List.mapi
      (fun i (field_n, field_st) ->
         let field_t = lltype_of_string field_st in
         get_struct_field struct_v struct_t field_t i field_n)
      fields
  in
  build_ret (call_printf "%d %d %d %d\n" vls) bld |> ignore;
  insert fun_name (Function (ty, fn)) symbols
;;

let proc_block context f l = List.map (f context) l |> List.rev |> List.hd

let build_int i ts =
  let t = lltype_of_string ts in
  const_int t i
;;

let replace_ast env name v =
  match StringMap.find_opt name env with
  | None -> v
  | Some vv -> vv
;;

let rec walk_macro_ast context f env = function
  | Symbol v as value -> replace_ast env v value
  | If (a, b, c) ->
    let cond = walk_macro_ast context f env a in
    let lhs = walk_macro_ast context f env b in
    let rhs = walk_macro_ast context f env c in
    If (cond, lhs, rhs)
  | other -> other
;;

let call_macro context f name exprs =
  let fields, ast_block = get_macro name in
  List.iter (Printf.printf "%s\n") fields;
  let env =
    List.fold_right2 (fun n e acc -> StringMap.add n e acc) fields exprs StringMap.empty
  in
  walk_macro_ast context f env ast_block |> f context
;;

let rec walk (context : Symbols.t) = function
  | Int (v, ts) -> build_int v ts
  | Binop _ as v -> build_binop (walk context) v
  | (Bool _ | Logicop _) as v -> build_logicop context walk v
  | String v -> build_global_stringptr (Scanf.unescaped v) "glbstr" bld
  | Symbol v -> get_var v
  | Call (n, l) -> build_fn_call context walk n l
  | If (cond, ifb, elseb) -> build_if context walk cond ifb elseb
  | Defun (name, args, expr) -> build_fn walk name args expr
  | Typedef (name, types) ->
    proc_typedef name types;
    const_int i32_t 0
  | Externdef (alias, name, types) ->
    proc_externdef alias name types;
    const_int i32_t 0
  | Defparam (name, e) -> build_defparam context walk name e
  | Block v -> proc_block context walk v
  | Defvar (name, e) -> build_variable context walk name e
  | Struct (name, fields) ->
    let ty = build_structdef name fields in
    make_struct_fn name fields ty;
    display_struct_fn name fields ty;
    build_getters name fields ty;
    const_int i32_t 0
  | Macrodef (name, args, expr) ->
    insert name (Macro (args, expr)) symbols;
    const_int i32_t 0
  | Macrouse (name, e) -> call_macro context walk name e
;;

let program f o =
  let context = Symbols.return main_ty main_fn main_entry in
  let v = List.map (walk context) o in
  let _ = ignore (build_ret (v |> List.rev |> List.hd) bld) in
  write_mod f the_mod;
  ()
;;
