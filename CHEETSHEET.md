Awesome — I’ll create a detailed and stylish LLVM-in-OCaml cheat sheet, focusing on mimicking C language constructs (like if/else, loops, structs, functions, and some advanced topics) purely in IR generation, and format it all as a Markdown file for readability and sharing. I’ll get started and let you know when it's ready!


# LLVM IR Codegen with OCaml

**LLVM IR** is a typed, **Static Single Assignment (SSA)** IR: once you create a value, it never changes. LLVM code is built inside a **module** (`Llvm.llmodule`) using an **IRBuilder** (`Llvm.llbuilder`) to insert instructions at the current point. Typically you start with:

```ocaml
let ctx    = Llvm.global_context ()
let the_mod = Llvm.create_module ctx "MyModule"
let builder = Llvm.builder ctx
```

The module holds all functions/globals (the top-level IR container). The builder tracks the **insertion point** (current basic block). Each **function** has basic blocks of instructions, ending in a terminator (`ret`, `br`, etc.). Because LLVM is SSA, you use *instructions* to create new values; existing values never change. You must explicitly create *types* (unique per context) for everything (e.g. `Llvm.i32_type ctx` for 32-bit int). Types are uniqued in LLVM, so you “get” a type instead of allocating one.

## Defining and Calling Functions

You define functions by first creating a **function type** and then using `Llvm.define_function` (or `Llvm.declare_function` for an external prototype). For example, to make `int add(int x, int y)`:

```ocaml
let i32_t  = Llvm.i32_type ctx
let fn_ty  = Llvm.function_type i32_t [| i32_t; i32_t |] false
let add_fn = Llvm.define_function "add" fn_ty the_mod
```

Here `function_type ret arg_array vararg` builds a function type. (LLVM types are uniqued – you don’t “new” them.) To implement the function body, create an entry block and an IRBuilder position:

```ocaml
let entry_bb = Llvm.append_block ctx "entry" add_fn
Llvm.position_at_end entry_bb builder
(* Name parameters for IR *)
let x = Llvm.param add_fn 0 in Llvm.set_value_name "x" x
let y = Llvm.param add_fn 1 in Llvm.set_value_name "y" y
(* Example body: compute x+y and return *)
let sum = Llvm.build_add x y "sum" builder
ignore (Llvm.build_ret sum builder)
```

This generates LLVM IR like:

```llvm
define i32 @add(i32 %x, i32 %y) {
entry:
  %sum = add i32 %x, %y
  ret i32 %sum
}
```

*(The `define_function` call automatically creates an external declaration or a new function if none exists.)*.

To **call** a function, use `Llvm.build_call` with the callee `llvalue` and an array of argument values. For example:

```ocaml
let print_ty = Llvm.function_type i32 [| Llvm.pointer_type (Llvm.i8_type ctx) |] true
let printf = Llvm.declare_function "printf" print_ty the_mod
let fmt = Llvm.build_global_stringptr "%d\n\000" "fmt" builder
let one = Llvm.const_int i32_t 1 in let two = Llvm.const_int i32_t 2
ignore (Llvm.build_call printf [| fmt; one; two |] "calltmp" builder)
```

This would emit IR like:

```llvm
declare i32 @printf(i8*, ...)
%calltmp = call i32 @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"fmt", i32 0, i32 0), i32 1, i32 2)
```

The `build_call` instruction corresponds to `%x = call @func(args...)`. For function pointers, the same `build_call` works on a pointer-to-function value.

## Conditionals (if/else)

To implement an `if`-statement, you create new basic blocks for the **then** and **else** paths and a **merge** block. First, generate code for the condition (an `i1` boolean). Then use:

```ocaml
let then_bb  = Llvm.append_block ctx "then" the_fun
let else_bb  = Llvm.append_block ctx "else" the_fun
let merge_bb = Llvm.append_block ctx "ifcont" the_fun
ignore (Llvm.build_cond_br cond_val then_bb else_bb builder)
```

This emits a branch `br i1 %cond, label %then, label %else`. In the `then` block, position the builder at `then_bb`, emit instructions, then unconditionally branch to `merge_bb`. Do similarly for the `else` block. Finally, position at `merge_bb` and use a **phi-node** if needed to merge values from both paths:

```ocaml
Llvm.position_at_end then_bb builder
let then_val = ... codegen ... in
ignore (Llvm.build_br merge_bb builder)
Llvm.position_at_end else_bb builder
let else_val = ... codegen ... in
ignore (Llvm.build_br merge_bb builder)
Llvm.position_at_end merge_bb builder
let phi = Llvm.build_phi [ (then_val, then_bb); (else_val, else_bb) ] "iftmp" builder
```

This yields IR like:

```llvm
; <cond>% = icmp eq i32 %x, %y
br i1 %cond, label %then, label %else

then:
  ...          ; code for then
  br label %ifcont

else:
  ...          ; code for else
  br label %ifcont

ifcont:
  %iftmp = phi i32 [ %then_val, %then ], [ %else_val, %else ]
```

Here `%iftmp` picks the appropriate value depending on which block branched in. The `build_cond_br` and `build_phi` APIs handle these constructs.

## Loops

Loops are built similarly with branches. For a `while`-loop, for example, you might create a **loop** block and an **after** block:

```ocaml
(* Setup initial variables if any; then: *)
ignore (Llvm.build_br loop_bb builder)
Llvm.position_at_end loop_bb builder
let i_val = Llvm.build_load i_alloca "i_val" builder in
let cmp = Llvm.build_icmp Llvm.Icmp.Sle i_val (Llvm.const_int i32_t 10) "cond" builder
ignore (Llvm.build_cond_br cmp body_bb after_bb builder)
(* body_bb: *)
Llvm.position_at_end body_bb builder
... (* loop body code *) ...
let next_i = Llvm.build_add i_val (Llvm.const_int i32_t 1) "next" builder
ignore (Llvm.build_store next_i i_alloca builder);
ignore (Llvm.build_br loop_bb builder)
(* after_bb: ... continue ... *)
```

This pattern emits a branch back to the loop condition as long as it’s true. In more complex cases (like `for` loops), you often allocate the loop variable with `alloca`, store/update it each iteration, and branch accordingly. For example, Kaleidoscope’s OCaml tutorial uses `build_alloca` in the function entry and then `build_store`/`build_load` in the loop body to update the loop variable. In short, loops are built with the same branching logic as if/else, typically using a `phi` or a stack slot to carry loop-carried values.

## Local Variables and Stack

To create local (mutable) variables, you use **alloca** in the function’s entry block, then **store** to it and **load** from it. For example:

```ocaml
let x_alloc = Llvm.build_alloca i32_t "x" builder
ignore (Llvm.build_store (Llvm.const_int i32_t 42) x_alloc builder)
let x_val = Llvm.build_load x_alloc "x_val" builder
```

This emits LLVM IR:

```llvm
%x = alloca i32
store i32 42, i32* %x
%x_val = load i32, i32* %x
```

Using this pattern, each variable is a pointer on the stack. Kaleidoscope’s tutorial uses exactly this: it creates an `alloca` in the entry block for each variable and then emits `load`/`store` for reads/writes. Note that LLVM’s *mem2reg* optimization can later remove these loads/stores and put values into SSA registers, but at IR-generation time this is the common approach.

## Structs and Memory Access

LLVM lets you define **struct types** in two steps. First create an opaque named struct, then set its body. For example:

```ocaml
let my_struct = Llvm.named_struct_type ctx "MyStruct"
Llvm.struct_set_body my_struct 
  [| Llvm.i32_type ctx; Llvm.i8_type ctx |] false
```

This defines `%MyStruct = type { i32, i8 }`. You can also create anonymous structs with `Llvm.struct_type`. In LLVM IR this appears as:

```llvm
%MyStruct = type { i32, i8 }
```

Named struct types are added to the module when used. (For recursive structs you name it first, then fill in the body.) This two-phase approach is standard.

To access struct fields, first allocate or get a pointer to the struct, then use `getelementptr`. LLVM OCaml provides `build_struct_gep` for struct fields:

```ocaml
let s_alloc = Llvm.build_alloca my_struct "s" builder
let f0_ptr = Llvm.build_struct_gep s_alloc 0 "f0" builder
ignore (Llvm.build_store (Llvm.const_int (Llvm.i32_type ctx) 7) f0_ptr builder)
let f0_val = Llvm.build_load f0_ptr "f0val" builder
```

This emits IR like:

```llvm
%0 = alloca %MyStruct
%f0 = getelementptr %MyStruct, %MyStruct* %0, i32 0, i32 0
store i32 7, i32* %f0
%f0val = load i32, i32* %f0
```

*(The first `0` index in GEP means “the same object”, then `0` selects field 0.)*  For non-struct pointers or arrays, use the general `Llvm.build_gep`. For example, for a pointer `p`,

```ocaml
let elem_ptr = Llvm.build_gep p [| idx1; idx2 |] "elemptr" builder
```

creates `%elemptr = getelementptr %p, idx1, idx2`. The special `build_struct_gep p idx` is shorthand for `%= getelementptr %p, 0, idx`.

## Arrays and Pointers

To work with arrays, you can allocate an array type on the stack or use global arrays. For a fixed-size local array:

```ocaml
let arr_ty = Llvm.array_type i32_t 10
let arr_alloc = Llvm.build_alloca arr_ty "arr" builder
```

To index it, use GEP. LLVM arrays are zero-indexed, and the first GEP index is usually 0 to step into the array. For example, to get a pointer to `arr[3]`:

```ocaml
let zero = Llvm.const_int (Llvm.i32_type ctx) 0
let three = Llvm.const_int (Llvm.i32_type ctx) 3
let elt_ptr = Llvm.build_gep arr_alloc [| zero; three |] "elt" builder
ignore (Llvm.build_store (Llvm.const_int i32_t 99) elt_ptr builder)
let val3 = Llvm.build_load elt_ptr "val3" builder
```

This yields IR:

```llvm
%arr = alloca [10 x i32]
%elt = getelementptr [10 x i32], [10 x i32]* %arr, i32 0, i32 3
store i32 99, i32* %elt
%val3 = load i32, i32* %elt
```

Similarly, pointer arithmetic on arbitrary pointers is done with `build_gep` and constant indices. (There is also `Llvm.const_array` to create LLVM constant array values if you want to define a global initializer.)

## Advanced Topics

* **Function Pointers:** In LLVM, a function pointer is just a value of pointer-to-function type. You can cast or bitcast function pointers as needed. To call through a function pointer `fp`, just use `Llvm.build_call fp args name builder` as usual.

* **Switch Statements:** Use `Llvm.build_switch`. Example:

  ```ocaml
  (* Assume 'cond' is an i32 value, default_bb and case_bb are basic blocks *)
  let sw = Llvm.build_switch cond default_bb 2 builder in
  Llvm.add_case sw (Llvm.const_int i32_t 0) case0_bb;
  Llvm.add_case sw (Llvm.const_int i32_t 1) case1_bb;
  ```

  This creates an LLVM `switch` instruction with a default and 2 cases. See `Llvm.build_switch cond default_bb count builder`.

* **Tail Calls:** After emitting a call, you can mark it as a *tail call* to hint optimization:

  ```ocaml
  let call_insn = Llvm.build_call callee args "tcall" builder in
  Llvm.set_tail_call true call_insn
  ```

  This sets the `tail` marker on the call so the backend can optimize it if possible.

Each example above shows the OCaml LLVM-binding calls and the corresponding LLVM IR. The LLVM OCaml API (in `Llvm.*`) mirrors the C++ IRBuilder. The key functions include `Llvm.define_function`, `Llvm.build_*` (for instructions like `add`, `br`, `call`, etc.) and helpers like `Llvm.build_gep`/`build_struct_gep` for pointers and arrays. Using these, you can implement all common C-like constructs (functions, branches, loops, arithmetic, memory, etc.) by generating the appropriate LLVM IR constructs.
