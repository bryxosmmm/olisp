# Olisp 
Olisp is compiller for lisp language, written in ocaml.

## Can i do...
1) Libc functions, yes
2) Binop, yes 
3) Logicop/if/else, yes
4) functions, yes
5) variables, yes
6) structs, yes
7) macros, not yet

## Examples
```lisp
(printf "Hello %s\n" "world")
```

```lisp
(if (= 1 1) (printf "true") (printf "false"))
```

```lisp
; Example of type system
(: greet (String -> Integer -> Integer)) 
(defun greet (name age) (printf "Hello, %s. Im %d years old\n" name age)) 

(: add (Integer -> Integer -> Integer)) 
(defun add (a b) (+ a b))

(greet "Sebastian" (add 2 2))
```
## TODO
- [ ] debug all this ...
