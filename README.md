# O'lisp 

O'lisp is compiller for lisp language, written in ocaml.

## Can i do...
1) Libc functions, yes
2) Binop, yes 
3) Logicop/if/else, yes
4) functions, yes
5) variables, not yet
6) macros, not yet

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
- [x] Functions 
- [ ] Variables (Working like args of functions, but not like variables)
- [ ] Macros
- [ ] My context for evaluating
