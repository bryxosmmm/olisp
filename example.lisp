; Example of type system
(: greet (String -> Integer -> Integer)) 
(defun greet (name age) (printf "Hello, %s. Im %d years old\n" name age)) 

(: add (Integer -> Integer -> Integer)) 
(defun add (a b) (+ a b))

(greet "Sebastian" (add 2 2))
