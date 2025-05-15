; Example of type system

; (defparameter username "Sebastian")
(defparameter default 0)

(: greet (Integer -> Integer)) 
(defun greet (a) (+ a (greet (- a 1))))

; (: add (Integer -> Integer -> Integer)) 
; (defun add (a b) (- (+ a b) 100))

(greet 5)
