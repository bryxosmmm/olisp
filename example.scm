; (struct Color (r I32) (g I32) (b I32) (a I32))
;
; (let blue (Color-make 123 0 255 255))
; (Color-show blue)
; (let r (Color-r blue))
; (display "Red component: %d\n" r)
; (extern display : printf (String -> ... -> I32))
; (defmacro when (expr th el) (if expr th el))
; (when! (= 2 10) (display "cool") (display "that is not okay"))

(: f (I32 -> I32))
(defun f (a) if (= a 10) :ok :err)

(let result (f 10))
(if (= result :ok) (display "SUPER COOL!!") (display "its wrong... you dumb"))
