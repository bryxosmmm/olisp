(extern display : printf (String -> ... -> I32))
(struct Color (r I32) (g I32) (b I32) (a I32))

(let blue (Color-make 123 0 255 255))
(Color-show blue)
(let r (Color-r blue))
(display "Red component: %d\n" r)
()


