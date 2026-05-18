;; Test basic include functionality
(include "lib.scm")

;; Use definitions from included file
(print "x = " x #\Newline)
(print "y = " y #\Newline)
(print "add(x, y) = " (add x y) #\Newline)
