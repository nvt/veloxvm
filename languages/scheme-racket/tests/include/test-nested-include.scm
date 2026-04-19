;; Test nested includes
(include "nested-lib.scm")

(print "z = " z #\Newline)
(print "square(z) = " (square z) #\Newline)
