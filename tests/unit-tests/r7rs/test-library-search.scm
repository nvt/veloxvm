;;; VeloxVM Unit Tests - R7RS library search path
;;; (import (mathx)) is not defined in this file; it is loaded from
;;; mathx.sld in the same directory via the library search path (the
;;; source file's own directory is searched first). The loaded library is
;;; mangled and compiled into the program like an in-file library.

(include "../unit-test-framework.scm")

(import (mathx))

(test-suite "R7RS library search path")

(assert-equal 27 (cube 3)  "procedure from a file-loaded library")
(assert-equal 5  (halve 10) "second export from a file-loaded library")

(test-summary)
