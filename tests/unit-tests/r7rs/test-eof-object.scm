;;; VeloxVM Unit Tests - R7RS EOF object
;;;
;;; R7RS §6.13.2 requires the EOF object to be a disjoint type. Before
;;; this fix, VeloxVM aliased EOF to (char->integer 0), so binary
;;; reads could not tell a literal NUL byte from end of stream. Now
;;; eof-object? recognises only the dedicated VM_TYPE_EOF, and
;;; (eof-object) constructs one.

(include "../unit-test-framework.scm")

(test-suite "R7RS EOF object")

;; The constructor returns an EOF object that satisfies the predicate.
(assert-equal #t (eof-object? (eof-object))
              "(eof-object?) recognises (eof-object)")

;; #\nul is *not* an EOF object — this is the regression test for the
;; original aliasing bug.
(assert-equal #f (eof-object? #\nul)
              "#\\nul is not an EOF object")

;; Other types are also not EOF.
(assert-equal #f (eof-object? 0)             "0 is not EOF")
(assert-equal #f (eof-object? #f)            "#f is not EOF")
(assert-equal #f (eof-object? "")            "empty string is not EOF")
(assert-equal #f (eof-object? '())           "() is not EOF")
(assert-equal #f (eof-object? 'eof)          "the symbol 'eof is not EOF")

;; The EOF object is disjoint from every other type.
(assert-equal #f (char?    (eof-object))     "EOF is not a character")
(assert-equal #f (integer? (eof-object))     "EOF is not an integer")
(assert-equal #f (boolean? (eof-object))     "EOF is not a boolean")
(assert-equal #f (string?  (eof-object))     "EOF is not a string")
(assert-equal #f (symbol?  (eof-object))     "EOF is not a symbol")
(assert-equal #f (pair?    (eof-object))     "EOF is not a pair")
(assert-equal #f (null?    (eof-object))     "EOF is not the empty list")
(assert-equal #f (vector?  (eof-object))     "EOF is not a vector")
(assert-equal #f (procedure? (eof-object))   "EOF is not a procedure")

(test-summary)
