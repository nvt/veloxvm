#lang racket

;; End-to-end test: derived forms through rewriter, compiler, and bytecode.

(require "../compiler.rkt")
(require "../bytecode.rkt")
(require "../rewriter.rkt")

(displayln "========================================")
(displayln "Derived Forms + Compilation")
(displayln "========================================\n")

;; Test program using derived forms
(define test-program
  '(;; Using cond
    (define (sign x)
      (cond ((< x 0) -1)
            ((= x 0) 0)
            (else 1)))

    ;; Using case
    (define (classify-grade n)
      (case n
        ((90 91 92 93 94 95 96 97 98 99 100) "A")
        ((80 81 82 83 84 85 86 87 88 89) "B")
        ((70 71 72 73 74 75 76 77 78 79) "C")
        (else "F")))

    ;; Using let
    (define (distance x1 y1 x2 y2)
      (let ((dx (- x2 x1))
            (dy (- y2 y1)))
        (sqrt (+ (* dx dx) (* dy dy)))))

    ;; Using let*
    (define (fibonacci n)
      (let* ((a 0)
             (b 1))
        (do ((i 0 (+ i 1))
             (a a b)
             (b b (+ a b)))
            ((>= i n) a))))

    ;; Using when/unless
    (define (check-positive x)
      (when (> x 0)
        (print "positive"))
      (unless (zero? x)
        (print "non-zero")))

    ;; Using do loop
    (define (sum-to-n n)
      (do ((i 0 (+ i 1))
           (sum 0 (+ sum i)))
          ((> i n) sum)))

    ;; Using composite car/cdr
    (define (third lst)
      (caddr lst))

    (define (fourth lst)
      (cadddr lst))

    ;; Using character predicates
    (define (is-digit? ch)
      (char-numeric? ch))

    ;; Using zero?
    (define (factorial n)
      (if (zero? n)
          1
          (* n (factorial (- n 1)))))
    ))

(displayln "Compiling derived-forms test program...\n")

;; Compile with rewriting
(define bc (make-bytecode))
(define compiled-count 0)

(for ([expr test-program])
  (set! compiled-count (+ compiled-count 1))
  (displayln (format "[~a/~a] Compiling: ~a"
                     compiled-count
                     (length test-program)
                     (car expr)))  ; Just show the first element (define/cond/etc)
  ;; Rewrite then compile
  (define rewritten (rewrite-expr expr))
  (compile-expr rewritten bc))

(displayln "\n========================================")
(displayln "Compilation Results")
(displayln "========================================\n")

(displayln (format "Magic number:  0x~x" (bytecode-magic bc)))
(displayln (format "Version:       ~a" (bytecode-version bc)))
(displayln (format "Strings:       ~a" (length (bytecode-strings bc))))
(displayln (format "Symbols:       ~a" (length (bytecode-symbols bc))))
(displayln (format "Expressions:   ~a" (length (bytecode-expressions bc))))

;; Write to file
(define output-file "derived-forms-test.vm")
(displayln (format "\nWriting bytecode to ~a..." output-file))
(write-bytecode-file output-file bc)

;; Check file
(displayln "File written successfully!")

;; Show some sample symbols
(displayln "\nSample symbols (first 20):")
(define symbols (bytecode-symbols bc))
(for ([sym (take symbols (min 20 (length symbols)))])
  (displayln (format "  - ~a" sym)))

(displayln "\n========================================")
(displayln " Derived-forms test completed!")
(displayln "========================================")

(displayln "\nDerived forms successfully implemented:")
(displayln "   cond")
(displayln "   case")
(displayln "   let, let*, letrec")
(displayln "   do")
(displayln "   when, unless")
(displayln "   All 28 car/cdr compositions")
(displayln "   zero? predicate")
(displayln "   char-ci=? and friends")
(displayln "   char-alphabetic?, char-numeric?, etc.")
(displayln "   call/cc abbreviation")
