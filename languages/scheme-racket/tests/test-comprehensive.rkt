#lang racket

;; Comprehensive test of the VeloxVM Racket Compiler

(require "../compiler.rkt")
(require "../bytecode.rkt")
(require racket/file)

(displayln "========================================")
(displayln "VeloxVM Racket Compiler - Comprehensive Test")
(displayln "========================================\n")

;; Test program with various Scheme features
(define test-program
  '(;; Simple arithmetic
    (+ 1 2)

    ;; Variable definition
    (define pi 3.14159)

    ;; Function definition (syntax sugar)
    (define (factorial n)
      (if (= n 0)
          1
          (* n (factorial (- n 1)))))

    ;; Function definition (explicit lambda)
    (define square (lambda (x) (* x x)))

    ;; Conditional
    (if (> 5 3)
        "yes"
        "no")

    ;; Begin (sequence)
    (begin
      (define x 10)
      (define y 20)
      (+ x y))

    ;; And/Or
    (and #t #f)
    (or #f #t)

    ;; Set!
    (set! x 42)

    ;; Lambda application
    ((lambda (a b) (+ a b)) 10 20)

    ;; Nested expressions
    (+ (* 2 3) (/ 10 2))
    ))

(displayln "Compiling test program...")
(displayln (format "Total expressions: ~a\n" (length test-program)))

;; Compile the program
(define bc (make-bytecode))
(define compiled-count 0)

(for ([expr test-program])
  (set! compiled-count (+ compiled-count 1))
  (displayln (format "[~a/~a] ~a"
                     compiled-count
                     (length test-program)
                     expr))
  (compile-expr expr bc))

(displayln "\n========================================")
(displayln "Compilation Results")
(displayln "========================================\n")

(displayln (format "Magic number:  0x~x" (bytecode-magic bc)))
(displayln (format "Version:       ~a" (bytecode-version bc)))
(displayln (format "Strings:       ~a" (length (bytecode-strings bc))))
(displayln (format "  Content:     ~a" (bytecode-strings bc)))
(displayln (format "Symbols:       ~a" (length (bytecode-symbols bc))))
(displayln (format "  Content:     ~a" (bytecode-symbols bc)))
(displayln (format "Expressions:   ~a" (length (bytecode-expressions bc))))

;; Write to file
(define output-file "comprehensive-test.vm")
(displayln (format "\nWriting bytecode to ~a..." output-file))
(write-bytecode-file output-file bc)

(displayln "File written successfully")

;; Show hex dump of first 256 bytes
(displayln "\nFirst 256 bytes of output file:")
(system (format "hexdump -C ~a | head -16" output-file))

(displayln "\n========================================")
(displayln " Comprehensive test completed successfully!")
(displayln "========================================")
