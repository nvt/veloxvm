#lang racket

;; Test bytecode file writing

(require "../compiler.rkt")
(require "../bytecode.rkt")

(displayln "Testing bytecode file writing...\n")

;; Create a simple program
(define program
  '((define (square x) (* x x))
    (square 5)))

(displayln "Compiling program:")
(for ([expr program])
  (displayln (format "  ~a" expr)))

;; Compile the program
(define bc (make-bytecode))
(for ([expr program])
  (compile-expr expr bc))

(displayln "\nCompiled bytecode:")
(displayln (format "  Magic: 0x~x" (bytecode-magic bc)))
(displayln (format "  Version: ~a" (bytecode-version bc)))
(displayln (format "  Strings: ~a" (bytecode-strings bc)))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc)))
(displayln (format "  Expressions: ~a entries" (length (bytecode-expressions bc))))

;; Write to file
(define output-file "test-output.vm")
(displayln (format "\nWriting to ~a..." output-file))
(write-bytecode-file output-file bc)

;; Check file was created
(displayln (format "File size: ~a bytes" (file-size output-file)))
(displayln "\nBytecode file writing test completed!")
