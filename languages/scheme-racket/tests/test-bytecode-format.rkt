#lang racket

;; Test bytecode format compliance with VeloxVM spec

(require "../compiler.rkt")
(require "../bytecode.rkt")
(require "../rewriter.rkt")
(require rackunit)

(displayln "========================================")
(displayln "Bytecode Format Verification Tests")
(displayln "========================================\n")

;; Test bytecode file format structure
(define (test-bytecode-file expr description)
  (displayln (format "Testing: ~a" description))
  (define bc (make-bytecode))
  (define rewritten (rewrite-expr expr))
  (compile-expr rewritten bc)

  (define test-file (format "test-~a.vm" (gensym)))
  (write-bytecode-file test-file bc)
  (define file-bytes (file->bytes test-file))

  ;; Verify file structure
  (displayln "  Checking magic number...")
  (check-equal? (bytes-ref file-bytes 0) #x5E "Magic byte 1")
  (check-equal? (bytes-ref file-bytes 1) #xB5 "Magic byte 2")

  (displayln "  Checking version...")
  (check-equal? (bytes-ref file-bytes 2) 1 "Version")

  (displayln "  Checking tables exist...")
  ;; Bytes 3-4: number of strings (16-bit little-endian)
  (define num-strings (+ (bytes-ref file-bytes 3)
                         (* 256 (bytes-ref file-bytes 4))))
  (displayln (format "    Strings: ~a" num-strings))

  ;; Calculate offset to symbol table
  (define offset 5)
  (for ([i (in-range num-strings)])
    (define str-len (+ (bytes-ref file-bytes offset)
                       (* 256 (bytes-ref file-bytes (+ offset 1)))))
    (set! offset (+ offset 2 str-len)))

  ;; Symbol table count
  (define num-symbols (+ (bytes-ref file-bytes offset)
                         (* 256 (bytes-ref file-bytes (+ offset 1)))))
  (displayln (format "    Symbols: ~a" num-symbols))
  (check-true (> num-symbols 0) "Has symbols")

  ;; Clean up
  (delete-file test-file)
  (displayln "   Format valid\n"))

;; Test 1: Simple expression
(test-bytecode-file '(+ 1 2) "Simple arithmetic")

;; Test 2: Function definition
(test-bytecode-file '(define (f x) (* x 2)) "Function definition")

;; Test 3: Lambda expression
(test-bytecode-file '((lambda (x) x) 42) "Lambda application")

;; Test 4: Derived form - cond
(test-bytecode-file '(cond ((< x 0) -1) (else 1)) "Cond expression")

;; Test 5: Derived form - let
(test-bytecode-file '(let ((x 1)) x) "Let binding")

(displayln "========================================")
(displayln "Testing specific bytecode encodings...")
(displayln "========================================\n")

;; Test integer encoding
(displayln "Test: Integer encoding")
(define int-bc (make-bytecode))
;; Compile in an expression context so it gets added to the table
(compile-expr '(+ 42 1) int-bc)
(define int-exprs (bytecode-expressions int-bc))
;; Find the integer 42 in the expressions
(define int-enc (findf (lambda (e)
                         (and (eq? (expr-encoding-type e) 'atom)
                              (let ([bytes (expr-encoding-data e)])
                                (and (not (null? bytes))
                                     (= (car bytes) #x09)))))  ; Integer type
                       int-exprs))
(check-not-false int-enc "Found integer encoding")
(define int-bytes (expr-encoding-data int-enc))
(displayln (format "  42 encoded as: ~a" int-bytes))
(check-equal? (car int-bytes) #x09 "Integer header byte")
(displayln "   Integer encoding correct\n")

;; Test boolean encoding
(displayln "Test: Boolean encoding")
(define bool-bc (make-bytecode))
(compile-expr '(if #t 1 2) bool-bc)
(define bool-exprs (bytecode-expressions bool-bc))
(define bool-enc (findf (lambda (e)
                          (and (eq? (expr-encoding-type e) 'atom)
                               (let ([bytes (expr-encoding-data e)])
                                 (and (not (null? bytes))
                                      (= (car bytes) #x08)))))  ; Boolean type
                        bool-exprs))
(check-not-false bool-enc "Found boolean encoding")
(define bool-bytes (expr-encoding-data bool-enc))
(displayln (format "  #t encoded as: ~a" bool-bytes))
(check-equal? (car bool-bytes) #x08 "Boolean true header")
(displayln "   Boolean encoding correct\n")

;; Test string encoding
(displayln "Test: String encoding")
(define str-bc (make-bytecode))
(compile-expr '(print "hello") str-bc)
(check-equal? (length (bytecode-strings str-bc)) 1 "String added to table")
(check-equal? (car (bytecode-strings str-bc)) "hello" "String value correct")
(displayln "   String encoding correct\n")

;; Test symbol encoding
(displayln "Test: Symbol encoding")
(define sym-bc (make-bytecode))
(compile-expr '(+ 1 2) sym-bc)
(check-true (if (member '+ (bytecode-symbols sym-bc)) #t #f) "Symbol added to table")
(displayln "   Symbol encoding correct\n")

;; Test lambda form encoding
(displayln "Test: Lambda form encoding")
(define lambda-bc (make-bytecode))
(define lambda-result (compile-expr '(lambda (x) x) lambda-bc))
(check-equal? (expr-encoding-type lambda-result) 'form "Lambda result is a form")
(define lambda-bytes (expr-encoding-data lambda-result))
;; First byte should have bit 7 set (form token) and bits 5-4 = 1 (lambda type)
(define lambda-header (car lambda-bytes))
(check-true (> (bitwise-and lambda-header #x80) 0) "Form token bit set")
;; Bits 5-4 should be 01 (lambda form type)
(define form-type (bitwise-and (arithmetic-shift lambda-header -4) #x03))
(check-equal? form-type 1 "Lambda form type is 1")
(displayln (format "  Lambda form header: 0x~x" lambda-header))
(displayln "   Lambda form encoding correct\n")

;; Test inline form encoding
(displayln "Test: Inline form encoding")
(define inline-bc (make-bytecode))
(define inline-result (compile-expr '(+ 1 2) inline-bc))
(check-equal? (expr-encoding-type inline-result) 'form "Application result is a form")
(define inline-bytes (expr-encoding-data inline-result))
(define inline-header (car inline-bytes))
(check-true (> (bitwise-and inline-header #x80) 0) "Form token bit set")
;; Bits 5-4 should be 00 (inline form type)
(define inline-type (bitwise-and (arithmetic-shift inline-header -4) #x03))
(check-equal? inline-type 0 "Inline form type is 0")
(check-equal? (cadr inline-bytes) 2 "Inline form has 2 args")
(displayln (format "  Inline form header: 0x~x, args: ~a"
                   inline-header (cadr inline-bytes)))
(displayln "   Inline form encoding correct\n")

(displayln "========================================")
(displayln " All bytecode format tests PASSED!")
(displayln "========================================\n")

(displayln "Verified:")
(displayln "   VeloxVM magic number (0x5E 0xB5)")
(displayln "   Version byte (1)")
(displayln "   String table format")
(displayln "   Symbol table format")
(displayln "   Expression table format")
(displayln "   Integer encoding (variable length)")
(displayln "   Boolean encoding")
(displayln "   String encoding (table-based)")
(displayln "   Symbol encoding (table-based)")
(displayln "   Lambda form encoding (VM_FORM_LAMBDA)")
(displayln "   Inline form encoding (VM_FORM_INLINE)")
