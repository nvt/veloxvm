#lang racket

;; VeloxVM Racket Compiler - VM Primitive Definitions
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Defines VM types and primitive operations
;; Based on: include/vm-bytecode.h, core/vm-procedures.c

(provide (all-defined-out))

;; ============================================================================
;; VM Type Constants (from include/vm-objects.h)
;; ============================================================================

(define VM-TYPE-BOOLEAN   0)
(define VM-TYPE-INTEGER   1)
(define VM-TYPE-RATIONAL  2)
(define VM-TYPE-REAL      3)
(define VM-TYPE-STRING    4)
(define VM-TYPE-SYMBOL    5)
(define VM-TYPE-CHARACTER 6)
(define VM-TYPE-FORM      7)
(define VM-TYPE-LIST      8)
(define VM-TYPE-VECTOR    9)
(define VM-TYPE-PORT     10)
(define VM-TYPE-COMPLEX  11)
(define VM-TYPE-PROCEDURE 12)
(define VM-TYPE-EXTERNAL 13)
(define VM-TYPE-NONE     14)

;; ============================================================================
;; VM Form Types
;; ============================================================================

(define VM-FORM-INLINE 0)   ; Regular function call
(define VM-FORM-LAMBDA 1)   ; Lambda expression
(define VM-FORM-REF 2)      ; Variable reference

;; ============================================================================
;; Atom Type Bits
;; ============================================================================

(define ATOM-BIT 7)         ; Bit 7: 1 = atom, 0 = form

;; ============================================================================
;; VM Primitive Operations (from core/vm-procedures.c)
;; ============================================================================

;; VM Primitives in exact order matching vm-procedures.c operator table
;; This list MUST match the operator order exactly to ensure
;; correct primitive ID encoding. Total: 202 primitives.
(define vm-primitives
  '(+ - * / gcd lcm numerator denominator quotient remainder modulo
    = /= < <= > >= zero?
    bind bind_function return begin if define set! and or apply quote
    number? integer? rational? real? complex? exact? inexact? procedure?
    boolean? port? not eq? eqv? equal?
    system-info load-program import get-devices print random time
    get-programs program-info exit
    list cons push pop car cdr list-ref list-tail slice append remove reverse
    length null? list? pair? set-car! set-cdr! memq memv member
    assq assv assoc
    list-enumerate list-zip list-index
    map filter for-each reduce count
    char? char-compare char-class char->integer integer->char
    char-upcase char-downcase
    make-string string string? string-length string-ref
    string-set! string->list list->string vector->string string-fill!
    string-compare substring string-append string-copy string-split string-join
    number->string string->number
    guard raise
    thread-create! thread-fork! thread-id thread-join!
    thread-sleep! thread-specific thread-specific-set!
    thread-terminate! thread-yield! thread-stats
    mutex? make-mutex mutex-name mutex-specific mutex-specific-set!
    mutex-state mutex-lock! mutex-unlock!
    make-vector vector vector? buffer? vector-merge vector-length vector-ref
    vector-set! vector->list list->vector vector-fill! make-buffer
    buffer-append
    vector-for-each vector-count vector-fold vector-map
    input-port? output-port? current-input-port current-output-port
    open-input-file open-output-file close-input-port close-output-port
    read-char read peek-char eof-object? char-ready? write-char write
    display with-input-from-file with-output-to-file
    make-client make-server peer-name accept-client
    incoming-client? addr->string resolve-hostname
    floor ceiling round truncate exp log sin cos tan asin acos atan
    sqrt expt exact->inexact inexact->exact
    call-with-current-continuation values call-with-values dynamic-wind eval
    bit-and bit-or bit-invert bit-not bit-xor bit-shift
    construct-packet deconstruct-packet
    symbol?
    symbol->string
    box box-ref box-set!
    bind_function_rest
    string->symbol))

;; Check if symbol is a VM primitive
(define (vm-primitive? sym)
  (and (symbol? sym)
       (if (member sym vm-primitives) #t #f)))

;; Get primitive index (for symbol table)
(define (vm-primitive-index sym)
  (let loop ([prims vm-primitives] [idx 0])
    (cond
      [(null? prims) #f]
      [(eq? (car prims) sym) idx]
      [else (loop (cdr prims) (+ idx 1))])))
