;;; VeloxVM Unit Tests - R7RS string higher-order helpers
;;; Tests for: string-map, string-for-each.

(include "../unit-test-framework.scm")

;; Inline definitions from languages/scheme-racket/runtime/r7rs-strings.scm
(define (string-map proc str)
  (list->string (map proc (string->list str))))

(define (string-for-each proc str)
  (for-each proc (string->list str)))

(test-suite "R7RS string higher-order")

;; string-map: identity transform
(assert-equal "hello" (string-map (lambda (c) c) "hello")
              "string-map with identity returns the same string")

;; string-map: char-upcase-style transform via integer arithmetic
;; (avoid char-upcase to keep this independent of its availability)
(define (lower->upper c)
  (let ((n (char->integer c)))
    (if (and (>= n (char->integer #\a))
             (<= n (char->integer #\z)))
        (integer->char (- n 32))
        c)))

(assert-equal "HELLO" (string-map lower->upper "hello")
              "string-map upcasing ASCII letters")
(assert-equal "HELLO, WORLD!" (string-map lower->upper "hello, world!")
              "string-map preserves non-letters")

;; string-map: empty string is empty
(assert-equal "" (string-map (lambda (c) c) "")
              "string-map of empty string is empty string")

;; string-for-each: visits each char in order, side-effect via accumulator
(define visited '())
(string-for-each
  (lambda (c) (set! visited (cons c visited)))
  "abc")
(assert-equal '(#\c #\b #\a) visited
              "string-for-each visits chars left-to-right")

;; string-for-each: empty string does nothing
(define empty-visited 0)
(string-for-each
  (lambda (c) (set! empty-visited (+ empty-visited 1)))
  "")
(assert-equal 0 empty-visited
              "string-for-each of empty string makes no calls")

;; string-for-each: counts chars
(define char-count 0)
(string-for-each
  (lambda (c) (set! char-count (+ char-count 1)))
  "veloxvm")
(assert-equal 7 char-count
              "string-for-each visits every char exactly once")

(test-summary)
