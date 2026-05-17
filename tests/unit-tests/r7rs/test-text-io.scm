;;; VeloxVM Unit Tests - R7RS §6.13 textual block I/O on ports
;;;
;;; Exercises read-string, write-string, and read-line against
;;; in-memory string ports.

(include "../unit-test-framework.scm")

(test-suite "R7RS textual block I/O")

;; --- read-string -------------------------------------------------------
(define ip (open-input-string "hello world"))
(assert-equal "hello" (read-string 5 ip) "read-string returns first k chars")
(assert-equal " "     (read-string 1 ip) "read-string advances past prior call")
(assert-equal "world" (read-string 10 ip)
              "read-string returns short tail when fewer than k remain")
(assert-equal #t (eof-object? (read-string 1 ip))
              "read-string returns eof when source exhausted")

(define empty-ip (open-input-string ""))
(assert-equal "" (read-string 0 empty-ip)
              "read-string with k=0 returns empty string at EOF")
(assert-equal #t (eof-object? (read-string 1 empty-ip))
              "read-string with k>0 on empty source returns eof")

;; read-string returns a string, not a list of characters
(assert-equal #t (string? (read-string 3 (open-input-string "abc")))
              "read-string yields a string")

;; --- write-string ------------------------------------------------------
(define op (open-output-string))
(write-string "abc" op)
(write-string "" op)
(write-string "DEF" op)
(assert-equal "abcDEF" (get-output-string op)
              "write-string appends chars in order")

;; write-string with start
(define op2 (open-output-string))
(write-string "ignore-this-prefix" op2 7)
(assert-equal "this-prefix" (get-output-string op2)
              "write-string with start skips a prefix")

;; write-string with start and end
(define op3 (open-output-string))
(write-string "abcdefghij" op3 2 7)
(assert-equal "cdefg" (get-output-string op3)
              "write-string with start/end writes the slice")

;; write-string with start == end writes nothing
(define op4 (open-output-string))
(write-string "ignored" op4 3 3)
(assert-equal "" (get-output-string op4)
              "write-string with start=end writes nothing")

;; --- read-line ---------------------------------------------------------
;; Mixed line endings: LF, CRLF, lone CR, trailing EOF without terminator.
(define lp (open-input-string "one\ntwo\r\nthree\rfour"))
(assert-equal "one"   (read-line lp) "read-line on LF-terminated line")
(assert-equal "two"   (read-line lp) "read-line on CRLF-terminated line")
(assert-equal "three" (read-line lp) "read-line on CR-terminated line")
(assert-equal "four"  (read-line lp) "read-line on tail without terminator")
(assert-equal #t (eof-object? (read-line lp))
              "read-line returns eof after last line consumed")

;; Empty line between two non-empty lines.
(define lp2 (open-input-string "a\n\nb\n"))
(assert-equal "a" (read-line lp2) "read-line first non-empty line")
(assert-equal "" (read-line lp2) "read-line returns empty string for blank line")
(assert-equal "b" (read-line lp2) "read-line next non-empty line")
(assert-equal #t (eof-object? (read-line lp2))
              "read-line returns eof after trailing newline consumed")

;; Source with only a terminator -- one empty line then EOF.
(define lp3 (open-input-string "\n"))
(assert-equal "" (read-line lp3)
              "read-line on single-newline source returns empty string")
(assert-equal #t (eof-object? (read-line lp3))
              "read-line then returns eof")

;; Empty source -- immediate EOF.
(define lp4 (open-input-string ""))
(assert-equal #t (eof-object? (read-line lp4))
              "read-line on empty source returns eof immediately")

;; CR alone at end of stream is a terminator.
(define lp5 (open-input-string "alone\r"))
(assert-equal "alone" (read-line lp5)
              "read-line strips trailing CR even without LF")
(assert-equal #t (eof-object? (read-line lp5))
              "read-line then EOF after CR-only terminator")

(test-summary)
