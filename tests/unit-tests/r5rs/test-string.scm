;; VeloxVM Unit Tests - String Operations (R5RS-Compliant)
;; Tests for R5RS string procedures: make-string, string, string?, string-length,
;; string-ref, string-set!, string->list, list->string, substring, string-append,
;; string-copy, number->string, string->number

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "String Operations (R5RS)")

;; Make-string (R5RS §6.3.5)
(assert-equal "zzzzz" (make-string 5 #\z) "Make-string with fill character")
(assert-equal 10 (string-length (make-string 10 #\a)) "Make-string creates correct length")

;; String (from characters) (R5RS §6.3.5)
(assert-equal "Hej" (string #\H #\e #\j) "String from characters")
(assert-equal "A" (string #\A) "Single character string")
(assert-equal "" (string) "Empty string from no characters")

;; String? (R5RS §6.3.5)
(assert-true (string? "hello") "String is a string")
(assert-true (string? "") "Empty string is a string")
(assert-false (string? 123) "Number is not a string")
(assert-false (string? '(a b c)) "List is not a string")
(assert-false (string? #\a) "Character is not a string")

;; String-length (R5RS §6.3.5)
(assert-equal 5 (string-length "hello") "Length of 'hello'")
(assert-equal 0 (string-length "") "Length of empty string")
(assert-equal 7 (string-length "ooooooo") "Length of 7 characters")

;; String-ref (R5RS §6.3.5)
(assert-equal #\h (string-ref "hello" 0) "First character")
(assert-equal #\l (string-ref "hello" 2) "Middle character")
(assert-equal #\o (string-ref "hello" 4) "Last character")

;; String-set! (R5RS §6.3.5)
(define test-str (string-copy "hello"))
(string-set! test-str 0 #\H)
(assert-equal "Hello" test-str "String-set! modifies character")

;; String->list (R5RS §6.3.5)
(assert-equal '(#\a #\b #\c) (string->list "abc") "String to list")
(assert-equal '() (string->list "") "Empty string to list")

;; List->string (R5RS §6.3.5)
(assert-equal "abc" (list->string '(#\a #\b #\c)) "List to string")
(assert-equal "" (list->string '()) "Empty list to string")

;; Substring (R5RS §6.3.5)
(assert-equal "3456" (substring "0123456789" 3 7) "Substring from middle")
(assert-equal "0123456789" (substring "0123456789" 0 10) "Full string as substring")
(assert-equal "" (substring "hello" 2 2) "Empty substring")
(assert-equal "hel" (substring "hello" 0 3) "Substring from start")

;; String-append (R5RS §6.3.5)
(assert-equal "hello world" (string-append "hello" " " "world") "Append multiple strings")
(assert-equal "abcdefg" (string-append "a" "b" "cd" "efg") "Append various lengths")
(assert-equal "hello" (string-append "hello") "Append single string")
(assert-equal "" (string-append "" "") "Append empty strings")

;; String-copy (R5RS §6.3.5)
(define original "hello")
(define copied (string-copy original))
(assert-equal "hello" copied "String-copy creates equal string")
(string-set! copied 0 #\H)
(assert-equal "hello" original "Original unchanged after modifying copy")
(assert-equal "Hello" copied "Copy was modified")

;; Number->string (R5RS §6.3.5)
(assert-equal "123" (number->string 123) "Integer to string")
(assert-equal "-456" (number->string -456) "Negative integer to string")
(assert-equal "0" (number->string 0) "Zero to string")
(assert-equal "ff" (number->string 255 16) "Integer to hex string")
(assert-equal "11111111" (number->string 255 2) "Integer to binary string")

;; String->number (R5RS §6.3.5)
(assert-equal 123 (string->number "123") "String to integer")
(assert-equal -456 (string->number "-456") "String to negative integer")
(assert-equal 0 (string->number "0") "String '0' to zero")
(assert-equal 255 (string->number "ff" 16) "Hex string to integer")
(assert-equal 255 (string->number "11111111" 2) "Binary string to integer")
(assert-false (string->number "not-a-number") "Invalid string returns #f")

;; String equality tests (using equal?)
(assert-true (equal? "hello" "hello") "Equal strings")
(assert-false (equal? "hello" "Hello") "Case sensitive comparison")
(assert-false (equal? "hello" "world") "Different strings")
(assert-true (equal? "" "") "Empty strings are equal")

;; String comparison with string predicates
(assert-true (equal? (string #\H #\e #\j) "Hej") "Constructed string equals literal")

(test-summary)
