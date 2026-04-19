;; VeloxVM Unit Tests - Character Operations (R5RS-Compliant)
;; Tests for R5RS character procedures: char?, char->integer, integer->char,
;; char-upcase, char-downcase, char=?, char<?, char>?, char<=?, char>=?,
;; char-ci=?, char-ci<?, char-ci>?, char-ci<=?, char-ci>=?

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Character Operations (R5RS)")

;; Char? (R5RS §6.3.4)
(assert-true (char? #\a) "Character is a character")
(assert-true (char? #\A) "Uppercase character is a character")
(assert-true (char? #\Space) "Space is a character")
(assert-true (char? #\Newline) "Newline is a character")
(assert-true (char? #\0) "Digit character is a character")
(assert-false (char? "a") "String is not a character")
(assert-false (char? 65) "Integer is not a character")
(assert-false (char? '()) "Empty list is not a character")

;; Char->integer (R5RS §6.3.4)
(assert-equal 65 (char->integer #\A) "A is ASCII 65")
(assert-equal 97 (char->integer #\a) "a is ASCII 97")
(assert-equal 48 (char->integer #\0) "0 is ASCII 48")
(assert-equal 32 (char->integer #\Space) "Space is ASCII 32")
(assert-equal 10 (char->integer #\Newline) "Newline is ASCII 10")

;; Integer->char (R5RS §6.3.4)
(assert-equal #\A (integer->char 65) "ASCII 65 is A")
(assert-equal #\a (integer->char 97) "ASCII 97 is a")
(assert-equal #\0 (integer->char 48) "ASCII 48 is 0")
(assert-equal #\Space (integer->char 32) "ASCII 32 is space")
(assert-equal #\Newline (integer->char 10) "ASCII 10 is newline")

;; Round trip: char -> integer -> char
(assert-equal #\Z (integer->char (char->integer #\Z)) "Round trip for Z")
(assert-equal #\! (integer->char (char->integer #\!)) "Round trip for !")

;; Char-upcase (R5RS §6.3.4)
(assert-equal #\A (char-upcase #\a) "Upcase lowercase a")
(assert-equal #\Z (char-upcase #\z) "Upcase lowercase z")
(assert-equal #\A (char-upcase #\A) "Upcase already uppercase")
(assert-equal #\0 (char-upcase #\0) "Upcase digit unchanged")

;; Char-downcase (R5RS §6.3.4)
(assert-equal #\a (char-downcase #\A) "Downcase uppercase A")
(assert-equal #\z (char-downcase #\Z) "Downcase uppercase Z")
(assert-equal #\a (char-downcase #\a) "Downcase already lowercase")
(assert-equal #\0 (char-downcase #\0) "Downcase digit unchanged")

(test-suite "Character Comparison Predicates (R5RS)")

;; char=? (R5RS §6.3.4)
(assert-true (char=? #\a #\a) "a equals a")
(assert-false (char=? #\a #\b) "a does not equal b")
(assert-false (char=? #\a #\A) "a does not equal A (case sensitive)")

;; char<? (R5RS §6.3.4)
(assert-true (char<? #\a #\b) "a < b")
(assert-false (char<? #\b #\a) "b not < a")
(assert-false (char<? #\a #\a) "a not < a")

;; char<=? (R5RS §6.3.4)
(assert-true (char<=? #\a #\b) "a <= b")
(assert-true (char<=? #\a #\a) "a <= a")
(assert-false (char<=? #\b #\a) "b not <= a")

;; char>? (R5RS §6.3.4)
(assert-true (char>? #\b #\a) "b > a")
(assert-false (char>? #\a #\b) "a not > b")
(assert-false (char>? #\a #\a) "a not > a")

;; char>=? (R5RS §6.3.4)
(assert-true (char>=? #\b #\a) "b >= a")
(assert-true (char>=? #\a #\a) "a >= a")
(assert-false (char>=? #\a #\b) "a not >= b")
(assert-false (char>=? #\D #\c) "D >= c (D is 68, c is 99, so this is false in ASCII)")

;; Case insensitive comparisons (R5RS §6.3.4)
(assert-true (char-ci=? #\E #\e) "E equals e (case insensitive)")
(assert-true (char-ci=? #\a #\A) "a equals A (case insensitive)")
(assert-true (char-ci>=? #\D #\c) "D >= c (case insensitive)")

(test-suite "Special Characters (R5RS)")

;; Test special character literals
(assert-true (char? #\Tab) "Tab is a character")
(assert-true (char? #\Return) "Return is a character")

;; Whitespace characters
(assert-equal 9 (char->integer #\Tab) "Tab is ASCII 9")
(assert-equal 13 (char->integer #\Return) "Return is ASCII 13")

;; Character ranges
(define (char-between? c low high)
  (and (char>=? c low) (char<=? c high)))

(assert-true (char-between? #\m #\a #\z) "m is between a and z")
(assert-true (char-between? #\M #\A #\Z) "M is between A and Z")
(assert-true (char-between? #\5 #\0 #\9) "5 is between 0 and 9")

(test-summary)
