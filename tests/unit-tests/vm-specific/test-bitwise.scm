;; VeloxVM Unit Tests - Bitwise Operations
;; Tests for: bit-and, bit-or, bit-xor, bit-not, bit-invert, bit-shift

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Bitwise Operations")

;; Bit-and
(assert-equal #b00100 (bit-and #b10110 #b01101) "Bitwise AND binary")
(assert-equal 0 (bit-and #b11110000 #b00001111) "Bitwise AND with no common bits")
(assert-equal 255 (bit-and 255 255) "Bitwise AND same value")
(assert-equal 0 (bit-and 0 12345) "Bitwise AND with zero")
(assert-equal 8 (bit-and 15 8) "15 AND 8 = 8")

;; Bit-or
(assert-equal #b11111 (bit-or #b10110 #b01101) "Bitwise OR binary")
(assert-equal 255 (bit-or #b11110000 #b00001111) "Bitwise OR complementary")
(assert-equal 15 (bit-or 0 15) "Bitwise OR with zero")
(assert-equal #o645 (bit-or #o244 #o401) "Bitwise OR octal")
(assert-equal 10147463 (bit-or 10000000 1234567) "Bitwise OR large numbers")

;; Bit-xor
(assert-equal 98 (bit-xor 103 5) "Bitwise XOR 103 ^ 5")
(assert-equal #b11001 (bit-xor #b10110 #b01111) "Bitwise XOR binary")
(assert-equal 0 (bit-xor 255 255) "Bitwise XOR same value = 0")
(assert-equal 255 (bit-xor 0 255) "Bitwise XOR with zero")
(assert-equal #b111000 (bit-xor #b111111 #x7) "Bitwise XOR mixed notation")

;; Bit-not (logical NOT)
(assert-equal -101 (bit-not 100) "Bitwise NOT 100")
(assert-equal 894 (bit-not (bit-not 894)) "Double NOT returns original")
(assert-equal -1 (bit-not 0) "Bitwise NOT 0")
(assert-equal 0 (bit-not -1) "Bitwise NOT -1")

;; Bit-invert
(assert-equal -64 (bit-invert 63) "Bitwise invert 63")

;; Bit-shift (positive = left, negative = right)
(assert-equal 10 (bit-shift 5 1) "Left shift 5 by 1 = 10")
(assert-equal 2 (bit-shift 5 -1) "Right shift 5 by 1 = 2")
(assert-equal 20 (bit-shift 5 2) "Left shift 5 by 2 = 20")
(assert-equal 1 (bit-shift 5 -2) "Right shift 5 by 2 = 1")
(assert-equal 256 (bit-shift 1 8) "Left shift 1 by 8 = 256")
(assert-equal 1 (bit-shift 256 -8) "Right shift 256 by 8 = 1")
(assert-equal 0 (bit-shift 5 -3) "Right shift 5 by 3 = 0")
(assert-equal 5 (bit-shift 5 0) "Shift by 0 unchanged")

(test-suite "Bitwise - Number Literals")

;; Binary literals
(assert-equal 22 #b10110 "Binary literal 10110 = 22")
(assert-equal 13 #b01101 "Binary literal 01101 = 13")
(assert-equal 255 #b11111111 "Binary literal 11111111 = 255")

;; Octal literals
(assert-equal 164 #o244 "Octal literal 244 = 164")
(assert-equal 257 #o401 "Octal literal 401 = 257")
(assert-equal 8 #o10 "Octal literal 10 = 8")

;; Hexadecimal literals
(assert-equal 7 #x7 "Hex literal 7 = 7")
(assert-equal 255 #xff "Hex literal ff = 255")
(assert-equal 256 #x100 "Hex literal 100 = 256")
(assert-equal 65535 #xffff "Hex literal ffff = 65535")

(test-suite "Bitwise - Practical Examples")

;; Setting bits
(define (set-bit n bit)
  (bit-or n (bit-shift 1 bit)))
(assert-equal 5 (set-bit 1 2) "Set bit 2 of 1 = 5")
(assert-equal 9 (set-bit 1 3) "Set bit 3 of 1 = 9")

;; Clearing bits
(define (clear-bit n bit)
  (bit-and n (bit-not (bit-shift 1 bit))))
(assert-equal 1 (clear-bit 5 2) "Clear bit 2 of 5 = 1")
(assert-equal 4 (clear-bit 5 0) "Clear bit 0 of 5 = 4")

;; Testing bits
(define (test-bit n bit)
  (not (= 0 (bit-and n (bit-shift 1 bit)))))
(assert-true (test-bit 5 0) "Bit 0 of 5 is set")
(assert-false (test-bit 5 1) "Bit 1 of 5 is not set")
(assert-true (test-bit 5 2) "Bit 2 of 5 is set")

;; Toggling bits
(define (toggle-bit n bit)
  (bit-xor n (bit-shift 1 bit)))
(assert-equal 4 (toggle-bit 5 0) "Toggle bit 0 of 5 = 4")
(assert-equal 7 (toggle-bit 5 1) "Toggle bit 1 of 5 = 7")

;; Extract low byte
(define (low-byte n)
  (bit-and n #xff))
(assert-equal 52 (low-byte #x1234) "Low byte of 0x1234 = 0x34 = 52")
(assert-equal 255 (low-byte #xffff) "Low byte of 0xffff = 255")

;; Extract high byte of 16-bit
(define (high-byte n)
  (bit-and (bit-shift n -8) #xff))
(assert-equal 18 (high-byte #x1234) "High byte of 0x1234 = 0x12 = 18")
(assert-equal 255 (high-byte #xffff) "High byte of 0xffff = 255")

(test-summary)
