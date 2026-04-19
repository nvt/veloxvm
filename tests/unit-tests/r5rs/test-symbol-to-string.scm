;; VeloxVM Unit Tests - R5RS symbol->string
;; Tests for: R5RS §6.3.3 (symbol->string)

(include "../unit-test-framework.scm")

(test-suite "symbol->string Function")

(assert-equal "foo" (symbol->string 'foo) "Convert symbol foo to string")
(assert-equal "bar" (symbol->string 'bar) "Convert symbol bar to string")
(assert-equal "hello-world" (symbol->string 'hello-world) "Convert hyphenated symbol")
(assert-equal "+" (symbol->string '+) "Convert operator symbol")
(assert-equal "list" (symbol->string 'list) "Convert list symbol")

(test-summary)
