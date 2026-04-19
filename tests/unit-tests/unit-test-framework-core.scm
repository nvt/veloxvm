;; VeloxVM Unit Test Framework - Core
;; Loads test framework modules in correct order
;; This file coordinates the three framework modules

;; NOTE: Platform adapters must define:
;; - running-on-veloxvm? (boolean)
;; - print (procedure) - for output
;; - assert-error (macro or procedure) - platform-specific exception handling

;; Load framework modules in dependency order:
;; 1. Assertions - Pure assertion logic (no dependencies)
;; 2. Reporter - Output formatting (depends on assertions for value->string)
;; 3. Runner - Test execution (depends on assertions and reporter)

(include "test-assertions.scm")
(include "test-reporter.scm")
(include "test-runner.scm")
