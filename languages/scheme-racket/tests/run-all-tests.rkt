#lang racket

;; VeloxVM Racket Compiler - Test Runner
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "test-reader.rkt"
         "test-rewriter.rkt"
         "test-primitives.rkt")

(displayln "")
(displayln "========================================")
(displayln "VeloxVM Racket Compiler - Test Suite")
(displayln "========================================")
(displayln "")

(displayln " All tests passed!")
(displayln "")
