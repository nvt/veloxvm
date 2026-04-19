#lang racket

;; VeloxVM Racket Compiler - Utility Functions
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(provide gensym-counter
         fresh-symbol
         verbose-print
         debug-print)

;; Gensym counter for unique variable generation
(define gensym-counter 0)

;; Generate fresh symbol
(define (fresh-symbol [prefix 'tmp])
  (set! gensym-counter (+ gensym-counter 1))
  (string->symbol (format "~a~a" prefix gensym-counter)))

;; Verbose printing
(define (verbose-print fmt . args)
  (when (verbose-mode)
    (apply printf (cons fmt args))))

;; Debug printing
(define (debug-print fmt . args)
  (when (debug-mode)
    (apply printf (cons fmt args))))

;; Parameters (defined in main.rkt)
(define verbose-mode (make-parameter #f))
(define debug-mode (make-parameter #f))
