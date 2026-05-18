;;; ============================================================================
;;; VeloxVM R5RS I/O Runtime Library
;;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;;
;;; R5RS-compliant I/O procedures implemented in Scheme
;;; Requires: open-input-file, open-output-file, close-input-port,
;;;           close-output-port
;;; ============================================================================

;; -----------------------------------------------------------------------------
;; File Operations with Cleanup
;; R5RS §6.6.1
;; -----------------------------------------------------------------------------

;; call-with-input-file: Opens filename for input, calls proc with the port,
;; closes the port, and returns the result from proc.
;; NOTE: This implementation closes the port after normal completion.
;; It does NOT guarantee cleanup on exceptions (requires dynamic-wind).
(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (let ((value (proc port)))
      (close-input-port port)
      value)))

;; call-with-output-file: Opens filename for output, calls proc with the port,
;; closes the port, and returns the result from proc.
;; NOTE: This implementation closes the port after normal completion.
;; It does NOT guarantee cleanup on exceptions (requires dynamic-wind).
(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (let ((value (proc port)))
      (close-output-port port)
      value)))

;;; End of r5rs-io.scm
