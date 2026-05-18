;;; ============================================================================
;;; VeloxVM R7RS features Procedure
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small `features` returns a list of symbols naming feature
;;; identifiers the implementation supports. Used with cond-expand to
;;; gate code by capability.
;;;
;;; The list below names features actually present in this Scheme
;;; today; expand it as new features ship. Keep entries lowercase and
;;; hyphenated to match R7RS conventions.
;;; ============================================================================

(define (features)
  '(veloxvm
    r5rs
    r7rs-subset
    exact-closed
    ratios))

;;; End of r7rs-features.scm
