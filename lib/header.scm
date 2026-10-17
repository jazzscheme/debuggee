;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel
;;;


(include "~~lib/_gambit#.scm")


;;;
;;;; Code
;;;


(define (code-cte c)
  (macro-code-cte c))


;;;
;;;; Environment
;;;


(define (rte-up r)
  (macro-rte-up r))


;;;
;;;; Various
;;;


(define (absent-object? obj)
  (##eq? obj (macro-absent-obj)))


(define (unbound-object? obj)
  (##eq? obj #!unbound))
