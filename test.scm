#! gsi-script

(load "header")
(include "all.scm")

(start-presence purpose: 'debugging)

(setup-local-process)

(setup-debuggee)
(ready-debuggee)

(current-process-title-set! "Welcome to Scheme!")

(define (yo)
  12)

(define (foo x)
  (let ((z (+ x x)))
    (break)
    z))

(define (goo y)
  (foo (* y 2))
  3)

(define (hoo z)
  (goo (+ z 3))
  5)

#;
(thread-start!
  (make-thread
    (lambda ()
      (hoo 7))
    'zoo))

(start-repl)
