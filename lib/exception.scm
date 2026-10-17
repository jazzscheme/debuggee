;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Exception
;;;


;;;
;;;; Exception
;;;


(define *exception-debugger*
  #f)


(define (get-exception-debugger)
  *exception-debugger*)

(define (set-exception-debugger exception-debugger)
  (set! *exception-debugger* exception-debugger))


(define current-exception-debugger
  (make-parameter #f))


(define (exception-debugger-hook exc other)
  (let ((exception-debugger (active-exception-debugger)))
    (if exception-debugger
        (exception-debugger exc)
      (system-exception-hook exc other))))


(define (with-exception-debugger exception-debugger thunk)
  (parameterize ((current-exception-debugger exception-debugger))
    (thunk)))


(define (active-exception-debugger)
  (or (current-exception-debugger) *exception-debugger*))


(define (system-exception-debugger exc)
  (invoke-exception-hook system-exception-hook exc))


(define (with-system-exception-debugger thunk)
  (with-exception-debugger system-exception-debugger
    thunk))


(define (catch-exception-filter filter catcher thunk)
  (let ((previous-handler (current-exception-handler)))
    (continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (if (with-exception-handler
                  (lambda (filter-exc)
                    (continuation-graft catcher-cont
                      (lambda ()
                        (previous-handler filter-exc))))
                  (lambda ()
                    (filter exc)))
                (continuation-graft catcher-cont
                  (lambda ()
                    (catcher exc)))
              (previous-handler exc)))
          thunk)))))


(define (call-with-catch predicate catcher thunk)
  (catch-exception-filter
    predicate
    catcher
    thunk))
