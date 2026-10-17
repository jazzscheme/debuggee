;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Thread
;;;


(define-type-of-object debuggee-thread
  thread)


(define (new-debuggee-thread thread)
  (let ((debuggee-thread
          (make-debuggee-thread
            'debuggee-thread
            #f
            thread)))
    (setup-debuggee-thread debuggee-thread)
    debuggee-thread))


(define (setup-debuggee-thread debuggee-thread)
  (setup-object debuggee-thread))


(define (debuggee-thread-get-id debuggee-thread)
  (object->serial (debuggee-thread-thread debuggee-thread)))


(define (debuggee-thread-get-name debuggee-thread)
  (thread-name (debuggee-thread-thread debuggee-thread)))


(define (debuggee-thread-get-state debuggee-thread)
  (present-thread-state (thread-state (debuggee-thread-thread debuggee-thread))))


(define (debuggee-thread-get-priority debuggee-thread)
  (thread-base-priority (debuggee-thread-thread debuggee-thread)))


;; This is a hack to support continuation inspection. This should be reimplemented by either allowing
;; the debugger to directly inspect a continuation with a proper continuation stub without having to
;; create a temporary thread
(define (debuggee-thread-get-debugged-continuation? debuggee-thread)
  (eq? (thread-thread-group (debuggee-thread-thread debuggee-thread)) debugged-continuations-thread-group))


(define (debuggee-thread-get-stops debuggee-thread)
  (thread-active-stops (debuggee-thread-thread debuggee-thread)))


(define (debuggee-thread-set-repl-frame debuggee-thread frame-proxy)
  ;; todo
  #f)
