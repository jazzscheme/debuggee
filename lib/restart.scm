;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Restart
;;;


(define-type-of-object debuggee-restart
  thread
  restart)


(define (new-debuggee-restart thread restart)
  (let ((debuggee-restart
          (make-debuggee-restart
            'debuggee-restart
            #f
            thread
            restart)))
    (setup-debuggee-restart debuggee-restart)
    debuggee-restart))


(define (setup-debuggee-restart debuggee-restart)
  (setup-object debuggee-restart))


(define (debuggee-restart-get-id debuggee-restart)
  (object->serial (debuggee-restart-restart debuggee-restart)))


(define (debuggee-restart-get-name debuggee-restart)
  (restart-name (debuggee-restart-restart debuggee-restart)))


(define (debuggee-restart-get-message debuggee-restart)
  (restart-message (debuggee-restart-restart debuggee-restart)))
