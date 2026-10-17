;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Stop
;;;


(define-type-of-object debuggee-stop
  thread
  kind
  reason
  detail
  location
  exception
  continuation
  frames
  restarts
  stack
  stepper)


(define (new-debuggee-stop thread kind reason detail exception continuation restarts locat stepper)
  (define max-depth
      1000)
  
  (define (collect-continuation-stack cont)
    (map (lambda (cont)
           (list cont (get-continuation-name cont) (boolean (hidden-frame? (##continuation-parent cont)))))
         (get-continuation-stack cont max-depth)))
  
  (define (stop-location stack)
    (let ((found (find-if (lambda (data) (not (caddr data))) stack)))
      (and found (exception-location exception (car found)))))
  
  (let ((stack (collect-continuation-stack continuation)))
    (let ((location (locat->container/line/col (or locat (stop-location stack)))))
      (let ((debuggee-stop
              (make-debuggee-stop
                'debuggee-stop
                #f
                thread
                kind
                reason
                detail
                location
                exception
                continuation
                #f
                restarts
                stack
                stepper)))
        (setup-debuggee-stop debuggee-stop)
        debuggee-stop))))


(define (setup-debuggee-stop debuggee-stop)
  (setup-object debuggee-stop))


(define (debuggee-stop-get-id debuggee-stop)
  (object->serial debuggee-stop))


(define (debuggee-stop-get-kind debuggee-stop)
  (debuggee-stop-kind debuggee-stop))


(define (debuggee-stop-get-reason debuggee-stop)
  (debuggee-stop-reason debuggee-stop))


(define (debuggee-stop-get-location debuggee-stop)
  (debuggee-stop-location debuggee-stop))


(define (debuggee-stop-get-detail debuggee-stop)
  (debuggee-stop-detail debuggee-stop))


(define (debuggee-stop-get-restarts debuggee-stop)
  (debuggee-stop-restarts debuggee-stop))


;;;
;;;; Frames
;;;


(define (debuggee-stop-get-frames debuggee-stop)
  (define (make-frame id frame)
    (bind (cont name hidden?) frame
      ;; to test the stepper
      (let ((location
              (and (debuggee-stop-stepper debuggee-stop) (= id 0)
                location)))
        (let ((frame (new-debuggee-frame id location (object->serial cont) name hidden?)))
          (new-debuggee-frame-local-proxy (require-presence 'debugging) frame)))))
  
  ;; gc protect
  (let ((stack (debuggee-stop-stack debuggee-stop)))
    (let ((frames
            (map (lambda (id frame)
                   (make-frame id frame))
                 (naturals 0 (length stack))
                 stack)))
      (debuggee-stop-frames-set! debuggee-stop frames)
      frames)))
