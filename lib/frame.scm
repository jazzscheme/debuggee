;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Frame
;;;


(define-type-of-object debuggee-frame
  id
  location
  continuation
  name
  hidden?)


(define (new-debuggee-frame id location continuation name hidden?)
  (let ((debuggee-frame
          (make-debuggee-frame
            'debuggee-frame
            #f
            id
            location
            continuation
            name
            hidden?)))
    (setup-debuggee-frame debuggee-frame)
    debuggee-frame))


(define (setup-debuggee-frame debuggee-frame)
  (setup-object debuggee-frame))


(define (debuggee-frame-get-id debuggee-frame)
  (debuggee-frame-id debuggee-frame))


(define (debuggee-frame-get-continuation debuggee-frame)
  (debuggee-frame-continuation debuggee-frame))


(define (debuggee-frame-get-name debuggee-frame)
  (debuggee-frame-name debuggee-frame))


(define (debuggee-frame-get-interpreted? debuggee-frame)
  (let ((cont (serial->object (debuggee-frame-continuation debuggee-frame))))
    (boolean (interpreted-continuation? cont))))


(define (debuggee-frame-get-hidden? debuggee-frame)
  (debuggee-frame-hidden? debuggee-frame))


;;;
;;;; Variables
;;;


(define (debuggee-frame-get-location debuggee-frame)
  ;; to test the stepper
  (or (debuggee-frame-location debuggee-frame)
      (let ((cont (serial->object (debuggee-frame-continuation debuggee-frame))))
        (get-continuation-location cont))))


(define (debuggee-frame-get-variables debuggee-frame kind)
  (let ((cont (serial->object (debuggee-frame-continuation debuggee-frame))))
    (let ((variables (case kind
                       ((:lexical) (get-continuation-lexical-environment cont))
                       ((:all) (append (get-continuation-lexical-environment cont)
                                       (get-continuation-dynamic-environment cont))))))
      (map (lambda (variable rank)
             (bind (name value mutable?) variable
               (cons (list #f name rank) (package-info value mutable?: mutable?))))
           variables
           (naturals 0 (length variables))))))
