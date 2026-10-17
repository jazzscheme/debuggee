;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Register
;;;


(define-type-of-object register
  objects)


(define (new-register)
  (let ((register
          (make-register
            'register
            #f
            (make-table test: eq?))))
    (setup-register register)
    register))


(define (setup-register register)
  (setup-object register))


(define (register-find-object register name)
  (table-ref (register-objects register) name #f))


(define (register-require-object register name)
  (or (register-find-object register name)
      (error "Unable to find in register" name)))


(define (register-load-object register module-name name)
  (load-unit module-name)
  (require-object register name))


(define (register-load-reference register reference)
  (if (symbol? reference)
      (require-object register reference)
    (bind (module-name name) reference
      (load-object register module-name name))))


;;;
;;;; Registration
;;;


(define (register-register-object register name object #!key (error? #t))
  (if (register-find-object register name)
      (if error?
          (error "Object is already registered" name)
        #f)
    (begin
      (table-set! (register-objects register) name object)
      #t)))


(define (register-unregister-object register name #!key (error? #t))
  (if (not (register-find-object register name))
      (if error?
          (error "Object is not registered" name)
        #f)
    (begin
      (table-clear (register-objects register) name)
      #t)))


;;;
;;;; Manifest
;;;


(define (register-manifest register)
  (table-keys (register-objects register)))
