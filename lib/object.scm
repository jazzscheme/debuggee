;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Objects
;;;


(define-type object
  extender: define-type-of-object
  class
  methods)


(define (setup-object object)
  (object-methods-set! object (make-table)))


(define (add-method object method-name method)
  (table-set! (object-methods object) method-name method))


(define (dispatch object method-name)
  (let ((method (table-ref (object-methods object) method-name #f)))
    (if (not method)
        (error "Unable to find method" method-name)
      method)))
