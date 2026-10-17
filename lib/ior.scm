;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Internet Object References
;;;


(define (marshall-ior ior)
  (serialize-object 'ior
                    (vector (ior-purpose ior)
                            (ior-uuid ior)
                            (ior-stub-interface ior)
                            (ior-reference ior)
                            (ior-values ior))))


(define (unmarshall-ior content)
  (unmarshall-proxy content))


(define-type-of-object ior
  purpose
  uuid
  stub-interface
  reference
  values)
  
  
(define (new-ior purpose uuid stub-interface reference values)
  (let ((ior
          (make-ior
            'ior
            #f
            purpose
            uuid
            stub-interface
            reference
            values)))
    (setup-ior ior)
    ior))


(define (setup-ior ior)
  (setup-object ior))


(define (ior-print ior output readably)
  (format output "~{{a} {s} {s} {s} {s} {s}}"
    (object-class ior)
    purpose
    uuid
    stub-interface
    reference
    values))
