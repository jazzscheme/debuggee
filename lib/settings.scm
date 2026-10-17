;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Settings
;;;


;;;
;;;; Arguments
;;;


(define (switch? arg)
  (and (fx> (string-length arg) 0)
       (eqv? (string-ref arg 0) #\-)))


(define (switch-name arg)
  (let ((len (string-length arg)))
    (let ((start (if (and (fx>= len 2) (equal? (substring arg 0 2) "--"))
                     2
                   1)))
      (substring arg start len))))


(define (command-arguments)
  (cdr (command-line)))


(define (command-argument name #!key (error? #t))
  (let ((all (command-arguments)))
    (let iter ((arguments all))
         (if (null? arguments)
             #f
           (let ((arg (car arguments)))
             (cond ((or (not (switch? arg))
                        (null? (cdr arguments)))
                    (if error?
                        (error "Unable to parse command line" all)
                      #f))
                   ((equal? name (switch-name arg))
                    (cadr arguments))
                   (else
                    (iter (cddr arguments)))))))))


;;;
;;;; Parameters
;;;


(define (parse-parameter arg arg-parser setting setting-parser #!optional (default (unspecified)))
  (let ((arg-value (and arg (command-argument arg))))
    (if arg-value
        (arg-parser arg-value)
      (if (specified? default)
          default
        (error "Mandatory parameter not found" arg setting)))))
