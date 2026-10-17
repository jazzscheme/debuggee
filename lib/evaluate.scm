;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Evaluation
;;;


(define (evaluate-forms syntax str container line col evaluation-mode #!key (file #f) (readtable #f) (evaluate? #t) (scripting? #f))
  (define (toplevel-form? expr)
    (and (pair? (source-code expr))
         (memq (source-code (car (source-code expr))) '(unit module script))))
  
  (define (parse-toplevel expr)
    (let ((kind (source-code (car (source-code expr))))
          (expr (cdr (source-code expr))))
      (let ((first (source-code (car expr))))
        (if (memq first '(protected public))
            (values kind (source-code (cadr expr)) first)
          (values kind first 'public)))))
  
  (define (present result)
    (when (specified? result)
      (let ((values (call-with-values (lambda () result) list)))
        (for-each (lambda (value)
                    (format ':console "\2$\3\2color Ochre {s}\3{%}" value))
                  values))
      (display-prompt (current-console-port) (current-repl-level))
      (force-output (current-console-port))))
  
  (let ((forms ;; hack around the worker giving me a -1 line that was
               ;; probably a hack around gambit's first line being 1!?
               (let ((line (max 0 line)))
                 ;; hack around what seems to be a change in the semantics
                 ;; of ##input-port-line-set! and ##input-port-column-set!
                 (let ((str (string-append (make-string line #\newline) (make-string col #\space) str))
                       (line 0)
                       (col 0))
                   (call-with-input-string str
                     (lambda (port)
                       (read-source-all port container line col)))))))
    (let ((first (car forms)))
      (if (and (eq? syntax 'jazz) (toplevel-form? first) (null? (cdr forms)))
          (let ((expression first))
            (receive (kind unit-name access) (parse-toplevel first)
              (if (not evaluate?)
                  (expand-form expression)
                (present (eval expression)))))
          (let ((expression (cons 'begin forms)))
            (present (eval expression)))))))
