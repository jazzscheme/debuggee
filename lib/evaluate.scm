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
  
  (define (sibling-sld-file container)
    (and (string? container)
         (let ((len (string-length container)))
           (and (>= len 4)
                (string=? (substring container (- len 4) len) ".scm")
                (let ((sld-path (string-append (substring container 0 (- len 4)) ".sld")))
                  (and (file-exists? sld-path)
                       sld-path))))))

  (define (extract-sld-info sld-path)
    (with-exception-catcher
      (lambda (exc)
        #f)
      (lambda ()
        (call-with-input-file sld-path
          (lambda (port)
            (let loop ((name #f)
                       (imports '())
                       (exports '()))
              (let ((expr (read port)))
                (if (eof-object? expr)
                    (and name (values name (reverse imports) (reverse exports)))
                  (if (and (pair? expr)
                           (eq? (car expr) 'define-library)
                           (pair? (cdr expr)))
                      (let ((lib-name (cadr expr)))
                        (let scan-clauses ((clauses (cddr expr))
                                           (cur-imports imports)
                                           (cur-exports exports))
                          (if (null? clauses)
                              (loop lib-name cur-imports cur-exports)
                            (let ((clause (car clauses)))
                              (cond ((and (pair? clause) (eq? (car clause) 'import))
                                     (scan-clauses (cdr clauses) (cons clause cur-imports) cur-exports))
                                    ((and (pair? clause) (eq? (car clause) 'export))
                                     (scan-clauses (cdr clauses) cur-imports (cons clause cur-exports)))
                                    (else
                                     (scan-clauses (cdr clauses) cur-imports cur-exports)))))))
                    (loop name imports exports))))))))))

  (define (library-name->string name)
    (let loop ((parts name) (strs '()))
      (if (null? parts)
          (apply string-append (reverse strs))
        (let ((part (car parts)))
          (let ((part-str (if (symbol? part) (symbol->string part) (number->string part))))
            (if (null? strs)
                (loop (cdr parts) (list part-str))
              (loop (cdr parts) (cons part-str (cons "/" strs)))))))))

  (define (library-name->module-sym name)
    (string->symbol (library-name->string name)))

  (define (library-name->namespace-str name)
    (string-append (library-name->string name) "#"))

  (define (normalize-imports imports)
    ;; Extracts each import-set from (import ...) forms, emitting (##import gambit)
    ;; for gambit or (gambit) and standard (import ...) for all other libraries.
    (let loop ((clauses imports) (res '()))
      (if (null? clauses)
          (reverse res)
        (let ((clause (car clauses)))
          (if (and (pair? clause) (eq? (car clause) 'import))
              (let inner ((sets (cdr clause)) (inner-res res))
                (if (null? sets)
                    (loop (cdr clauses) inner-res)
                  (let ((s (car sets)))
                    (if (or (eq? s 'gambit) (equal? s '(gambit)))
                        (inner (cdr sets) (cons '(##import gambit) inner-res))
                      (inner (cdr sets) (cons (if (pair? s) `(import ,s) `(import (,s))) inner-res))))))
            (loop (cdr clauses) res))))))

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
          (let ((expression (cons 'begin forms))
                (sld-path (sibling-sld-file container)))
            (if sld-path
                (receive (name imports exports) (extract-sld-info sld-path)
                  (if name
                      (let* ((mod-sym (library-name->module-sym name))
                             (ns-str (library-name->namespace-str name))
                             (expanded-imports (normalize-imports imports))
                             (library-expression
                               `(begin
                                  (declare (separate))
                                  (##demand-module ,mod-sym)
                                  (##namespace (,ns-str))
                                  ,@expanded-imports
                                  ,@forms
                                  (##namespace (""))))
                             (isolated-cte (##top-cte-clone ##interaction-cte)))
                        (present (##eval-top library-expression isolated-cte)))
                    (present (eval expression))))
              (present (eval expression))))))))
