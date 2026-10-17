;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Syntax
;;;


(define-macro (library . rest)
  `(begin))


(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))


(##define-syntax bind
  (lambda (src)
    (define (generate-symbol #!optional (prefix "sym"))
      (let ((name (string-append prefix "^")))
        (##gensym (##string->uninterned-symbol name))))
    
    (define (source-code expr)
      (if (##source? expr)
          (##source-code expr)
        expr))
    
    (define (desourcify expr)
      (##desourcify expr))
    
    (define (expand-car bindings tree body)
      (let ((car-binding (car bindings)))
        (cond ((symbol? car-binding)
               `((let ((,car-binding (car ,tree)))
                   ,@(expand-cdr bindings tree body))))
              ((pair? car-binding)
               (let ((car-symbol (generate-symbol "car")))
                 `((if (null? ,tree)
                       (error "Unable to bind")
                     (let ((,car-symbol (car ,tree)))
                       ,@(expand-car car-binding car-symbol
                           (expand-cdr bindings tree body))))))))))
    
    (define (expand-cdr bindings tree body)
      (let ((cdr-binding (cdr bindings)))
        (cond ((null? cdr-binding)
               body)
              ((symbol? cdr-binding)
               `((let ((,cdr-binding (cdr ,tree)))
                   ,@body)))
              ((pair? cdr-binding)
               (let ((cdr-symbol (generate-symbol "cdr")))
                 `((let ((,cdr-symbol (cdr ,tree)))
                     ,@(expand-car cdr-binding cdr-symbol body))))))))
    
    (let ((form (cdr (source-code src))))
      (let ((bindings (desourcify (car form)))
            (tree (desourcify (cadr form)))
            (body (cddr form)))
        (let ((tree-symbol (generate-symbol "tree")))
          `(let ((,tree-symbol ,tree))
             ,@(expand-car bindings tree-symbol body)))))))


(##define-syntax catch
  (lambda (form-src)
    (define (source-code expr)
      (if (##source? expr)
          (##source-code expr)
        expr))
    
    (define (sourcify-deep-if expr src)
      (if (##source? src)
          (##sourcify-deep expr src)
        expr))
    
    (define (desourcify expr)
      (##desourcify expr))
    
    (if (null? (cdr (source-code form-src)))
        (error "Ill-formed catch")
      (let ((predicate/type (cadr (source-code form-src)))
            (body (cddr (source-code form-src))))
        (sourcify-deep-if
          (cond ((symbol? (source-code predicate/type))
                 `(call-with-catch ,predicate/type (lambda (exc) exc)
                    (lambda ()
                      ,@body)))
                ((pair? (source-code predicate/type))
                 `(call-with-catch ,(car (source-code predicate/type)) (lambda (,(source-code (cadr (source-code predicate/type)))) ,@(cddr (source-code predicate/type)))
                    (lambda ()
                      ,@body)))
                (else
                 (error "Ill-formed predicate/type in catch: {t}" (desourcify predicate/type))))
          form-src)))))
