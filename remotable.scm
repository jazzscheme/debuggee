;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Remotable Stub
;;;
;;;  The contents of this file are subject to the Mozilla Public License Version
;;;  1.1 (the "License"); you may not use this file except in compliance with
;;;  the License. You may obtain a copy of the License at
;;;  http://www.mozilla.org/MPL/
;;;
;;;  Software distributed under the License is distributed on an "AS IS" basis,
;;;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;;  for the specific language governing rights and limitations under the
;;;  License.
;;;
;;;  The Original Code is JazzScheme.
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;
;;;  Alternatively, the contents of this file may be used under the terms of
;;;  the GNU General Public License Version 2 or later (the "GPL"), in which
;;;  case the provisions of the GPL are applicable instead of those above. If
;;;  you wish to allow use of your version of this file only under the terms of
;;;  the GPL, and not to allow others to use your version of this file under the
;;;  terms of the MPL, indicate your decision by deleting the provisions above
;;;  and replace them with the notice and other provisions required by the GPL.
;;;  If you do not delete the provisions above, a recipient may use your version
;;;  of this file under the terms of any one of the MPL or the GPL.
;;;
;;;  See www.jazzscheme.org for details.


(##define-syntax remotable-stub
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
    
    (define (parse-remotable-stub rest)
      (let ((name (car rest))
            (body (cdr rest)))
        (values name body)))
    
    (define (parse-method-stub rest)
      (define (parse-passage rest)
        (if (memq (car rest) '(reference value))
            (values (car rest) (cdr rest))
          (values 'reference rest)))
      
      (let ((access (car rest))
            (invocation (cadr rest))
            (rest (cddr rest)))
        (receive (passage rest) (parse-passage rest)
          (let ((signature (car rest)))
            (let ((name (car signature))
                  (parameters (cdr signature)))
              (if (or (null? parameters)
                      (not (eq? (car parameters) 'self)))
                  (error "Missing self for method" name)
                (let ((parameters (cdr parameters)))
                  (values name access invocation passage parameters))))))))
    
    (define (expand-remotable-stub rest)
      (receive (remotable-name remotable-body) (parse-remotable-stub rest)
        (define (add name suffix)
          (string->symbol (string-append (symbol->string name) suffix)))
        
        (define (parse-parameters params)
          (let ((parameters '())
                (positional '()))
            (define (encode parameter)
              (string->symbol (string-append "__" (symbol->string parameter))))
            
            (let iter ((scan params))
                 (cond ((null? scan)
                        (values parameters positional #f))
                       ((symbol? scan)
                        (let ((rest (encode scan)))
                          (set! parameters (append parameters rest))
                          (values parameters positional rest)))
                       (else
                        (let ((parameter (encode (car scan))))
                          (set! parameters (append parameters (list parameter)))
                          (set! positional (append positional (list parameter)))
                          (iter (cdr scan))))))))
        
        (define (parse-value-keyword name passage)
          (case passage
            ((reference)
             #f)
            ((value)
             (let* ((str (symbol->string name))
                    (len (string-length str)))
               (if (not (and (> len 4) (string=? (substring str 0 4) "get-")))
                   (error "Ill-formed value")
                 (string->keyword (substring str 4 len)))))))
        
        (let ((local-class (add remotable-name "-local-proxy"))
              (local-make (string->symbol (string-append "make-" (symbol->string remotable-name) "-local-proxy")))
              (local-new (string->symbol (string-append "new-" (symbol->string remotable-name) "-local-proxy")))
              (local-setup (string->symbol (string-append "setup-" (symbol->string remotable-name) "-local-proxy")))
              (remote-class (add remotable-name "-remote-proxy"))
              (remote-make (string->symbol (string-append "make-" (symbol->string remotable-name) "-remote-proxy")))
              (remote-new (string->symbol (string-append "new-" (symbol->string remotable-name) "-remote-proxy")))
              (remote-setup (string->symbol (string-append "setup-" (symbol->string remotable-name) "-remote-proxy")))
              (defines '())
              (values '())
              (locals '())
              (remotes '()))
          (for-each (lambda (method-form)
                      (receive (name access invocation passage parameters) (parse-method-stub (cdr method-form))
                        (receive (parameters positional rest) (parse-parameters parameters)
                          (let ((invoker (case invocation ((post) 'post-remote) ((send) 'send-remote) ((call) 'call-remote)))
                                (dispatch (string->symbol (string-append (symbol->string name))))
                                (local-result (case invocation ((post send) '((unspecified))) ((call) '())))
                                (value-keyword (parse-value-keyword name passage)))
                            (set! defines (append defines (list `(define (,(add remotable-name (string-append "-proxy-" (symbol->string name))) proxy ,@parameters)
                                                                   ,@(cond (rest `((apply (dispatch proxy ',name) proxy ,@positional ,rest)))
                                                                           (else `(((dispatch proxy ',name) proxy ,@positional))))))))
                            (if value-keyword
                                (set! values (append values (list value-keyword `(,name self)))))
                            (set! locals (append locals (list #; `(method override (,name self ,@parameters)
                                                                 ,@(cond (rest `((apply (~ ,name object) ,@positional ,rest) ,@local-result))
                                                                         (else `((,dispatch object ,@positional) ,@local-result))))
                                                          `(add-method local-proxy ',name (lambda (local-proxy . rest)
                                                                                            (apply ,(add remotable-name (string-append "-" (symbol->string name))) (local-proxy-object local-proxy) rest))))))
                            (set! remotes (append remotes (list #; `(method override (,name self ,@parameters)
                                                                   ,(let ((call (if rest `(apply ,invoker ',name self ,@positional ,rest) `(,invoker ',name self ,@positional))))
                                                                      (if value-keyword
                                                                          `(proxy-value self ,value-keyword (lambda () ,call))
                                                                        call)))
                                                            `(add-method remote-proxy ',name (lambda (remote-proxy . rest)
                                                                                               (apply call-remote ',name remote-proxy rest))))))))))
                    remotable-body)
          (let ((values-method
                  (if (null? values)
                      '()
                    `((method override (proxy-values self)
                        (append (list ,@values)
                                (nextmethod self)))))))
            `(begin
               ,@defines
               ;; local
               (define-type-of-local-proxy ,local-class)
               (define (,local-new . rest)
                 (let ((local-proxy
                         (apply
                           ,local-make
                           ',local-class
                           #f
                           rest)))
                   (,local-setup local-proxy)
                   local-proxy))
               (define (,local-setup local-proxy)
                 (setup-object local-proxy)
                 (add-method local-proxy 'stub (lambda (local-proxy) ',remotable-name))
                 ,@locals)
               (register-local-proxy-class ',remotable-name ,local-new)
               ;; remote
               (define-type-of-remote-proxy ,remote-class)
               (define (,remote-new . rest)
                 (let ((remote-proxy
                         (apply
                           ,remote-make
                           ',remote-class
                           #f
                           rest)))
                   (,remote-setup remote-proxy)
                   remote-proxy))
               (define (,remote-setup remote-proxy)
                 (setup-object remote-proxy)
                 (add-method remote-proxy 'stub (lambda (remote-proxy) ',remotable-name))
                 ,@remotes)
               (register-remote-proxy-class ',remotable-name ,remote-new))))))
    
    (let ((rest (cdr (desourcify form-src))))
      (sourcify-deep-if
        (let ((expansion (expand-remotable-stub rest)))
          ;(pp expansion)
          expansion)
        form-src))))
