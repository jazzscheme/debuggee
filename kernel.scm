;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel
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


;;;
;;;; Job
;;;


(define (exit-jobs)
  ##exit-jobs)

(define (add-exit-job! thunk)
  (##add-exit-job! thunk))

(define (clear-exit-jobs!)
  (##clear-exit-jobs!))


;;;
;;;; Exception
;;;


(define (primordial-exception-handler-hook-ref)
  ##primordial-exception-handler-hook)

(define (primordial-exception-handler-hook-set! hook)
  (set! ##primordial-exception-handler-hook hook))

(define (thread-end-with-uncaught-exception! exc)
  (##thread-end-with-uncaught-exception! exc))


(define (repl-exception-handler-hook exc other)
  (##repl-exception-handler-hook exc other))


;;;
;;;; Stack
;;;


(define hidden-frames
  (list
    ##dynamic-env-bind))

(define (hidden-frame? frame)
  (memq frame hidden-frames))

(define (hidden-frame?-set! predicate)
  (set! hidden-frame? predicate))


(define (iterate-continuation-stack cont depth proc)
  (let iter ((d 0)
             (cont cont))
       (if (or (not depth) (fx< d depth))
           (and cont
                (begin
                  (proc cont)
                  (iter (fx+ d 1)
                        (##continuation-next-frame cont #f)))))))


(define (iterate-var-val var val-or-box cte proc)
  (cond ((##var-i? var)
         (iterate-var-val-aux (##var-i-name var)
                              val-or-box
                              #t
                              cte
                              proc))
        ((##var-c-boxed? var)
         (iterate-var-val-aux (##var-c-name var)
                              (unbox val-or-box)
                              #t
                              cte
                              proc))
        (else
         (iterate-var-val-aux (##var-c-name var)
                              val-or-box
                              #f
                              cte
                              proc))))


(define (iterate-var-val-aux var val mutable? cte proc)
  (define (remove-quote val)
    (if (and (pair? val)
             (eq? (car val) 'quote)
             (not (null? (cdr val))))
        (cadr val)
      val))
  
  (proc
    (list (object->string var)
          (cond ((absent-object? val)
                 '<absent>)
                ((unbound-object? val)
                 '<unbound>)
                ((procedure? val)
                 (remove-quote
                   (if (##cte-top? cte)
                       (##inverse-eval-in-env val cte)
                     (##inverse-eval-in-env val (##cte-parent-cte cte)))))
                (else
                 val))
          mutable?)))


(define (iterate-continuation-dynamic-environment cont reference-name proc)
  (define (iterate-parameters lst cte proc)
    (let iter ((lst lst))
         (if (pair? lst)
             (let* ((param-val (car lst))
                    (param (car param-val))
                    (val (cdr param-val)))
               (if (not (##hidden-parameter? param))
                   (let ((name (reference-name (##inverse-eval-in-env param cte))))
                     (iterate-var-val-aux (list name) val #t cte proc)))
               (iter (cdr lst))))))
  
  (and cont
       (iterate-parameters
         (##dynamic-env->list (continuation-denv cont))
         (if (##interp-continuation? cont)
             (let (($code (##interp-continuation-code cont)))
               (code-cte $code))
           ##interaction-cte)
         proc)))


(define (iterate-continuation-lexical-environment cont proc)
  (define (iterate-rte cte rte proc)
    (let loop1 ((c cte)
                (r rte))
         (cond ((##cte-top? c))
               ((##cte-frame? c)
                (let loop2 ((vars (##cte-frame-vars c))
                            (vals (cdr (vector->list r))))
                     (if (pair? vars)
                         (let ((var (car vars)))
                           (if (not (##hidden-local-var? var))
                               (iterate-var-val var (car vals) c proc))
                           (loop2 (cdr vars)
                                  (cdr vals)))
                       (loop1 (##cte-parent-cte c)
                              (rte-up r)))))
               (else
                (loop1 (##cte-parent-cte c)
                       r)))))
  
  (define (iterate-vars lst cte proc)
    (let iter ((lst lst))
         (if (pair? lst)
             (let* ((var-val (car lst))
                    (var (car var-val))
                    (val (cdr var-val)))
               (iterate-var-val var val cte proc)
               (iter (cdr lst))))))
  
  (define (iterate-locals lst cte proc)
    (and lst
         (iterate-vars lst cte proc)))
  
  (and cont
       (if (##interp-continuation? cont)
           (let (($code (##interp-continuation-code cont))
                 (rte (##interp-continuation-rte cont)))
             (iterate-rte (code-cte $code) rte proc)
             (code-cte $code))
         (begin
           (iterate-locals (##continuation-locals cont) ##interaction-cte proc)
           ##interaction-cte))))


;;;
;;;; Process
;;;


(define process-name
  #f)

(define process-title
  #f)

(define process-prefix
  #f)

(define process-traits
  #f)

(define process-icon
  #f)

(define process-version
  #f)


(define (current-process-name)
  process-name)

(define (current-process-name-set! name)
  (set! process-name name))

(define (current-process-title)
  process-title)

(define (current-process-title-set! title)
  (set! process-title title))

(define (current-process-prefix)
  process-prefix)

(define (current-process-prefix-set! prefix)
  (set! process-prefix prefix))

(define (current-process-traits)
  process-traits)

(define (current-process-traits-set! traits)
  (set! process-traits traits))

(define (current-process-icon)
  process-icon)

(define (current-process-icon-set! icon)
  (set! process-icon icon))

(define (current-process-version)
  process-version)

(define (current-process-version-set! version)
  (set! process-version version))
