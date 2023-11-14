;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Core
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
;;;; Enumerator
;;;


(define (enumerator? obj)
  (and (symbol? obj)
       (eqv? (string-ref (symbol->string obj) 0) #\:)))


(define (enumerator->symbol enumerator)
  (let ((name (symbol->string enumerator)))
    (string->symbol (substring name 1 (string-length name)))))


(define (symbol->enumerator symbol)
  (string->symbol (string-append ":" (symbol->string symbol))))


;;;
;;;; Queue
;;;


(define-type queue
  head
  tail
  shared)


(define (new-queue)
  (make-queue
    '()
    #f
    #f))


(define (queue-empty? queue)
  (null? (queue-head queue)))


(define (queue-length queue)
  (length (queue-head queue)))


(define (enqueue queue object)
  (let ((added (cons object '())))
    (enqueue-impl queue added
      (lambda (tail)
        (set-cdr! tail added)))
    (queue-tail-set! queue added)
    (queue-shared-set! queue #f)))


(define (enqueue-impl queue added proc)
  (define (stitch-head added)
    (let ((tail (queue-tail queue)))
      (if tail
          (set-cdr! tail added)
        (queue-head-set! queue added))))
  
  (let ((shared (queue-shared queue)))
    (if shared
        (let ((copy (list-copy (queue-shared queue))))
          (stitch-head copy)
          (proc (last-pair copy)))
      (stitch-head added))))


(define (dequeue queue)
  (let ((head (queue-head queue)))
    (if (null? head)
        (error "Queue is empty")
      (let ((next (cdr head)))
        (cond ((eq? head (queue-tail queue))
               (queue-tail-set! queue #f))
              ((eq? head (queue-shared queue))
               (queue-shared-set! queue (if (null? next) #f next))))
        (queue-head-set! queue next)
        (car head)))))


(define (queue-list queue)
  (queue-head queue))


;;;
;;;; Structure
;;;


(define (kind? obj)
  (##type? obj))

(define (kind-id type)
  (##type-id type))

(define (kind-name type)
  (##type-name type))

(define (kind-flags type)
  (##type-flags type))

(define (kind-super type)
  (##type-super type))

(define (kind-length type)
  (##type-field-count type))

(define (kind-fields type)
  (let loop ((index 1)
             (fields (##type-all-fields type))
             (alist '()))
       (if (pair? fields)
           (let* ((name (car fields))
                  (rest (cdr fields))
                  (options (car rest))
                  (rest (cdr rest))
                  (val (car rest))
                  (rest (cdr rest)))
             (loop (fx+ index 1)
                   rest
                   (cons (list name index options val)
                         alist)))
         (reverse alist))))


(define (structure? obj)
  (##structure? obj))

(define (structure-kind obj)
  (##structure-type obj))

(define (structure-ref obj i type)
  (##structure-ref obj i type #f))

(define (structure-set! obj val i type)
  (##structure-set! obj val i type #f))


;;;
;;;; Readtable
;;;


(define scheme-readtable
  (current-readtable))


(define (with-readtable readtable thunk)
  (parameterize ((current-readtable readtable))
    (thunk)))


;;;
;;;; Reader
;;;


(define (input-port-names-set! port names)
  (##vector-set! port 4 names))

(define (input-port-line-set! port line)
  (##input-port-line-set! port line))

(define (input-port-column-set! port col)
  (##input-port-column-set! port col))


(define (read-source-all port #!optional (container #f) (line #f) (col #f))
  (if container
      (input-port-names-set! port (lambda (port) container)))
  (if line
      (input-port-line-set! port (fx+ line 1)))
  (if col
      (input-port-column-set! port (fx+ col 1)))
  
  (let ((begin-vector
          (##read-all-as-a-begin-expr-from-port
            port
            (current-readtable)
            ##wrap-datum
            ##unwrap-datum
            (readtable-start-syntax (current-readtable))
            #f)))
    (cdr (##source-code (vector-ref begin-vector 1)))))


;;;
;;;; Exception
;;;


(define (exception-reason exc)
  (let ((output (open-output-string)))
    (display-exception exc output)
    (let ((str (get-output-string output)))
      (let ((len (string-length str)))
        (if (and (fx> len 0)
                 (eqv? (string-ref str (fx- len 1)) #\newline))
            (substring str 0 (fx- len 1))
          str)))))


(define (exception-detail exc)
  #f)


(define (exception-location exc cont)
  (##exception->locat exc cont))


;;;
;;;; Thread
;;;


(define thread-int!
  ##thread-int!)


;;;
;;;; Source
;;;


(define (locat->container/line/col locat)
  (let ((container (and locat (##locat-container locat))))
    (if container
        (let ((filepos (##position->filepos (##locat-position locat))))
          (let ((line (##filepos-line filepos))
                (col (##filepos-col filepos)))
            (list container line col)))
      #f)))

;;;
;;;; Stack
;;;


(define (procedure-name procedure)
  (##procedure-name procedure))


(define (closure? obj)
  (and (##procedure? obj)
       (##closure? obj)))


(define (get-continuation-location cont)
  (locat->container/line/col (##continuation-locat cont)))


(define (interpreted-continuation? cont)
  (##interp-continuation? cont))


(define (get-continuation-stack cont depth)
  (let ((queue (new-queue)))
    (iterate-continuation-stack cont depth
      (lambda (cont)
        (enqueue queue cont)))
    (queue-list queue)))


(define (get-continuation-dynamic-environment cont)
  (let ((queue (new-queue)))
    (iterate-continuation-dynamic-environment cont reference-name
      (lambda (info)
        (enqueue queue info)))
    (queue-list queue)))


(define (get-continuation-lexical-environment cont)
  (let ((queue (new-queue)))
    (iterate-continuation-lexical-environment cont
      (lambda (info)
        (enqueue queue info)))
    (queue-list queue)))


(define (with-repl-context cont thunk)
  (let ((prev-context (thread-repl-context-get!)))
    (let ((context
            (make-repl-context
              (fx+ (repl-context-level prev-context) 1)
              0
              cont
              cont
              #f
              prev-context
              #f)))
      (repl-context-bind
        context
        thunk))))


;;;
;;;; Repl
;;;


(define (repl-result-history-add result)
  (let ((channel (##thread-repl-channel-get! (current-thread))))
    (##repl-channel-result-history-add channel result)))


;;;
;;;; Hook
;;;


(define (get-exception-hook)
  (primordial-exception-handler-hook-ref))

(define (set-exception-hook hook)
  (primordial-exception-handler-hook-set! hook))


(define (invoke-exception-hook hook exc)
  (hook exc thread-end-with-uncaught-exception!))


;;;
;;;; System
;;;


(define (system-exception-hook exc other)
  (repl-exception-handler-hook exc other))
