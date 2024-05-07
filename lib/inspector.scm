;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Inspector
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
;;;; Package
;;;


(define presentation-limit
  256)


(define Unbound
  (cons #f #f))


(define (package-unbound)
  Unbound)


(define (package-info value #!key (kind ':value) (mutable? #t) (max-width #f))
  (let ((max-width (or max-width presentation-limit)))
    (define (more-value? value)
      (cond ((pair? value) #t)
            ((table? value) (> (table-length value) 0))
            ((structure? value) (> (kind-length (structure-kind value)) 0))
            ((string? value) #f)
            ((sequence? value) (> (sequence-length value) 0))
            ((closure? value) #t)
            (else #f)))
    
    (define (truncate str)
      (if (> (string-length str) max-width)
          (string-append (substring str 0 max-width) "...")
        str))
    
    (let ((serial (object->serial value)))
      (gc-protect value)
      (let* ((unbound? (eq? value Unbound))
             (raw? (eq? kind ':raw))
             (class (find-class-name value))
             (presentation (cond (unbound? "<unbound>")
                                 (raw? (truncate (->string value)))
                                 (else (safe-present-object value max-width))))
             (more? (cond ((or unbound? raw?) #f)
                          ((memq kind '(frame: context:)) #t)
                          (else (more-value? value)))))
        (list serial class presentation more? mutable? kind)))))


;;;
;;;; Inspect
;;;


(define inspect-max-width
  256)

(define inspect-max-content
  256)


(define (inspect-value value #!key (max-width #f) (max-content inspect-max-content) (packager package-info))
  (let ((max-width (or max-width inspect-max-width)))
    (define (inspect-structure struct)
      (let ((kind (structure-kind struct)))
        (map (lambda (info)
               (bind (name rank) info
                 (cons (list #f name rank) (packager (structure-ref struct rank kind)))))
             (cached-kind-fields kind))))
    
    (define (inspect-list lst)
      (define (proper-length l n)
        (cond ((null? l) n)
              ((pair? l) (proper-length (cdr l) (+ n 1)))
              (else (+ n 1))))
      
      (let ((total (proper-length lst 0))
            (content (let iter ((scan lst)
                                (rank 0))
                       (cond ((or (null? scan) (and max-content (>= rank max-content)))
                              '())
                             ((pair? scan)
                              (cons (cons (list #f rank rank) (packager (car scan)))
                                    (iter (cdr scan) (+ rank 1))))
                             (else
                              (list (cons (list #f 'rest rank) (packager scan))))))))
        (add-missing total content)))
    
    (define (inspect-sequence seq)
      (inspect-list (coerce-list seq)))
    
    (define (inspect-table table)
      (let ((total (table-length table))
            (content (let iter ((scan (table->list table))
                                (rank 0))
                       (if (or (null? scan) (and max-content (>= rank max-content)))
                           '()
                         (bind (key . value) (car scan)
                           (cons (cons (list #f (safe-present-object key max-width) rank) (packager value))
                                 (iter (cdr scan) (+ rank 1))))))))
        (let ((sorted (sort string<? content key: (lambda (info) (cadr (car info))))))
          (add-missing total sorted))))
    
    (define (inspect-closure value)
      (inspect-list (closure-environment value)))
    
    (define (add-missing total content)
      (let ((missing (- total (length content))))
        (if (> missing 0)
            (cons (cons (list #f #f -1) (packager (format "{a} missing of {a}" missing total) kind: ':raw)) content)
          content)))
    
    (cond ((null/pair? value) (inspect-list value))
          ((table? value) (inspect-table value))
          ((structure? value) (inspect-structure value))
          ((sequence? value) (inspect-sequence value))
          ((closure? value) (inspect-closure value))
          (else '()))))


;;;
;;;; Kind
;;;


(define kind-cache
  (make-table test: eq?))


(define (cached-kind-fields kind)
  (or (table-ref kind-cache kind #f)
      (let ((fields (kind-fields kind)))
        (table-set! kind-cache kind fields)
        fields)))


;;;
;;;; Sort
;;;


(define (sort smaller l #!key (key #f))
  (define (apply-key key object)
    (if (not key)
        object
      (key object)))

  (define (merge-sort l)
    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (smaller (apply-key key e1) (apply-key key e2))
                   (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))
    
    (define (split l)
      (if (or (null? l) (null? (cdr l)))
          l
        (cons (car l) (split (cddr l)))))
    
    (if (or (null? l) (null? (cdr l)))
        l
      (let* ((l1 (merge-sort (split l)))
             (l2 (merge-sort (split (cdr l)))))
        (merge l1 l2))))
  
  (merge-sort l))
