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
            ((string? value) #f)
            ;; todo ((sequence? value) (> (cardinality value) 0))
            ;; todo ((closure? value) #t)
            ;; todo ((object? value) (not-null? (get-instance-slots (class-of value))))
            (else #f)))
    
    (define (truncate str)
      (if (> (cardinality str) max-width)
          (concatenate (subseq str 0 max-width) "...")
        str))
    
    (let ((serial (object->serial value)))
      (gc-protect value)
      (let* ((unbound? (eq? value Unbound))
             (raw? (eq? kind ':raw))
             (class (if (object? value) (object-class value) '<unknown>))
             (presentation (cond (unbound? "<unbound>")
                                 (raw? (truncate (->string value)))
                                 (else (safe-present-object value max-width))))
             (more? (cond ((or unbound? raw?) #f)
                          ((memq kind '(frame: context:)) #t)
                          (else (more-value? value)))))
        (list serial class presentation more? mutable? kind)))))
