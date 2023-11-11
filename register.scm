;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Register
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


(define-type-of-local-proxy register-local-proxy)


(define (new-register-proxy presence)
  (make-register-local-proxy
    presence
    local-proxy-live?
    (new-register)))


(define-type register
  objects)


(define (new-register)
  (make-register
    (make-table test: eq?)))


(define (register-find-object self name)
  (table-ref objects name #f))


(define (register-require-object self name)
  (or (find-object self name)
      (error "Unable to find in register" name)))


(define (register-load-object self module-name name)
  (load-unit module-name)
  (require-object self name))


(define (register-load-reference self reference)
  (if (symbol? reference)
      (require-object self reference)
    (bind (module-name name) reference
      (load-object self module-name name))))


;;;
;;;; Registration
;;;


(define (register-register-object self name object #!key (error? #t))
  (if (find-object self name)
      (if error?
          (error "Object is already registered" name)
        #f)
    (begin
      (table-set! objects name object)
      #t)))


(define (register-unregister-object self name #!key (error? #t))
  (if (not (find-object self name))
      (if error?
          (error "Object is not registered" name)
        #f)
    (begin
      (table-clear objects name)
      #t)))


;;;
;;;; Manifest
;;;


(define (register-get-manifest self)
  (table-keys objects))
