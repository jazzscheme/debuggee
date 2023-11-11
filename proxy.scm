;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Proxies
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


(define-type proxy
  extender: define-type-of-proxy
  unprintable:
  presence
  method-live?)


(define (proxy-live? proxy)
  ((proxy-method-live? proxy) proxy))


;;;
;;;; Local
;;;


#;
(method meta override (marshall-object local-proxy object)
  (marshall-local-proxy object))


#;
(method meta override (unmarshall-object local-proxy content)
  (unmarshall-proxy content))


(define-type-of-proxy local-proxy
  extender: define-type-of-local-proxy
  object)


(define (new-local-proxy presence object)
  (make-local-proxy
    presence
    local-proxy-live?
    object))


(define (local-proxy-print local-proxy output readably)
  (print-unreadable local-proxy output
    (lambda (output)
      (format output "{?{a} ~}{s}"
        (get-purpose presence)
        object))))


(define (local-proxy-proxy-values local-proxy)
  '())


(define (local-proxy-live? local-proxy)
  #t)


;;;
;;;; Remote
;;;


#;
(method meta override (marshall-object remote-proxy object)
  (marshall-remote-proxy object))


#;
(method meta override (unmarshall-object remote-proxy content)
  (unmarshall-proxy content))


(define-type-of-proxy remote-proxy
  ior
  values)


(define (new-remote-proxy presence ior values)
  (make-remote-proxy
    presence
    remote-proxy-live?
    ior
    values))


(define (remote-proxy-print remote-proxy output readably)
  (print-unreadable remote-proxy output
    (lambda (output)
      (format output "{?{a} ~}{s}"
        (get-purpose presence)
        (get-uuid ior)))))


;; meant to be used by the remoting system only
(define (remote-proxy-set-values remote-proxy lst)
  (set! values lst))


(define (remote-proxy-proxy-value remote-proxy keyword thunk)
  (let ((prop (getprop values keyword)))
    (if prop
        (cadr prop)
      (thunk))))
