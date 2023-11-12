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


(define-type-of-object proxy
  extender: define-type-of-proxy
  unprintable:
  presence)


(define (setup-proxy proxy)
  (setup-object proxy))


(define (proxy-stub proxy)
  ((dispatch proxy 'stub) proxy))


(define (proxy-live? proxy)
  ((dispatch proxy 'live?) proxy))


;;;
;;;; Local
;;;


(define-type-of-proxy local-proxy
  extender: define-type-of-local-proxy
  object)


(define (new-local-proxy presence object)
  (let ((local-proxy
          (make-local-proxy
            'local-proxy
            #f
            presence
            object)))
    (setup-local-proxy local-proxy)
    local-proxy))


(define (setup-local-proxy local-proxy)
  (setup-proxy local-proxy)
  (add-method local-proxy 'live? local-proxy-live?))


(define (local-proxy-print local-proxy output readably)
  (print-unreadable local-proxy output
    (lambda (output)
      (format output "{?{a} ~}{s}"
        (presence-purpose presence)
        object))))


(define (local-proxy-proxy-values local-proxy)
  '())


(define (local-proxy-live? local-proxy)
  #t)


;;;
;;;; Remote
;;;


(define-type-of-proxy remote-proxy
  extender: define-type-of-remote-proxy
  ior
  values)


(define (new-remote-proxy presence ior values)
  (let ((remote-proxy
          (make-remote-proxy
            'remote-proxy
            #f
            presence
            ior
            values)))
    (setup-remote-proxy remote-proxy)
    remote-proxy))


(define (setup-remote-proxy remote-proxy)
  (setup-proxy remote-proxy)
  (add-method remote-proxy 'live? remote-proxy-live?))


(define (remote-proxy-print remote-proxy output readably)
  (print-unreadable remote-proxy output
    (lambda (output)
      (format output "{?{a} ~}{s}"
        (presence-purpose presence)
        (ior-uuid ior)))))


;; meant to be used by the remoting system only
(define (remote-proxy-set-values remote-proxy lst)
  (set! values lst))


(define (remote-proxy-proxy-value remote-proxy keyword thunk)
  (let ((prop (getprop values keyword)))
    (if prop
        (cadr prop)
      (thunk))))
