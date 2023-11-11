;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Stubs
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


#;
(class package ,interface-class extends Stub-Interface
  (method override (local-class self)
    ,local-class)
  (method override (remote-class self)
    ,remote-class))


#;
(interface ,@stub-modifiers ,stub-interface extends Remotable-Stub metaclass ,interface-class
  ,@(queue-list proxies))


#;
(class package ,local-class extends Local-Proxy implements ,stub-interface
  (method override (stub-reference self)
    (reify-reference ,stub-interface))
  ,@values-method
  ,@(queue-list locals))


#;
(class package ,remote-class extends Remote-Proxy implements ,stub-interface
  (method override (stub-reference self)
    (reify-reference ,stub-interface))
  ,@(queue-list remotes))


#;
(method override (break-thread self rest)
  ((apply (~ ,name object) rest)))


#;
(method override (break-thread self rest)
  (apply post-remote 'break-thread self rest))


;;;
;;;; Register
;;;


(define (register-proxy-find-object register-proxy name)
  ((dispatch register-proxy 'find-object) register-proxy name))


(define (register-proxy-register-object register-proxy name object . rest)
  (apply (dispatch register-proxy 'register-object) register-proxy name object rest))


(define-type-of-local-proxy register-local-proxy)


(define (new-register-local-proxy presence)
  (let ((register-local-proxy
          (make-register-local-proxy
            'register-local-proxy
            #f
            presence
            (new-register))))
    (setup-register-local-proxy register-local-proxy)
    register-local-proxy))


(define (setup-register-local-proxy register-local-proxy)
  (setup-object register-local-proxy)
  (add-method register-local-proxy 'stub (lambda (register-local-proxy) 'register))
  (add-method register-local-proxy 'find-object register-local-proxy-find-object)
  (add-method register-local-proxy 'register-object register-local-proxy-register-object))


(define (register-local-proxy-find-object register-local-proxy name)
  (register-find-object (local-proxy-object register-local-proxy) name))


(define (register-local-proxy-register-object register-local-proxy name object . rest)
  (apply register-register-object (local-proxy-object register-local-proxy) name object rest))


(define-type-of-remote-proxy register-remote-proxy)


(define (new-register-remote-proxy presence ior values)
  (let ((register-remote-proxy
          (make-register-remote-proxy
            'register-remote-proxy
            #f
            presence
            ior
            values)))
    (setup-register-remote-proxy register-remote-proxy)
    register-remote-proxy))


(define (setup-register-remote-proxy register-remote-proxy)
  (setup-object register-remote-proxy)
  (add-method register-remote-proxy 'stub (lambda (register-remote-proxy) 'register))
  (add-method register-remote-proxy 'find-object register-remote-proxy-find-object)
  (add-method register-remote-proxy 'register-object register-remote-proxy-register-object))


(define (register-remote-proxy-find-object register-remote-proxy name)
  (call-remote 'find-object register-remote-proxy name))


(define (register-remote-proxy-register-object register-remote-proxy name object . rest)
  (apply call-remote 'register-object name object rest))


#;
(remotable-stub register
  
  
  (method public call (find-object self name))
  (method public call (require-object self name))
  (method public call (load-object self module-name name))
  (method public call (load-reference self reference))
  (method public call (register-object self name object . rest))
  (method public call (unregister-object self name . rest))
  (method public call (get-manifest self)))


;;;
;;;; Debuggee-Process
;;;


(define (debuggee-process-proxy-foo debuggee-process-proxy)
  ((dispatch debuggee-process-proxy 'foo) debuggee-process-proxy))


(define-type-of-local-proxy debuggee-process-local-proxy)


(define (new-debuggee-process-local-proxy presence object)
  (let ((debuggee-process-local-proxy
          (make-debuggee-process-local-proxy
            'debuggee-process-local-proxy
            #f
            presence
            object)))
    (setup-debuggee-process-local-proxy debuggee-process-local-proxy)
    debuggee-process-local-proxy))


(define (setup-debuggee-process-local-proxy debuggee-process-local-proxy)
  (setup-object debuggee-process-local-proxy)
  (add-method debuggee-process-local-proxy 'stub (lambda (debuggee-process-local-proxy) 'debuggee-process))
  (add-method debuggee-process-local-proxy 'foo debuggee-process-local-proxy-foo))


(define (debuggee-process-local-proxy-foo debuggee-process-local-proxy)
  'local)


(define-type-of-remote-proxy debuggee-process-remote-proxy)


(define (new-debuggee-process-remote-proxy presence ior values)
  (let ((debuggee-process-remote-proxy
          (make-debuggee-process-remote-proxy
            'debuggee-process-remote-proxy
            #f
            presence
            ior
            values)))
    (setup-debuggee-process-remote-proxy debuggee-process-remote-proxy)
    debuggee-process-remote-proxy))


(define (setup-debuggee-process-remote-proxy debuggee-process-remote-proxy)
  (setup-object debuggee-process-remote-proxy)
  (add-method debuggee-process-remote-proxy 'stub (lambda (debuggee-process-remote-proxy) 'debuggee-process))
  (add-method debuggee-process-remote-proxy 'foo debuggee-process-remote-proxy-foo))


(define (debuggee-process-remote-proxy-foo debuggee-process-remote-proxy)
  'remote)


#;
(remotable-stub Debuggee-Process
  
  
  (method public call value (get-id self))
  (method public call value (get-title self))
  (method public call value (get-traits self))
  (method public call value (get-icon self))
  (method public send (detach-process self))
  (method public post (reload-process self))
  (method public post (attach-to-debugger self debugger-proxy debugger-cookie))
  (method public post (quit-process self))
  (method public send (console-input self thread line))
  (method public post (console-close self thread))
  (method public call (get-toplevel-hwnd self))
  (method public call (get-threads self))
  (method public post (restart-stop self restart))
  (method public post (break-thread self thread))
  (method public post (repl-thread self thread))
  (method public post (terminate-thread self thread preserve-console?))
  (method public post (kill-thread self thread))
  (method public post (continue-stop self stop))
  (method public post (step-stop self stop command))
  (method public post (transmit-runtime-information self what))
  (method public post (transmit-product-runtime-information self what product))
  (method public post (evaluate-expressions self forms syntax str container line col evaluation-mode walk-debugger walk-cookie))
  (method public send (profiler-start self))
  (method public send (profiler-stop self))
  (method public call (profiler-profiles self))
  (method public call (profiler-selected-profile self))
  (method public call (profiler-selected-profile-set! self name))
  (method public call (profiler-profile self))
  (method public send (profile-reset self profiler depth all?))
  (method public send (tracker-start self))
  (method public send (tracker-stop self))
  (method public call (tracker-trackings self))
  (method public call (tracker-selected-tracking self))
  (method public call (tracker-selected-tracking-set! self name))
  (method public call (tracker-tracking self))
  (method public send (tracking-reset self all?))
  (method public call (inspect-serial self serial max-content))
  (method public post (inspect-continuation self serial))
  (method public call (present-serial self serial))
  (method public post (try-out self class)))


;;;
;;;; Debugger
;;;


#;
(remotable-stub Debugger
  
  
  (method public send (attach-process self process focus?))
  (method public send (ready-process self process cookie))
  (method public send (update-process self process))
  (method public post (detach-process self process))
  (method public send (unavailable-runtime-information self process))
  (method public send (receive-runtime-information self process filenames processes properties))
  (method public post (report-walk-problems self process reason detail cookie))
  (method public call (register-console self process thread select?))
  (method public send (unregister-console self console))
  (method public send (select-console self console))
  (method public send (persist-console self console))
  (method public send (console-output self console line))
  (method public send (clear-console self console))
  (method public send (status-message self message))
  (method public call (get-toplevel-hwnd self))
  (method public post (debuggee-stop self process thread stop kind reason))
  (method public post (register-stop self process thread stop . rest))
  (method public post (unregister-stop self process thread stop))
  (method public post (inspect-object self process info)))


;;;
;;;; Marshall
;;;


(define (marshall-object class obj)
  (case class
    ((version) (marshall-version obj))
    ((ior) (marshall-ior obj))
    ((debuggee-process-local-proxy) (marshall-local-proxy obj))
    ((debuggee-process-remote-proxy) (marshall-remote-proxy obj))
    (else (error "Unable to marshall" class))))


(define (unmarshall-object class content)
  (case class
    ((version) (unmarshall-version content))
    ((ior) (unmarshall-ior content))
    ((debuggee-process-local-proxy) (unmarshall-proxy content))
    ((debuggee-process-remote-proxy) (unmarshall-proxy content))
    (else (error "Unable to unmarshall" class))))


;;;
;;;; Local
;;;


;; quicky
(define (local-class stub-interface)
  (case stub-interface
    ((register) new-register-local-proxy)
    (else (error "Unknow local stub" stub-interface))))


;;;
;;;; Remote
;;;


;; quicky
(define (remote-class stub-interface)
  (case stub-interface
    ((register) new-register-remote-proxy)
    ((debuggee-process) new-debuggee-process-remote-proxy)
    (else (error "Unknow remote stub" stub-interface))))
