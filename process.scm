;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Process
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


(define-type-of-object debuggee-process
  threads)


(define (new-debuggee-process)
  (let ((debuggee-process
          (make-debuggee-process
            'debuggee-process
            #f
            #f)))
    (setup-debuggee-process debuggee-process)
    debuggee-process))


(define (setup-debuggee-process debuggee-process)
  (setup-object debuggee-process))


(define (debuggee-process-get-id debuggee-process)
  #f)


(define (debuggee-process-get-title debuggee-process)
  "Scheme")


(define (debuggee-process-get-traits debuggee-process)
  #f)


(define (debuggee-process-get-icon debuggee-process)
  #f)


(define (unimplemented)
  (pp 'unimplemented))


;;;
;;;; Control
;;;


(define (debuggee-process-detach-process debuggee-process)
  (detach-from-controller))


;; quicky
(define (debuggee-process-detach-process-quicky debuggee-process debugger-proxy)
  (closing-remote-proxy debugger-proxy))


(define (debuggee-process-reload-process debuggee-process)
  (reload-process (current-process)))


(define (debuggee-process-attach-to-debugger debuggee-process debugger-proxy debugger-cookie)
  (thread-post (primordial-thread) 'attach-to-debugger
    (lambda ()
      (attach-debuggee-to-controller debugger-proxy focus?: #t)
      (start-repl-thread (current-thread) #f (current-console-port))
      (select-debugger-console)
      (ready-to-controller debugger-cookie))))


(define (debuggee-process-quit-process debuggee-process)
  (detach-from-controller)
  (exit))


;;;
;;;; Console
;;;


(define (debuggee-process-console-input debuggee-process thread-proxy str)
  (let ((thread (debuggee-thread-thread (local-proxy-object thread-proxy))))
    (let ((port (thread-console-pump-port thread)))
      (if (not str)
          (close-port port)
        (begin
          (display str port)
          (force-output port))))))


(define (debuggee-process-console-close debuggee-process thread-proxy)
  (let ((thread (debuggee-thread-thread (local-proxy-object thread-proxy))))
    (close-thread-console thread)))


;;;
;;;; Process
;;;


(define (debuggee-process-get-toplevel-hwnd debuggee-process)
  (unimplemented))


;;;
;;;; State
;;;


(define (debuggee-process-get-threads debuggee-process)
  (let ((threads (map (lambda (thread)
                        (new-debuggee-thread-local-proxy (require-presence 'debugging) (new-debuggee-thread thread)))
                      (top-threads))))
    ;; gc protect
    (debuggee-process-threads-set! debuggee-process threads)
    threads))


;;;
;;;; Restart
;;;


(define (debuggee-process-restart-stop debuggee-process restart-proxy)
  (let ((debuggee-restart (local-proxy-object restart-proxy)))
    (let ((thread (get-thread debuggee-restart))
          (restart (get-restart debuggee-restart)))
      (thread-post thread 'restart-stop
        (lambda ()
          (invoke-restart restart))))))


;;;
;;;; Thread
;;;


(define (debuggee-process-break-thread debuggee-process thread-proxy)
  (let ((thread (debuggee-thread-thread (local-proxy-object thread-proxy))))
    (thread-int! thread
      (lambda ()
        (break)))))


(define (debuggee-process-repl-thread debuggee-process thread-proxy)
  (let ((thread (debuggee-thread-thread (local-proxy-object thread-proxy))))
    (thread-int! thread
      (lambda ()
        (jazz.platform:terminal-repl)))))


(define (debuggee-process-terminate-thread debuggee-process thread-proxy preserve-console?)
  (let ((thread (debuggee-thread-thread (local-proxy-object thread-proxy))))
    (if (eq? thread (primordial-thread))
        (exit)
      (begin
        (catch-exception-filter
          inactive-thread-exception?
          (lambda (exception) #f)
          (lambda () (exit-thread thread)))
        (when (not preserve-console?)
          (close-thread-console thread))))))


(define (debuggee-process-kill-thread debuggee-process thread-proxy)
  (let ((thread (debuggee-thread-thread (local-proxy-object thread-proxy))))
    (if (eq? thread (primordial-thread))
        (exit)
      (begin
        (catch-exception-filter
          inactive-thread-exception?
          (lambda (exception) #f)
          (lambda () (thread-terminate! thread)))
        (close-thread-console thread)))))


;;;
;;;; Stop
;;;


(define (debuggee-process-continue-stop debuggee-process stop-proxy)
  (let ((stop (local-proxy-object stop-proxy)))
    (let ((thread (get-thread stop))
          (continuation (get-continuation stop)))
      (thread-post thread 'continue-stop
        (lambda ()
          (continuation-return continuation #f))))))


(define (debuggee-process-step-stop debuggee-process stop-proxy command)
  (let ((stop (local-proxy-object stop-proxy)))
    (let ((thread (get-thread stop))
          (stepper (get-stepper stop)))
      (thread-post thread 'step-stop
        (lambda ()
          (stepper command))))))


;;;
;;;; Runtime
;;;


(define (debuggee-process-transmit-runtime-information debuggee-process what)
  (debugger-proxy-receive-runtime-information controller-debugger local-process '() '() '()))


(define (debuggee-process-transmit-product-runtime-information debuggee-process what product)
  (unimplemented))


;;;
;;;; Evaluate
;;;


(define (debuggee-process-evaluate-expressions debuggee-process forms syntax str container line col evaluation-mode walk-debugger walk-cookie)
  (thread-call (primordial-thread) 'evaluate-expressions
    (lambda ()
      (evaluate-forms syntax str container line col evaluation-mode))))


;;;
;;;; Profiler
;;;


(define (debuggee-process-profiler-start debuggee-process)
  (unimplemented))


(define (debuggee-process-profiler-stop debuggee-process)
  (unimplemented))


(define (debuggee-process-profiler-profiles debuggee-process)
  (unimplemented))


(define (debuggee-process-profiler-selected-profile debuggee-process)
  (unimplemented))


(define (debuggee-process-profiler-selected-profile-set! debuggee-process name)
  (unimplemented))


(define (debuggee-process-profiler-profile debuggee-process)
  (unimplemented))


(define (debuggee-process-profile-reset debuggee-process profiler depth all?)
  (unimplemented))


;;;
;;;; Tracker
;;;


(define (debuggee-process-tracker-start debuggee-process)
  (unimplemented))


(define (debuggee-process-tracker-stop debuggee-process)
  (unimplemented))


(define (debuggee-process-tracker-trackings debuggee-process)
  (unimplemented))


(define (debuggee-process-tracker-selected-tracking debuggee-process)
  (unimplemented))


(define (debuggee-process-tracker-selected-tracking-set! debuggee-process name)
  (unimplemented))


(define (debuggee-process-tracker-tracking debuggee-process)
  (unimplemented))


(define (debuggee-process-tracking-reset debuggee-process all?)
  (unimplemented))


;;;
;;;; Inspector
;;;


(define (debuggee-process-inspect-serial debuggee-process serial max-content)
  (inspect-value (serial->object serial) max-content: max-content))


(define (debuggee-process-inspect-continuation debuggee-process serial)
  (let ((k (serial->object serial)))
    (assert (continuation? k))
    (thread-start! (make-thread (lambda ()
                                  ;; Prevent problems with continuation-checkpoints
                                  (continuation-graft-no-winding k break))
                                (format "Continuation #{a}" serial)
                                debugged-continuations-thread-group))))


(define (debuggee-process-present-serial debuggee-process serial)
  (let ((port (open-output-string)))
    (display (serial->object serial) port)
    (get-output-string port)))


;;;
;;;; Designer
;;;


(define (debuggee-process-try-out debuggee-process class)
  (unimplemented))
