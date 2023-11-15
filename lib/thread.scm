;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Thread
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


(define-type-of-object debuggee-thread
  thread)


(define (new-debuggee-thread thread)
  (let ((debuggee-thread
          (make-debuggee-thread
            'debuggee-thread
            #f
            thread)))
    (setup-debuggee-thread debuggee-thread)
    debuggee-thread))


(define (setup-debuggee-thread debuggee-thread)
  (setup-object debuggee-thread))


(define (debuggee-thread-get-id debuggee-thread)
  (object->serial (debuggee-thread-thread debuggee-thread)))


(define (debuggee-thread-get-name debuggee-thread)
  (thread-name (debuggee-thread-thread debuggee-thread)))


(define (debuggee-thread-get-state debuggee-thread)
  (present-thread-state (thread-state (debuggee-thread-thread debuggee-thread))))


(define (debuggee-thread-get-priority debuggee-thread)
  (thread-base-priority (debuggee-thread-thread debuggee-thread)))


;; This is a hack to support continuation inspection. This should be reimplemented by either allowing
;; the debugger to directly inspect a continuation with a proper continuation stub without having to
;; create a temporary thread
(define (debuggee-thread-get-debugged-continuation? debuggee-thread)
  (eq? (thread-thread-group (debuggee-thread-thread debuggee-thread)) debugged-continuations-thread-group))


(define (debuggee-thread-get-stops debuggee-thread)
  (thread-active-stops (debuggee-thread-thread debuggee-thread)))


(define (debuggee-thread-set-repl-frame debuggee-thread frame-proxy)
  ;; todo
  #f)
