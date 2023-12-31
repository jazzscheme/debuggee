;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Exception
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
;;;; Exception
;;;


(define *exception-debugger*
  #f)


(define (get-exception-debugger)
  *exception-debugger*)

(define (set-exception-debugger exception-debugger)
  (set! *exception-debugger* exception-debugger))


(define current-exception-debugger
  (make-parameter #f))


(define (exception-debugger-hook exc other)
  (let ((exception-debugger (active-exception-debugger)))
    (if exception-debugger
        (exception-debugger exc)
      (system-exception-hook exc other))))


(define (with-exception-debugger exception-debugger thunk)
  (parameterize ((current-exception-debugger exception-debugger))
    (thunk)))


(define (active-exception-debugger)
  (or (current-exception-debugger) *exception-debugger*))


(define (system-exception-debugger exc)
  (invoke-exception-hook system-exception-hook exc))


(define (with-system-exception-debugger thunk)
  (with-exception-debugger system-exception-debugger
    thunk))


(define (catch-exception-filter filter catcher thunk)
  (let ((previous-handler (current-exception-handler)))
    (continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (if (with-exception-handler
                  (lambda (filter-exc)
                    (continuation-graft catcher-cont
                      (lambda ()
                        (previous-handler filter-exc))))
                  (lambda ()
                    (filter exc)))
                (continuation-graft catcher-cont
                  (lambda ()
                    (catcher exc)))
              (previous-handler exc)))
          thunk)))))


(define (call-with-catch predicate catcher thunk)
  (catch-exception-filter
    predicate
    catcher
    thunk))
