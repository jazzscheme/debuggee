;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee Frame
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


(define-type-of-object debuggee-frame
  id
  location
  continuation
  name
  hidden?)


(define (new-debuggee-frame id location continuation name hidden?)
  (let ((debuggee-frame
          (make-debuggee-frame
            'debuggee-frame
            #f
            id
            location
            continuation
            name
            hidden?)))
    (setup-debuggee-frame debuggee-frame)
    debuggee-frame))


(define (setup-debuggee-frame debuggee-frame)
  (setup-object debuggee-frame))


(define (debuggee-frame-get-id debuggee-frame)
  (debuggee-frame-id debuggee-frame))


(define (debuggee-frame-get-continuation debuggee-frame)
  (debuggee-frame-continuation debuggee-frame))


(define (debuggee-frame-get-name debuggee-frame)
  (debuggee-frame-name debuggee-frame))


(define (debuggee-frame-get-interpreted? debuggee-frame)
  (let ((cont (serial->object (debuggee-frame-continuation debuggee-frame))))
    (interpreted-continuation? cont)))


(define (debuggee-frame-get-hidden? debuggee-frame)
  (debuggee-frame-hidden? debuggee-frame))


;;;
;;;; Variables
;;;


(define (debuggee-frame-get-location debuggee-frame)
  ;; to test the stepper
  (or (debuggee-frame-location debuggee-frame)
      (let ((cont (serial->object (debuggee-frame-continuation debuggee-frame))))
        (get-continuation-location cont))))


(define (debuggee-frame-get-variables debuggee-frame kind)
  (let ((cont (serial->object (debuggee-frame-continuation debuggee-frame))))
    (let ((variables (case kind
                       ((:lexical) (get-continuation-lexical-environment cont))
                       ((:all) (append (get-continuation-lexical-environment cont)
                                       (get-continuation-dynamic-environment cont))))))
      (map (lambda (variable rank)
             (bind (name value mutable?) variable
               (cons (list #f name rank) (package-info value mutable?: mutable?))))
           variables
           (naturals 0 (length variables))))))
