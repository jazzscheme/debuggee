;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Network
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


(define localhost
  '#u8(127 0 0 1))

(define anyhost
  "*")


(define (string->host string)
  string)


(define (port->string port)
  (number->string port))


(define (string->port string)
  (string->number string))


(define (parse-host/service str)
  (let ((pos (string-find str #\:))
        (len (string-length str)))
    (if (not pos)
        (values (string->host str) #f #f)
      (let ((host (if (= pos 0)
                      #f
                    (string->host (substring str 0 pos)))))
        (if (= pos (- len 1))
            (values host #f #f)
          (let ((str (substring str (+ pos 1) len)))
            (let ((pos (string-find str #\/))
                  (len (string-length str)))
              (if (not pos)
                  (values host (string->port str) #f)
                (let ((service (string->port (substring str 0 pos)))
                      (alternate-service (string->port (substring str (+ pos 1) len))))
                  (values host service alternate-service))))))))))


;;;
;;;; Services
;;;


(define Port-Numbers
  '((any           .     0)
    (echo          .     7)
    (http          .    80)
    (router-server . 32000)))


(define (resolve-service service)
  (if (integer? service)
      service
    (let ((pair (assq service Port-Numbers)))
      (if pair
          (cdr pair)
        (error "Unknow service" service)))))
