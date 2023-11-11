;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Remote Listener
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


(define-type remote-listener
  presence
  host
  service
  alternate-service
  socket-info
  server-thread)


(define (new-remote-listener presence #!key (host #f) (service #f) (alternate-service #f))
  (make-remote-listener
    presence
    host
    service
    alternate-service
    #f
    #f))


;;;
;;;; Start/Stop
;;;


(define (remote-listener-start self)
  (remote-listener-start-listener self))


(define (remote-listener-stop self)
  (exit-thread (remote-listener-server-thread self))
  (remote-listener-server-thread-set! self #f))


(define (remote-listener-start-listener self)
  (let ((host (remote-listener-host self))
        (service (remote-listener-service self))
        (alternate-service (remote-listener-alternate-service self)))
    (let ((server-port (open-tcp-server (list server-address: host port-number: (resolve-service service) keep-alive: #t))))
      (remote-listener-socket-info-set! self (tcp-server-socket-info server-port))
      (let ((thread
              (make-thread
                (lambda ()
                  (let loop ()
                       (let ((port (read server-port)))
                         (thread-start!
                           (make-thread
                             (lambda ()
                               (pp 'accept-connection)
                               (remote-listener-accept-connection self port))))
                         (loop)))))))
        (thread-start! thread)
        (remote-listener-server-thread-set! self thread)))))


(define (remote-listener-listening-host self)
  (socket-info-address (remote-listener-socket-info self)))


(define (remote-listener-listening-port self)
  (socket-info-port-number (remote-listener-socket-info self)))


;;;
;;;; Connection
;;;


(define (remote-listener-accept-connection self port)
  (remote-presence-accept-remote (remote-listener-presence self) port))
