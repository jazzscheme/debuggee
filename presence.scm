;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Presence
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


;; OVERVIEW
;;   Socket is persistent between two connected processes
;;   and messages and responses travel through the bidirectional
;;   socket pipe. Initial connection is done with connect-uuid
;;
;; LEXICON
;;   uuid
;;     used to uniquely identify processes
;;   ior
;;     marshalled object sent remotely
;;   listener
;;     tcp server receiving a connection request
;;     and that starts the pump called remote-connection
;;   connection
;;     waits on socket for messages client and server side
;;   stub
;;     interface common to local and remote objects
;;   proxy local / remote
;;     objects implementing transparent communication
;;   register
;;     used to register remotely accessible objects
;;
;; CALL
;;   local <-> remote call
;;     local code calls invoke from some thread
;;       message is written to the connection port
;;         thread then blocks on mutex
;;           remote reads message and executes it
;;           result is written back to connection port
;;       connection reads result and unlocks mutex
;;     thread continues execution


;;;
;;;; Presences
;;;


(define default-presence-priority
  0.)


(define presence-priorities
  (make-table test: eq?))

(define (set-presence-priority purpose priority)
  (table-set! presence-priorities purpose priority))

(define (get-presence-priority purpose)
  (table-ref presence-priorities purpose default-presence-priority))


;; quick hack until priorities in package metadata
(define debugging-priority 500.)
(define working-priority   400.)
(define presence-priority  100.)

(set-presence-priority 'debugging debugging-priority)
(set-presence-priority 'working working-priority)
(set-presence-priority #f presence-priority)


(define presences
  (make-table test: eq?))

(define uuid-presences
  (make-table test: equal?))


(define presences-mutex
  (make-mutex 'presences))

(define (with-presences-mutex thunk)
  (dynamic-wind
    (lambda ()
      (mutex-lock! presences-mutex))
    thunk
    (lambda ()
      (mutex-unlock! presences-mutex))))


(define (find-presence purpose)
  (with-presences-mutex
    (lambda ()
      (table-ref presences purpose #f))))


(define (require-presence purpose)
  (with-presences-mutex
    (lambda ()
      (or (table-ref presences purpose #f)
          (let ((presence (new-presence purpose)))
            (table-set! presences purpose presence)
            (table-set! uuid-presences (presence-uuid presence) presence)
            presence)))))


(define (find-presence-by-uuid uuid)
  (with-presences-mutex
    (lambda ()
      (table-ref uuid-presences uuid #f))))


(define (iterate-presences proc)
  (with-presences-mutex
    (lambda ()
      (iterate-table presences
        (lambda (purpose presence)
          (proc presence))))))


(define (collect-presences)
  (let ((presences '()))
    (iterate-presences
      (lambda (presence)
        (set! presences (cons presence presences))))
    presences))


;;;
;;;; Presence
;;;


(define-type-of-object presence
  unprintable:
  purpose
  uuid
  priority
  register
  listener
  connections
  connections-mutex
  code
  version
  already-connected
  accept-handler
  connect-handler
  invoke-handler
  process-handler
  processing-handler
  execute-handler)


(define (new-presence purpose)
  (let ((presence
          (make-presence
            'presence
            #f
            purpose
            (make-uuid)
            (get-presence-priority purpose)
            #f
            #f
            (make-table)
            (make-mutex 'connections)
            *presence-code*
            *presence-version*
            #f
            (lambda (proc presence) (proc presence))
            (lambda (proc presence) (proc presence))
            (lambda (proc connection) (proc connection))
            (lambda (proc connection) (proc connection))
            #f
            (lambda (proc connection) (proc connection)))))
    (setup-presence presence)
    presence))


(define (setup-presence presence)
  (setup-object presence)
  (presence-register-set! presence (new-register-local-proxy presence (new-register))))


(define (presence-print presence output readably)
  (print-unreadable presence output
    (lambda (output)
      (format output "{a} {a}" purpose uuid))))


;;;
;;;; Listener
;;;


(define (presence-start-listener presence host service alternate-service)
  (if (not (presence-listener presence))
      (receive (listen-host listen-service listen-alternate-service) (listen-parameter)
        (let ((host (or host listen-host default-listener-host))
              (service (or service listen-service default-listener-service))
              (alternate-service (or alternate-service listen-alternate-service)))
          (let ((server (new-listener presence host: host service: service alternate-service: alternate-service)))
            (presence-listener-set! presence server)
            (listener-start server 'presence-listener))))))


(define (presence-stop-listener presence)
  (let ((listener (presence-listener presence)))
    (if listener
        (begin
          (stop listener)
          (presence-listener-set! presence #f)))))


;;;
;;;; Accept
;;;


(define (presence-accept-remote presence port)
  ((presence-accept-handler presence)
    (lambda (presence)
      (let ((uuid (presence-uuid presence))
            (code (presence-code presence))
            (version (presence-version presence)))
        (let ((message
                ;; only reply to an invalid code/version here in accept
                ;; as always replying could be used maliciously to try
                ;; and figure out the communication protocol. also note
                ;; that by simply replying, the client will report the
                ;; right invalid code/version error
                (catch (invalid-code? exc
                         (write-port port code version #f)
                         (raise exc))
                  (catch (invalid-version? exc
                           (write-port port code version #f)
                           (raise exc))
                    (read-port port code version)))))
          (bind (remote-kind remote-uuid remote-title remote-service remote-address reference) message
            (define (accept)
              (let ((local-uuid uuid)
                    (local-title presence-title)
                    (local-service (listener-listening-port (presence-listener presence)))
                    (local-address (socket-info-address (tcp-client-peer-socket-info port)))
                    (reference-proxy (and reference (load-reference (presence-register presence) reference))))
                (write-port port code version (list local-uuid local-title local-service local-address reference-proxy))
                (if (not (equal? (read-port port code version) '(handshake)))
                    (error "Fatal")
                  (let ((invoke-handler (presence-invoke-handler presence))
                        (process-handler (presence-process-handler presence))
                        (processing-handler (presence-processing-handler presence))
                        (execute-handler (presence-execute-handler presence)))
                    (let ((connection (new-connection presence port remote-uuid remote-title remote-service remote-address #f invoke-handler process-handler processing-handler execute-handler)))
                      (if (and debug-remote? (debug-presence? presence))
                          (begin
                            (callee-garble-hack)
                            (debug-remote presence '<<< remote-title remote-uuid 'accept connection)))
                      (presence-register-connection presence remote-uuid connection)
                      (connection-thread-set! connection (current-thread))
                      (connection-process connection))))))
            
            (let ((existing-connection (presence-find-connection presence remote-uuid)))
              (if existing-connection
                  (if already-connected
                      (already-connected presence remote-uuid existing-connection accept)
                    (write-port port code version 'already-connected))
                (accept)))))))
    presence))


;;;
;;;; Connect
;;;


(define (presence-connect-remote presence host service reference)
  (if (and (or debug-remote? debug-remote-blocking?) (debug-presence? presence))
      (begin
        (debug-remote presence '>>> #f #f 'open host service)
        (caller-garble-hack)))
  (let ((listener (presence-listener presence))
        (code (presence-code presence))
        (version (presence-version presence))
        (uuid (presence-uuid presence)))
    (if (not listener)
        (presence-start-listener presence #f #f #f))
    ((presence-connect-handler presence)
      (lambda (presence)
        (let ((port (open-tcp-client (list server-address: host port-number: service))))
          (let ((local-uuid uuid)
                (local-title presence-title)
                (local-service (listener-listening-port listener))
                (local-address (socket-info-address (tcp-client-peer-socket-info port))))
            (write-port port code version (list 'connect local-uuid local-title local-service local-address reference))
            (let ((reply (read-port port code version)))
              (cond ((eof-object? reply)
                     (throw-connection-broke (format "Connecting to {a} {a} received eof" host service)))
                    ((eq? reply 'already-connected)
                     (error "Already connected to" host service))
                    (else
                     (bind (remote-uuid remote-title remote-service remote-address reference-proxy) reply
                       (write-port port code version '(handshake))
                       (let ((invoke-handler (presence-invoke-handler presence))
                             (process-handler (presence-process-handler presence))
                             (processing-handler (presence-processing-handler presence))
                             (execute-handler (presence-execute-handler presence)))
                         (let ((connection (new-connection presence port remote-uuid remote-title remote-service remote-address #f invoke-handler process-handler processing-handler execute-handler)))
                           (if (and debug-remote? (debug-presence? presence))
                               (begin
                                 (callee-garble-hack)
                                 (callee-garble-hack)
                                 (debug-remote presence '<<< remote-title remote-uuid 'connect)))
                           (presence-register-connection presence remote-uuid connection)
                           (let ((thread (make-thread
                                           (lambda ()
                                             (connection-process connection))
                                           'presence-connected)))
                             (connection-thread-set! connection thread)
                             (thread-base-priority-set! thread (presence-priority presence))
                             (thread-start! thread)
                             (or reference-proxy remote-uuid)))))))))))
      presence)))


;;;
;;;; Connections
;;;


(define (presence-with-connections-mutex presence thunk)
  (let ((connections-mutex (presence-connections-mutex presence)))
    (dynamic-wind
      (lambda ()
        (mutex-lock! connections-mutex))
      thunk
      (lambda ()
        (mutex-unlock! connections-mutex)))))


(define (presence-register-connection presence remote-uuid connection)
  (presence-with-connections-mutex presence
    (lambda ()
      (table-set! (presence-connections presence) remote-uuid connection))))


(define (presence-find-connection presence remote-uuid)
  (presence-with-connections-mutex presence
    (lambda ()
      (table-ref (presence-connections presence) remote-uuid #f))))


(define (presence-require-connection presence remote-uuid)
  (or (presence-find-connection presence remote-uuid)
      (throw-connection-broke (format "Unable to find connection: {a}" remote-uuid))))


(define (presence-close-connection presence remote-uuid connection)
  (presence-with-connections-mutex presence
    (lambda ()
      (table-clear (presence-connections presence) remote-uuid)
      (connection-close connection))))


(define (presence-disconnect-connection presence remote-uuid connection)
  (presence-with-connections-mutex presence
    (lambda ()
      (table-clear (presence-connections presence) remote-uuid)
      (connection-disconnect connection))))


;;;
;;;; Debug
;;;


(define (debug-remote presence arrow remote-title remote-uuid action . rest)
  (format (current-output-port) "{a} {a} {a}   {a}   {a} {a}   {a}   {l}{%}"
    (presence-purpose presence)
    presence-title
    (uuid-prefix (presence-uuid presence))
    arrow
    (or remote-title "")
    (if remote-uuid (uuid-prefix remote-uuid) "")
    action
    rest))


(define (debug-simplify presence obj)
  (cond ((not debug-remote-simplify?)
         obj)
        ((atom? obj)
         obj)
        ((object? obj)
         (object-class obj))
        (else
         (object->serial obj))))


;;;
;;;; Connection
;;;


(define-type-of-object connection
  presence
  port
  thread
  write-mutex
  remote-uuid
  remote-title
  service   ; the peer's listener port
  address   ; the peer's address of us
  invocations
  invocations-mutex
  lag
  closing?
  invoke-handler
  process-handler
  processing-handler
  execute-handler)


(define (new-connection presence port remote-uuid remote-title service address lag invoke-handler process-handler processing-handler execute-handler)
  (let ((connection
          (make-connection
            'connection
            #f
            presence
            port
            #f
            (make-mutex 'write)
            remote-uuid
            remote-title
            service
            address
            '()
            (make-mutex 'invocations)
            lag
            #f
            invoke-handler
            process-handler
            processing-handler
            execute-handler)))
    (setup-connection connection)
    connection))


(define (setup-connection connection)
  (setup-object connection))


(define (connection-close connection)
  (connection-disconnect connection))


(define (connection-disconnect connection)
  ;; robust
  (with-exception-catcher
    (lambda (exc)
      #f)
    (lambda ()
      (close-port port)))
  (connection-sever-invocations connection))


(define (connection-print connection output readably)
  (print-unreadable connection output
    (lambda (output)
      (format output "{a}" remote-title))))


(define (connection-register-invocation connection mutex)
  (let ((invocations-mutex (connection-invocations-mutex connection)))
    (mutex-lock! invocations-mutex)
    (connection-invocations-set! connection (cons mutex (connection-invocations connection)))
    (mutex-unlock! invocations-mutex)))


(define (connection-unregister-invocation connection mutex)
  (let ((invocations-mutex (connection-invocations-mutex connection)))
    (mutex-lock! invocations-mutex)
    (connection-invocations-set! connection (remove mutex (connection-invocations connection)))
    (mutex-unlock! invocations-mutex)))


(define (connection-sever-invocations connection)
  (let ((invocations-mutex (connection-invocations-mutex connection)))
    (mutex-lock! invocations-mutex)
    (for-each (lambda (mutex)
                (mutex-specific-set! mutex (new-connection-broke "Invocation severed"))
                (mutex-unlock! mutex))
              (connection-invocations connection))
    (mutex-unlock! invocations-mutex)))


;;;
;;;; Process
;;;


(define (connection-process connection)
  ((connection-process-handler connection)
    (lambda (connection)
      (continuation-capture
        (lambda (exit)
          (let loop ()
            (let ((message (catch-exception-filter
                             (lambda (exc)
                               (connection-closing? connection))
                             (lambda (exc)
                               (let ((presence (connection-presence connection))
                                     (remote-uuid (connection-remote-uuid connection)))
                                 (if (presence-find-connection presence remote-uuid)
                                     (presence-close-connection presence remote-uuid connection)))
                               (continuation-return exit #f))
                             (lambda ()
                               (let ((processing-handler (connection-processing-handler connection)))
                                 (if processing-handler
                                     (processing-handler)))
                               (connection-read-message connection)))))
              (bind (kind . rest) message
                (case kind
                  ((post send call)
                   (connection-execute connection kind rest))
                  ((result)
                   (connection-result connection rest)))))
            (loop)))))
    connection))


;;;
;;;; Invoke
;;;


(define (connection-invoke connection kind method-name remote-proxy arguments)
  (let ((presence (connection-presence connection)))
    (if (and (or debug-remote? (and debug-remote-blocking? (not (eq? kind 'post)))) (debug-presence? presence) (debug-remote-method? method-name))
        (begin
          (debug-remote presence '>>> (connection-remote-title connection) (connection-remote-uuid connection) kind method-name)
          (caller-garble-hack))))
  (let ((ior (remote-proxy-ior remote-proxy)))
    ((connection-invoke-handler connection)
      (lambda (connection)
        (if (eq? kind 'post)
            (connection-write-message connection `(,kind ,method-name #f ,ior ,arguments))
          (let ((mutex (make-mutex 'invoke)))
            (let ((cookie (object->serial mutex)))
              (mutex-lock! mutex)
              (connection-write-message connection `(,kind ,method-name ,cookie ,ior ,arguments))
              (connection-register-invocation connection mutex)
              (mutex-lock! mutex)
              (mutex-unlock! mutex)
              (connection-unregister-invocation connection mutex)
              (let ((result (mutex-specific mutex)))
                (if (connection-broke-exception? result)
                    (raise result)
                  result))))))
      connection)))


(define (connection-invoke-live? connection remote-proxy timeout)
  (let ((ior (remote-proxy-ior remote-proxy)))
    (let ((mutex (make-mutex 'invoke)))
      (let ((cookie (object->serial mutex)))
        (mutex-lock! mutex)
        (connection-write-message connection `(call live? ,cookie ,ior ()))
        (let ((timed-out (not (mutex-lock! mutex timeout))))
          (mutex-unlock! mutex)
          (if timed-out
              #f
            (mutex-specific mutex)))))))


(define (connection-result connection rest)
  (bind (kind method-name cookie result) rest
    (let ((presence (connection-presence connection)))
      (if (and debug-remote? (debug-presence? presence) (debug-remote-method? method-name))
          (begin
            (if (eq? kind 'call)
                (debug-remote presence '<<< (connection-remote-title connection) (connection-remote-uuid connection) 'result method-name (debug-simplify presence result))
              (debug-remote presence '<<< (connection-remote-title connection) (connection-remote-uuid connection) 'return method-name))
            (caller-garble-hack))))
    (let ((mutex (serial->object cookie)))
      (mutex-specific-set! mutex result)
      (mutex-unlock! mutex))))


;;;
;;;; Execute
;;;


(define (connection-execute connection kind rest)
  (bind (method-name cookie local-proxy arguments) rest
    (let ((presence (connection-presence connection)))
      (if (and debug-remote? (debug-presence? presence) (debug-remote-method? method-name))
          (begin
            (callee-garble-hack)
            (debug-remote presence '<<< (connection-remote-title connection) (connection-remote-uuid connection) 'execute method-name))))
    (let ((thread
            (make-thread
              (lambda ()
                (let ((result (apply (dispatch local-proxy method-name) local-proxy arguments)))
                  ((connection-execute-handler connection)
                    (lambda (connection)
                      (case kind
                        ((send)
                         (connection-write-message connection `(result ,kind ,method-name ,cookie ,(unspecified))))
                        ((call)
                         (connection-write-message connection `(result ,kind ,method-name ,cookie ,result)))))
                   connection)))
              (list 'execute method-name))))
      (thread-base-priority-set! thread (presence-priority (connection-presence connection)))
      (thread-start! thread))))


;;;
;;;; IO
;;;


(define (connection-write-message connection message)
  (let ((write-mutex (connection-write-mutex connection)))
    (dynamic-wind
      (lambda ()
        #f)
      (lambda ()
        (let ((presence (connection-presence connection))
              (port (connection-port connection)))
          (mutex-lock! write-mutex)
          (if (and debug-remote-io? (debug-presence? presence))
              (begin
                (debug-remote presence '>>> (connection-remote-title connection) (connection-remote-uuid connection) 'write (car message) (cadr message) (caddr message))
                (caller-garble-hack)))
          (write-port port (presence-code presence) (presence-version presence) message)))
      (lambda ()
        (mutex-unlock! write-mutex)))))


(define (connection-read-message connection)
  (let ((presence (connection-presence connection))
        (port (connection-port connection)))
    (let ((message (read-port port (presence-code presence) (presence-version presence))))
      (if (and debug-remote-io? (debug-presence? presence))
          (begin
            (callee-garble-hack)
            (debug-remote presence '<<< (connection-remote-title connection) (connection-remote-uuid connection) 'read (car message) (cadr message) (caddr message))))
      (if (eof-object? message)
          (throw-connection-broke "Read message received eof")
        message))))


;;;
;;;; Listener
;;;


(define default-listener-host
  localhost)

(define default-listener-service
  'any)


(define (get-remote-listener #!key (purpose #f))
  (get-listener (require-presence purpose)))


(define (require-remote-listener #!key (purpose #f))
  (or (get-remote-listener purpose: purpose)
      (error "Unable to complete operation because the remote listener is not running")))


(define (listen-parameter)
  (host/service-parameter "listen" 'listen (values #f #f #f)))


(define (host/service-parameter arg setting #!optional (default (unspecified)))
  (parse-parameter arg parse-host/service setting parse-host/service default))


(define (start-presence #!key (purpose #f) (host #f) (service #f) (alternate-service #f))
  (let ((presence (require-presence purpose)))
    (presence-start-listener presence host service alternate-service)))


(define (stop-presence #!key (purpose #f))
  (let ((presence (find-presence purpose)))
    (if presence
        (presence-stop-listener presence))))


;;;
;;;; Invoke
;;;


(define (post-remote method-name remote-proxy . arguments)
  (let ((connection (remote-proxy->connection remote-proxy)))
    (connection-invoke connection 'post method-name remote-proxy arguments)))


(define (send-remote method-name remote-proxy . arguments)
  (let ((connection (remote-proxy->connection remote-proxy)))
    (connection-invoke connection 'send method-name remote-proxy arguments)))


(define (call-remote method-name remote-proxy . arguments)
  (let ((connection (remote-proxy->connection remote-proxy)))
    (connection-invoke connection 'call method-name remote-proxy arguments)))


;;;
;;;; Live
;;;


(define live-timeout
  0.5)


(define (remote-proxy-live? remote-proxy)
  #f
  #; ;; testing
  (catch (connection-exception? exc
           #f)
    (let ((connection (proxy->connection remote-proxy)))
      (invoke-live? connection remote-proxy live-timeout))))


;;;
;;;; Validation
;;;


(define (set-presence-code code #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-code presence code)))

(define (set-presence-version version #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-version presence version)))


;;;
;;;; Handler
;;;


(define (set-presence-already-connected already-connected #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-already-connected presence already-connected)))

(define (set-presence-accept-handler accept-handler #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-accept-handler presence accept-handler)))

(define (set-presence-connect-handler connect-handler #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-connect-handler presence connect-handler)))


(define (set-presence-invoke-handler invoke-handler #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-invoke-handler presence invoke-handler)))

(define (set-presence-process-handler process-handler #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-process-handler presence process-handler)))

(define (set-presence-processing-handler processing-handler #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-processing-handler presence processing-handler)))

(define (set-presence-execute-handler execute-handler #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (set-execute-handler presence execute-handler)))


(define (reset-presence-process-handler #!key (purpose #f))
  (set-presence-process-handler
    (lambda (proc connection)
      (proc connection))
    purpose: purpose))

(define (reset-presence-processing-handler #!key (purpose #f))
  (set-presence-processing-handler #f purpose: purpose))


;;;
;;;; Register
;;;


(define (local-register #!optional (purpose #f))
  (presence-register (require-presence purpose)))


(define (new-remote-register purpose uuid)
  (ior->proxy (new-ior purpose uuid 'register #f '())))


(define (connect-remote-register host port #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (let ((uuid (presence-connect-remote presence host port #f)))
      (new-remote-register purpose uuid))))


(define (connect-remote-reference host port reference #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (presence-connect-remote presence host port reference)))


(define (get-remote-register remote-proxy)
  (let ((ior (remote-proxy-ior remote-proxy)))
    (let ((purpose (ior-purpose ior))
          (uuid (ior-uuid ior)))
      (let ((presence (require-presence purpose)))
        (new-remote-register purpose uuid)))))


(define (register-proxy name proxy-class object #!optional (purpose #f))
  (let ((presence (require-presence purpose)))
    (let ((register (local-register purpose))
          (proxy (new proxy-class presence object)))
      (register-proxy-register-object register name proxy)
      proxy)))


(define (proxy-connection-info client proxy)
  (let ((client-presence (get-presence client))
        (client-uuid (ior-uuid (get-ior client))))
    (let ((client-connection (presence-require-connection client-presence client-uuid)))
      (let ((host (get-address client-connection)))
        (if (local-proxy? proxy)
            (let ((port (listener-listening-port (get-remote-listener))))
              (list proxy host port))
          (let ((proxy-presence (proxy-presence proxy))
                (proxy-uuid (ior-uuid (remote-proxy-ior proxy))))
            (let ((proxy-connection (presence-require-connection proxy-presence proxy-uuid)))
              (let ((port (get-service proxy-connection)))
                ;; we cannot use the peer port number because if it was him that connected
                ;; to us then it will have a distinct port from its process remote listener
                (if (not port)
                    (error "Fatal")
                  (list proxy host port))))))))))


(define (connect-remote-proxy info)
  (bind (proxy host port) info
    (let ((presence (proxy-presence proxy))
          (ior (remote-proxy-ior proxy)))
      (let ((uuid (ior-uuid ior)))
        ;; already connected
        (if (not (presence-find-connection presence uuid))
            (if (not (uuid=? (presence-connect-remote presence host port #f)
                             uuid))
                (error "Fatal")))
        proxy))))


(define (disconnect-remote-proxy remote-proxy)
  (let ((presence (proxy-presence remote-proxy))
        (uuid (ior-uuid (remote-proxy-ior remote-proxy))))
    (let ((connection (presence-find-connection presence uuid)))
      ;; robust
      (if connection
          (begin
            (connection-closing?-set! connection #t)
            (disconnect-connection presence uuid connection))))))


(define (close-remote-proxy remote-proxy)
  (let ((presence (proxy-presence remote-proxy))
        (uuid (ior-uuid (remote-proxy-ior remote-proxy))))
    (let ((connection (presence-require-connection presence uuid)))
      (connection-closing?-set! connection #t)
      (close-connection presence uuid connection))))


(define (closing-remote-proxy remote-proxy)
  (let ((presence (proxy-presence remote-proxy))
        (uuid (ior-uuid (remote-proxy-ior remote-proxy))))
    (let ((connection (presence-require-connection presence uuid)))
      (connection-closing?-set! connection #t))))


;; quick safe version
(define (closing-remote-proxy-safe remote-proxy)
  (let ((presence (proxy-presence remote-proxy))
        (uuid (ior-uuid (remote-proxy-ior remote-proxy))))
    (let ((connection (presence-find-connection presence uuid)))
      (if connection
          (connection-closing?-set! connection #t)))))


;; quick safe version
(define (closing-remote-connection-safe purpose uuid)
  (let ((presence (require-presence purpose)))
    (let ((connection (presence-find-connection presence uuid)))
      (if connection
          (connection-closing?-set! connection #t)))))


;;;
;;;; IOR
;;;


(define (ior=? x y)
  (and (uuid=? (ior-uuid x) (ior-uuid y))
       (reference=? (ior-reference x) (ior-reference y))))


(define (ior-server=? x y)
  (uuid=? (ior-uuid x) (ior-uuid y)))


(define (host=? x y)
  (equal? x y))


(define (service=? x y)
  (eqv? x y))


(define (reference=? x y)
  (eqv? x y))


(define (ior->proxy ior)
  (let ((purpose (ior-purpose ior)))
    (let ((presence (require-presence purpose)))
      (define (local-ior? ior)
        (find-presence-by-uuid (ior-uuid ior)))
      
      (define (local->proxy stub-interface ior)
        (define (reference->local-proxy stub-interface reference)
          (if (not reference)
              (local-register purpose)
            ((local-proxy-class stub-interface) presence (serial->object reference))))
        
        (reference->local-proxy stub-interface (ior-reference ior)))
      
      (define (remote->proxy stub-interface ior)
        ((remote-proxy-class stub-interface) presence ior (ior-values ior)))
      
      (let ((stub-interface (ior-stub-interface ior)))
        (if (local-ior? ior)
            (local->proxy stub-interface ior)
          (remote->proxy stub-interface ior))))))


;;;
;;;; Proxy
;;;


(define (proxy=? x y)
  (cond ((and (remote-proxy? x) (remote-proxy? y))
         (remote-proxy=? x y))
        ((and (local-proxy? x) (local-proxy? y))
         (local-proxy=? x y))
        (else
         #f)))


(define (proxy-server=? x y)
  (cond ((and (remote-proxy? x) (remote-proxy? y))
         (ior-server=? (remote-proxy-ior x)
                       (remote-proxy-ior y)))
        ((and (local-proxy? x) (local-proxy? y))
         #t)
        (else
         #f)))


;;;
;;;; Local Proxy
;;;


(define (local-proxy=? x y)
  (eq? (local-proxy-object x)
       (local-proxy-object y)))


;;;
;;;; Remote Proxy
;;;


(define (remote-proxy=? x y)
  (ior=? (remote-proxy-ior x)
         (remote-proxy-ior y)))


(define (remote-proxy->connection proxy)
  (let ((presence (proxy-presence proxy)))
    (presence-require-connection presence (ior-uuid (remote-proxy-ior proxy)))))


(define (remote-proxy-connected? proxy)
  (let ((presence (proxy-presence proxy)))
    (presence-find-connection presence (ior-uuid (remote-proxy-ior proxy)))))


;;;
;;;; GC
;;;


;; Put back temporarily until all the bugs of
;; the new pragmatic approach are ironed out...


(define *Mega-Patch*
  '())


(define (gc-protect obj)
  (set! *Mega-Patch* (cons obj *Mega-Patch*)))


;;;
;;;; Marshall
;;;


(define (marshall-local-proxy proxy)
  (let ((presence (proxy-presence proxy)))
    (define (local-proxy->reference)
      (if (proxy=? proxy (presence-register presence))
          #f
        (let ((object (local-proxy-object proxy)))
          (gc-protect object)
          (object->serial object))))
    
    (let ((ior
            (new-ior
              (presence-purpose presence)
              (presence-uuid presence)
              (proxy-stub proxy)
              (local-proxy->reference)
              (local-proxy-values proxy))))
      (serialize-object (object-class ior) (encode-ior ior)))))


(define (marshall-remote-proxy proxy)
  (let ((ior (remote-proxy-ior proxy)))
    (serialize-object (object-class ior) (encode-ior ior))))


(define (encode-ior ior)
  (vector (ior-purpose ior)
          (ior-uuid ior)
          (ior-stub-interface ior)
          (ior-reference ior)
          (ior-values ior)))


;;;
;;;; Unmarshall
;;;


(define (unmarshall-proxy content)
  (ior->proxy (decode-ior content)))


(define (decode-ior content)
  (new-ior
    (vector-ref content 0)
    (vector-ref content 1)
    (vector-ref content 2)
    (vector-ref content 3)
    (vector-ref content 4)))


;;;
;;;; Debug
;;;


;; todo
(define (global-setting name default)
  default)


(define presence-title
  (let ((prefix (command-argument "title")))
    (and prefix (upcase prefix))))

(define (get-presence-title)
  presence-title)

(define (set-presence-title title)
  (set! presence-title title))


;; kinda quicky
(define presence-name
  (and presence-title (string->symbol presence-title)))

(define (get-presence-name)
  presence-name)


(define debug-remote?
  (global-setting 'debug-remote? #f))

(define debug-remote-blocking?
  (global-setting 'debug-remote-blocking? #f))

(define debug-remote-io?
  (global-setting 'debug-remote-io? #f))

(define debug-remote-seconds?
  (global-setting 'debug-remote-seconds? #f))

(define debug-remote-simplify?
  (global-setting 'debug-remote-simplify? #t))

(define debug-remote-ignore-presences
  (global-setting 'debug-remote-ignore-presences #f))

(define debug-remote-ignore-methods
  (global-setting 'debug-remote-ignore-methods #f))


(define (debug-presence? presence)
  (or (not debug-remote-ignore-presences)
      (not (memq (presence-purpose presence) debug-remote-ignore-presences))))


(define (debug-remote-method? method-name)
  (or (not debug-remote-ignore-methods)
      (not (memq method-name debug-remote-ignore-methods))))


(define (set-debug-remote? flag)
  (set! debug-remote? flag))

(define (set-debug-remote-blocking? flag)
  (set! debug-remote-blocking? flag))

(define (set-debug-remote-io? flag)
  (set! debug-remote-io? flag))


(define (caller-garble-hack)
  ;; quick hack to help output from getting garbled
  (thread-sleep! .01))


(define (callee-garble-hack)
  ;; quick hack to help output from getting garbled
  (thread-sleep! .01))
