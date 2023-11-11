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


(define-type presence
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
    (presence-register-set! presence (new-register-proxy presence))
    presence))


(define (presence-print self output readably)
  (print-unreadable self output
    (lambda (output)
      (format output "{a} {a}" purpose uuid))))


;;;
;;;; Listener
;;;


(define (presence-start-listener self host service alternate-service)
  (if (not (presence-listener self))
      (receive (listen-host listen-service listen-alternate-service) (listen-parameter)
        (let ((host (or host listen-host default-listener-host))
              (service (or service listen-service default-listener-service))
              (alternate-service (or alternate-service listen-alternate-service)))
          (let ((server (new-listener self host: host service: service alternate-service: alternate-service)))
            (presence-listener-set! self server)
            (listener-start server))))))


(define (presence-stop-listener self)
  (let ((listener (presence-listener self)))
    (if listener
        (begin
          (stop listener)
          (presence-listener-set! self #f)))))


;;;
;;;; Accept
;;;


(define (presence-accept-remote self port)
  ((presence-accept-handler self)
    (lambda (presence)
      (let ((uuid (presence-uuid self))
            (code (presence-code self))
            (version (presence-version self)))
        (let ((message
                ;; only reply to an invalid code/version here in accept
                ;; as always replying could be used maliciously to try
                ;; and figure out the communication protocol. also note
                ;; that by simply replying, the client will report the
                ;; right invalid code/version error
                (catch (invalid-code? exc
                         (write-port port code version #f)
                         (signal exc))
                  (catch (invalid-version? exc
                           (write-port port code version #f)
                           (signal exc))
                    (read-port port code version)))))
          (bind (remote-kind remote-uuid remote-title remote-service remote-address reference) message
            (define (accept)
              (let ((local-uuid uuid)
                    (local-title presence-title)
                    (local-service (listener-listening-port (presence-listener self)))
                    (local-address (socket-info-address (tcp-client-peer-socket-info port)))
                    (start (current-monotonic))
                    (reference-proxy (and reference (load-reference (presence-register self) reference))))
                (write-port port code version (list local-uuid local-title local-service local-address reference-proxy))
                (if (not (eq? (read-port port code version) 'handshake))
                    (error "Fatal")
                  (let ((lag (- (current-monotonic) start)))
                    (let ((invoke-handler (presence-invoke-handler self))
                          (process-handler (presence-process-handler self))
                          (processing-handler (presence-processing-handler self))
                          (execute-handler (presence-execute-handler self)))
                      (let ((connection (new-connection self port remote-uuid remote-title remote-service remote-address lag invoke-handler process-handler processing-handler execute-handler)))
                        (if (and debug-remote? (debug-presence? self))
                            (begin
                              (callee-garble-hack)
                              (debug-remote self '<<< remote-title remote-uuid 'accept connection)))
                        (presence-register-connection self remote-uuid connection)
                        (connection-thread-set! connection (current-thread))
                        (connection-process connection)))))))
            
            (let ((existing-connection (presence-find-connection self remote-uuid)))
              (if existing-connection
                  (if already-connected
                      (already-connected self remote-uuid existing-connection accept)
                    (write-port port code version 'already-connected))
                (accept)))))))
    self))


;;;
;;;; Connect
;;;


(define (presence-connect-remote self host service reference)
  (if (and (or debug-remote? debug-remote-blocking?) (debug-presence? self))
      (begin
        (debug-remote self '>>> #f #f 'open host service)
        (caller-garble-hack)))
  (let ((listener (presence-listener self))
        (code (presence-code self))
        (version (presence-version self))
        (uuid (presence-uuid self)))
    (if (not listener)
        (start-listener self #f #f #f))
    ((presence-connect-handler self)
      (lambda (presence)
        (let ((port (open-tcp-client (list server-address: host port-number: service))))
          (let ((local-uuid uuid)
                (local-title presence-title)
                (local-service (listener-listening-port listener))
                (local-address (socket-info-address (tcp-client-peer-socket-info port)))
                (start (current-monotonic)))
            (write-port port code version (list 'connect local-uuid local-title local-service local-address reference))
            (let ((reply (read-port port code version)))
              (cond ((eof-object? reply)
                     (throw-connection-broke (format "Connecting to {a} {a} received eof" host service)))
                    ((eq? reply 'already-connected)
                     (error "Already connected to" host service))
                    (else
                     (bind (remote-uuid remote-title remote-service remote-address reference-proxy) reply
                       (let ((lag (- (current-monotonic) start)))
                         (write-port port code version 'handshake)
                         (let ((invoke-handler (presence-invoke-handler self))
                               (process-handler (presence-process-handler self))
                               (processing-handler (presence-processing-handler self))
                               (execute-handler (presence-execute-handler self)))
                           (let ((connection (new-connection self port remote-uuid remote-title remote-service remote-address lag invoke-handler process-handler processing-handler execute-handler)))
                             (if (and debug-remote? (debug-presence? self))
                                 (begin
                                   (callee-garble-hack)
                                   (callee-garble-hack)
                                   (debug-remote self '<<< remote-title remote-uuid 'connect connection)))
                             (presence-register-connection self remote-uuid connection)
                             (let ((thread (make-thread
                                             (lambda ()
                                               (connection-process connection))
                                             'presence-connected)))
                               (connection-thread-set! connection thread)
                               (thread-base-priority-set! thread (presence-priority self))
                               (thread-start! thread)
                               (or reference-proxy remote-uuid))))))))))))
      self)))


;;;
;;;; Connections
;;;


(define (presence-with-connections-mutex self thunk)
  (let ((connections-mutex (presence-connections-mutex self)))
    (dynamic-wind
      (lambda ()
        (mutex-lock! connections-mutex))
      thunk
      (lambda ()
        (mutex-unlock! connections-mutex)))))


(define (presence-register-connection self remote-uuid connection)
  (presence-with-connections-mutex self
    (lambda ()
      (table-set! (presence-connections self) remote-uuid connection))))


(define (presence-find-connection self remote-uuid)
  (presence-with-connections-mutex self
    (lambda ()
      (table-ref (presence-connections self) remote-uuid #f))))


(define (presence-require-connection self remote-uuid)
  (or (presence-find-connection self remote-uuid)
      (throw-connection-broke (format "Unable to find connection: {a}" remote-uuid))))


(define (presence-close-connection self remote-uuid connection)
  (presence-with-connections-mutex self
    (lambda ()
      (table-clear (presence-connections self) remote-uuid)
      (close connection))))


(define (presence-disconnect-connection self remote-uuid connection)
  (presence-with-connections-mutex self
    (lambda ()
      (table-clear (presence-connections self) remote-uuid)
      (disconnect connection))))


;;;
;;;; Debug
;;;


(define (presence-debug-remote self arrow remote-title remote-uuid action . rest)
  (if debug-remote-seconds?
      (format :terminal "{r precision: 6} " (current-seconds)))
  (format :terminal "{?{a} ~}{a width: 9} {a}   {a}   {a width: 9} {a width: 8}   {a width: 7}   {l}{%}" purpose presence-title (uuid-prefix uuid) arrow (or remote-title "") (if remote-uuid (uuid-prefix remote-uuid) "") action rest))


(define (presence-debug-simplify self obj)
  (cond ((not debug-remote-simplify?)
         obj)
        ((atom? obj)
         obj)
        ((object? obj)
         (category-name (class-of obj)))
        (else
         (type->specifier (class-of obj)))))


;;;
;;;; Connection
;;;


(define-type connection
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
  (make-connection
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
    execute-handler))


(define (connection-destroy self)
  (disconnect self)
  (nextmethod self))


(define (connection-disconnect self)
  ;; robust
  (with-exception-catcher
    (lambda (exc)
      #f)
    (lambda ()
      (close-port port)))
  (sever-invocations self))


(define (connection-print self output readably)
  (print-unreadable self output
    (lambda (output)
      (format output "{a}" remote-title))))


(define (connection-register-invocation self mutex)
  (mutex-lock! invocations-mutex)
  (set! invocations (cons mutex invocations))
  (mutex-unlock! invocations-mutex))


(define (connection-unregister-invocation self mutex)
  (mutex-lock! invocations-mutex)
  (set! invocations (remove! mutex invocations))
  (mutex-unlock! invocations-mutex))


(define (connection-sever-invocations self)
  (mutex-lock! invocations-mutex)
  (for-each (lambda (mutex)
              (mutex-specific-set! mutex (new-connection-broke "Invocation severed"))
              (mutex-unlock! mutex))
            invocations)
  (mutex-unlock! invocations-mutex))


;;;
;;;; Process
;;;


(define (connection-process self)
  ((connection-process-handler self)
    (lambda (connection)
      (continuation-capture
        (lambda (exit)
          (let loop ()
            (let ((message (catch-exception-filter
                             (lambda (exc)
                               (connection-closing? self))
                             (lambda (exc)
                               (if (presence-find-connection presence remote-uuid)
                                   (close-connection presence remote-uuid self))
                               (continuation-return exit #f))
                             (lambda ()
                               (let ((processing-handler (connection-processing-handler self)))
                                 (if processing-handler
                                     (processing-handler)))
                               (connection-read-message self)))))
              (bind (kind . rest) message
                (case kind
                  ((post send call)
                   (execute self kind rest))
                  ((result)
                   (result self rest)))))
            (loop)))))
    self))


;;;
;;;; Invoke
;;;


(define (connection-invoke self kind method-name remote-proxy arguments)
  (if (and (or debug-remote? (and debug-remote-blocking? (neq? kind 'post))) (debug-presence? presence) (debug-remote-method? method-name))
      (begin
        (debug-remote presence '>>> remote-title remote-uuid kind method-name)
        (caller-garble-hack)))
  (let ((ior (get-ior remote-proxy)))
    ((connection-invoke-handler self)
      (lambda (connection)
        (if (eq? kind 'post)
            (write-message self `(,kind ,method-name #f ,ior ,arguments))
          (let ((mutex (make-mutex 'invoke)))
            (let ((cookie (object->serial mutex)))
              (mutex-lock! mutex)
              (write-message self `(,kind ,method-name ,cookie ,ior ,arguments))
              (register-invocation self mutex)
              (mutex-lock! mutex)
              (mutex-unlock! mutex)
              (unregister-invocation self mutex)
              (let ((result (mutex-specific mutex)))
                (if (is? result Connection-Broke)
                    (signal result)
                  result))))))
      self)))


(define (connection-invoke-live? self remote-proxy timeout)
  (let ((ior (get-ior remote-proxy)))
    (let ((mutex (make-mutex 'invoke)))
      (let ((cookie (object->serial mutex)))
        (mutex-lock! mutex)
        (write-message self `(call live? ,cookie ,ior ()))
        (let ((timed-out (not (mutex-lock! mutex timeout))))
          (mutex-unlock! mutex)
          (if timed-out
              #f
            (mutex-specific mutex)))))))


(define (connection-result self rest)
  (bind (kind method-name cookie result) rest
    (if (and debug-remote? (debug-presence? presence) (debug-remote-method? method-name))
        (begin
          (if (eq? kind 'call)
              (debug-remote presence '<<< remote-title remote-uuid 'result method-name (debug-simplify presence result))
            (debug-remote presence '<<< remote-title remote-uuid 'return method-name))
          (caller-garble-hack)))
    (let ((mutex (serial->object cookie)))
      (mutex-specific-set! mutex result)
      (mutex-unlock! mutex))))


;;;
;;;; Execute
;;;


(define (connection-execute self kind rest)
  (bind (method-name cookie ior arguments) rest
    (if (and debug-remote? (debug-presence? presence) (debug-remote-method? method-name))
        (begin
          (callee-garble-hack)
          (debug-remote presence '<<< remote-title remote-uuid 'execute method-name)))
    (let ((thread
            (new-thread
              (lambda ()
                (let ((local-proxy (ior->proxy ior)))
                  (let ((result (apply (dispatch (class-of local-proxy) method-name) local-proxy arguments)))
                    (execute-handler
                      (lambda (connection)
                        (case kind
                          ((send)
                           (write-message self `(result ,kind ,method-name ,cookie ,(unspecified))))
                          ((call)
                           (write-message self `(result ,kind ,method-name ,cookie ,result)))))
                      self))))
              (list 'execute method-name))))
      (thread-base-priority-set! thread (presence-priority presence))
      (thread-start! thread))))


;;;
;;;; IO
;;;


(define (connection-write-message self message)
  (unwind-protect
      (begin
        (mutex-lock! write-mutex)
        (if (and debug-remote-io? (debug-presence? presence))
            (begin
              (debug-remote presence '>>> remote-title remote-uuid 'write (car message) (cadr message) (caddr message))
              (caller-garble-hack)))
        (write-port port (presence-code presence) (presence-version presence) message))
    (mutex-unlock! write-mutex)))


(define (connection-read-message self)
  (let ((presence (connection-presence self))
        (port (connection-port self)))
    (let ((message (read-port port (presence-code presence) (presence-version presence))))
      (if (and debug-remote-io? (debug-presence? presence))
          (begin
            (callee-garble-hack)
            (debug-remote presence '<<< remote-title remote-uuid 'read (car message) (cadr message) (caddr message))))
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
  (let ((connection (proxy->connection remote-proxy)))
    (invoke connection 'post method-name remote-proxy arguments)))


(define (send-remote method-name remote-proxy . arguments)
  (let ((connection (proxy->connection remote-proxy)))
    (invoke connection 'send method-name remote-proxy arguments)))


(define (call-remote method-name remote-proxy . arguments)
  (let ((connection (proxy->connection remote-proxy)))
    (invoke connection 'call method-name remote-proxy arguments)))


;;;
;;;; Live
;;;


(define live-timeout
  0.5)


(define (proxy-live? remote-proxy)
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


(define (get-local-register #!optional (purpose #f))
  (get-register (require-presence purpose)))


(define (new-remote-register purpose uuid)
  (ior->proxy (new-ior purpose uuid (serialize-runtime-reference (reify-reference Register-Stub)) #f '())))


(define (connect-remote-register host port #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (let ((uuid (connect-remote presence host port #f)))
      (new-remote-register purpose uuid))))


(define (connect-remote-reference host port reference #!key (purpose #f))
  (let ((presence (require-presence purpose)))
    (connect-remote presence host port reference)))


(define (get-remote-register remote-proxy)
  (let ((ior (get-ior remote-proxy)))
    (let ((purpose (get-purpose ior))
          (uuid (get-uuid ior)))
      (let ((presence (require-presence purpose)))
        (new-remote-register purpose uuid)))))


(define (register-proxy name proxy-class object #!optional (purpose #f))
  (let ((presence (require-presence purpose)))
    (let ((register (get-local-register purpose))
          (proxy (new proxy-class presence object)))
      (register-object register name proxy)
      proxy)))


(define (proxy-connection-info client proxy)
  (let ((client-presence (get-presence client))
        (client-uuid (get-uuid (get-ior client))))
    (let ((client-connection (require-connection client-presence client-uuid)))
      (let ((host (get-address client-connection)))
        (if (local-proxy? proxy)
            (let ((port (listener-listening-port (get-remote-listener))))
              (list proxy host port))
          (let ((proxy-presence (get-presence proxy))
                (proxy-uuid (get-uuid (get-ior proxy))))
            (let ((proxy-connection (require-connection proxy-presence proxy-uuid)))
              (let ((port (get-service proxy-connection)))
                ;; we cannot use the peer port number because if it was him that connected
                ;; to us then it will have a distinct port from its process remote listener
                (if (not port)
                    (error "Fatal")
                  (list proxy host port))))))))))


(define (connect-remote-proxy info)
  (bind (proxy host port) info
    (let ((presence (get-presence proxy))
          (ior (get-ior proxy)))
      (let ((uuid (get-uuid ior)))
        ;; already connected
        (if (not (presence-find-connection presence uuid))
            (if (not (uuid=? (connect-remote presence host port #f)
                             uuid))
                (error "Fatal")))
        proxy))))


(define (disconnect-remote-proxy remote-proxy)
  (let ((presence (get-presence remote-proxy))
        (uuid (get-uuid (get-ior remote-proxy))))
    (let ((connection (presence-find-connection presence uuid)))
      ;; robust
      (if connection
          (begin
            (set-closing? connection #t)
            (disconnect-connection presence uuid connection))))))


(define (close-remote-proxy remote-proxy)
  (let ((presence (get-presence remote-proxy))
        (uuid (get-uuid (get-ior remote-proxy))))
    (let ((connection (require-connection presence uuid)))
      (set-closing? connection #t)
      (close-connection presence uuid connection))))


(define (closing-remote-proxy remote-proxy)
  (let ((presence (get-presence remote-proxy))
        (uuid (get-uuid (get-ior remote-proxy))))
    (let ((connection (require-connection presence uuid)))
      (set-closing? connection #t))))


;; quick safe version
(define (closing-remote-proxy-safe remote-proxy)
  (let ((presence (get-presence remote-proxy))
        (uuid (get-uuid (get-ior remote-proxy))))
    (let ((connection (presence-find-connection presence uuid)))
      (if connection
          (set-closing? connection #t)))))


;; quick safe version
(define (closing-remote-connection-safe purpose uuid)
  (let ((presence (require-presence purpose)))
    (let ((connection (presence-find-connection presence uuid)))
      (if connection
          (set-closing? connection #t)))))


;;;
;;;; IOR
;;;


(define (ior? object)
  (is? object IOR))


(define (ior=? x y)
  (and (uuid=? (get-uuid x) (get-uuid y))
       (reference=? (get-reference x) (get-reference y))))


(define (ior-server=? x y)
  (uuid=? (get-uuid x) (get-uuid y)))


(define (host=? x y)
  (equal? x y))


(define (service=? x y)
  (eqv? x y))


(define (reference=? x y)
  (eqv? x y))


(define (ior->proxy ior)
  (let ((purpose (get-purpose ior)))
    (let ((presence (require-presence purpose)))
      (define (local-ior? ior)
        (find-presence-by-uuid (get-uuid ior)))
      
      (define (local->proxy stub-interface ior)
        (define (reference->local-proxy stub-interface reference)
          (if (not reference)
              (get-local-register purpose)
            (new (local-class stub-interface) presence (serial->object reference))))
        
        (reference->local-proxy stub-interface (get-reference ior)))
      
      (define (remote->proxy stub-interface ior)
        (let ((remote-class (remote-class stub-interface)))
          (new remote-class presence ior (get-values ior))))
      
      (let ((stub-interface (resolve-runtime-reference (deserialize-runtime-reference (get-stub-interface ior)))))
        (if (local-ior? ior)
            (local->proxy stub-interface ior)
          (remote->proxy stub-interface ior))))))


;;;
;;;; Proxy
;;;


(define (proxy? object)
  (is? object Proxy))


(define (proxy=? x y)
  (cond ((and (remote-proxy? x) (remote-proxy? y))
         (remote-proxy=? x y))
        ((and (local-proxy? x) (local-proxy? y))
         (local-proxy=? x y))
        (else
         #f)))


(define (proxy-server=? x y)
  (cond ((and (remote-proxy? x) (remote-proxy? y))
         (ior-server=? (get-ior x)
                       (get-ior y)))
        ((and (local-proxy? x) (local-proxy? y))
         #t)
        (else
         #f)))


;;;
;;;; Local Proxy
;;;


(define (local-proxy? object)
  (is? object Local-Proxy))


(define (local-proxy=? x y)
  (eq? (get-object x)
       (get-object y)))


;;;
;;;; Remote Proxy
;;;


(define (remote-proxy? object)
  (is? object Remote-Proxy))


(define (remote-proxy=? x y)
  (ior=? (get-ior x)
         (get-ior y)))


(define (remote-proxy->connection proxy)
  (let ((presence (get-presence proxy)))
    (require-connection presence (get-uuid (get-ior proxy)))))


(define (remote-proxy-connected? proxy)
  (let ((presence (get-presence proxy)))
    (presence-find-connection presence (get-uuid (get-ior proxy)))))


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
  (let ((presence (get-presence proxy)))
    (define (local-proxy->reference)
      (if (proxy=? proxy (get-register presence))
          #f
        (let ((object (get-object proxy)))
          (gc-protect object)
          (object->serial object))))
    
    (let ((ior
            (new-ior
              (get-purpose presence)
              (get-uuid presence)
              (serialize-runtime-reference (stub-reference proxy))
              (local-proxy->reference)
              (proxy-values proxy))))
      (serialize-object (class-of proxy) (encode-ior ior)))))


(define (marshall-remote-proxy proxy)
  (serialize-object (class-of proxy) (encode-ior (get-ior proxy))))


(define (encode-ior ior)
  (vector (get-purpose ior)
          (get-uuid ior)
          (get-stub-interface ior)
          (get-reference ior)
          (get-values ior)))


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
      (not (memq? (get-purpose presence) debug-remote-ignore-presences))))


(define (debug-remote-method? method-name)
  (or (not debug-remote-ignore-methods)
      (not (memq? method-name debug-remote-ignore-methods))))


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
