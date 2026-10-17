;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Remote Listener
;;;


(define-type-of-object listener
  presence
  host
  service
  alternate-service
  socket-info
  server-thread)


(define (new-listener presence #!key (host #f) (service #f) (alternate-service #f))
  (let ((listener
          (make-listener
            'listener
            #f
            presence
            host
            service
            alternate-service
            #f
            #f)))
    (setup-listener listener)
    listener))


(define (setup-listener listener)
  (setup-object listener))


;;;
;;;; Start/Stop
;;;


(define (listener-start listener name)
  (listener-start-listener listener name))


(define (listener-stop listener)
  (exit-thread (listener-server-thread listener))
  (listener-server-thread-set! listener #f))


(define (listener-start-listener listener name)
  (let ((host (listener-host listener))
        (service (listener-service listener))
        (alternate-service (listener-alternate-service listener)))
    (let ((server-port (open-tcp-server (list server-address: host port-number: (resolve-service service) keep-alive: #t))))
      (listener-socket-info-set! listener (tcp-server-socket-info server-port))
      (let ((thread
              (make-thread
                (lambda ()
                  (let loop ()
                       (let ((port (read server-port)))
                         (thread-start!
                           (make-thread
                             (lambda ()
                               (listener-accept-connection listener port))
                             'presence-accepted))
                         (loop))))
                name)))
        (thread-start! thread)
        (listener-server-thread-set! listener thread)))))


(define (listener-listening-host listener)
  (socket-info-address (listener-socket-info listener)))


(define (listener-listening-port listener)
  (socket-info-port-number (listener-socket-info listener)))


;;;
;;;; Connection
;;;


(define (listener-accept-connection listener port)
  (presence-accept-remote (listener-presence listener) port))
