;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Proxies
;;;


(define-type-of-object proxy
  extender: define-type-of-proxy
  unprintable:
  presence)


(define (setup-proxy proxy)
  (setup-object proxy))


(define (proxy-stub proxy)
  ((dispatch proxy 'stub) proxy))


(define (proxy-live? proxy)
  ((dispatch proxy 'live?) proxy))


;;;
;;;; Local
;;;


(define-type-of-proxy local-proxy
  extender: define-type-of-local-proxy
  object)


(define (new-local-proxy presence object)
  (let ((local-proxy
          (make-local-proxy
            'local-proxy
            #f
            presence
            object)))
    (setup-local-proxy local-proxy)
    local-proxy))


(define (setup-local-proxy local-proxy)
  (setup-proxy local-proxy)
  (add-method local-proxy 'live? local-proxy-live?))


(define (local-proxy-print local-proxy output readably)
  (print-unreadable local-proxy output
    (lambda (output)
      (format output "{?{a} ~}{s}"
        (presence-purpose presence)
        object))))


(define (local-proxy-values local-proxy)
  ((dispatch local-proxy 'values) local-proxy))


(define (local-proxy-live? local-proxy)
  #t)


;;;
;;;; Remote
;;;


(define-type-of-proxy remote-proxy
  extender: define-type-of-remote-proxy
  ior
  values)


(define (new-remote-proxy presence ior values)
  (let ((remote-proxy
          (make-remote-proxy
            'remote-proxy
            #f
            presence
            ior
            values)))
    (setup-remote-proxy remote-proxy)
    remote-proxy))


(define (setup-remote-proxy remote-proxy)
  (setup-proxy remote-proxy)
  (add-method remote-proxy 'live? remote-proxy-live?))


(define (remote-proxy-print remote-proxy output readably)
  (print-unreadable remote-proxy output
    (lambda (output)
      (format output "{?{a} ~}{s}"
        (presence-purpose presence)
        (ior-uuid ior)))))


;; meant to be used by the remoting system only
(define (remote-proxy-set-values remote-proxy lst)
  (set! values lst))


(define (remote-proxy-proxy-value remote-proxy keyword thunk)
  (let ((prop (getprop values keyword)))
    (if prop
        (cadr prop)
      (thunk))))
