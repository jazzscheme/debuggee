;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Network
;;;


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
