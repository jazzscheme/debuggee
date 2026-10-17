;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; UUID
;;;


(define uuid-hex
  '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))


(define (uuid? obj)
  (and (string? obj)
       (= (string-length obj) 36)
       (loop (for n from 0 below 36)
             (for c = (string-ref obj n))
             (every (if (or (= n 8)
                            (= n 13)
                            (= n 18)
                            (= n 23))
                        (eqv? c #\-)
                      (vector-memv? c uuid-hex))))))


(define (generate-uuid)
  (cond-expand
    (windows (make-uuid))
    (else (upcase (read-line (open-process (list path: "uuidgen")))))))


(define (make-uuid)
  (let ((n1 (random-integer-65536))
        (n2 (random-integer-65536))
        (n3 (random-integer-65536))
        (n4 (random-integer-65536))
        (n5 (random-integer-65536))
        (n6 (random-integer-65536))
        (n7 (random-integer-65536))
        (n8 (random-integer-65536)))
    (string
      ;; time_lo
      (vector-ref uuid-hex (extract-bit-field 4 12 n1))
      (vector-ref uuid-hex (extract-bit-field 4  8 n1))
      (vector-ref uuid-hex (extract-bit-field 4  4 n1))
      (vector-ref uuid-hex (extract-bit-field 4  0 n1))
      (vector-ref uuid-hex (extract-bit-field 4 12 n2))
      (vector-ref uuid-hex (extract-bit-field 4  8 n2))
      (vector-ref uuid-hex (extract-bit-field 4  4 n2))
      (vector-ref uuid-hex (extract-bit-field 4  0 n2))
      #\-
      ;; time_mid
      (vector-ref uuid-hex (extract-bit-field 4 12 n3))
      (vector-ref uuid-hex (extract-bit-field 4  8 n3))
      (vector-ref uuid-hex (extract-bit-field 4  4 n3))
      (vector-ref uuid-hex (extract-bit-field 4  0 n3))
      #\-
      ;; time_hi_and_version
      (vector-ref uuid-hex #b0100)
      (vector-ref uuid-hex (extract-bit-field 4  8 n4))
      (vector-ref uuid-hex (extract-bit-field 4  4 n4))
      (vector-ref uuid-hex (extract-bit-field 4  0 n4))
      #\-
      ;; clock_seq_hi_and_reserved
      (vector-ref uuid-hex (bitwise-ior (extract-bit-field 2 12 n5) #b1000))
      (vector-ref uuid-hex (extract-bit-field 4  8 n5))
      ;; clock_seq_low
      (vector-ref uuid-hex (extract-bit-field 4  4 n5))
      (vector-ref uuid-hex (extract-bit-field 4  0 n5))
      #\-
      ;; node
      (vector-ref uuid-hex (extract-bit-field 4 12 n6))
      (vector-ref uuid-hex (extract-bit-field 4  8 n6))
      (vector-ref uuid-hex (extract-bit-field 4  4 n6))
      (vector-ref uuid-hex (extract-bit-field 4  0 n6))
      (vector-ref uuid-hex (extract-bit-field 4 12 n7))
      (vector-ref uuid-hex (extract-bit-field 4  8 n7))
      (vector-ref uuid-hex (extract-bit-field 4  4 n7))
      (vector-ref uuid-hex (extract-bit-field 4  0 n7))
      (vector-ref uuid-hex (extract-bit-field 4 12 n8))
      (vector-ref uuid-hex (extract-bit-field 4  8 n8))
      (vector-ref uuid-hex (extract-bit-field 4  4 n8))
      (vector-ref uuid-hex (extract-bit-field 4  0 n8)))))


(define (make-u8vector-uuid)
  (let ((n1 (random-integer-65536))
        (n2 (random-integer-65536))
        (n3 (random-integer-65536))
        (n4 (random-integer-65536))
        (n5 (random-integer-65536))
        (n6 (random-integer-65536))
        (n7 (random-integer-65536))
        (n8 (random-integer-65536)))
    (u8vector
      ;; time_lo
      (extract-bit-field 8  8 n1)
      (extract-bit-field 8  0 n1)
      (extract-bit-field 8  8 n2)
      (extract-bit-field 8  0 n2)
      ;; time_mid
      (extract-bit-field 8  8 n3)
      (extract-bit-field 8  0 n3)
      ;; time_hi_and_version
      (extract-bit-field 8  8 n4)
      (extract-bit-field 8  0 n4)
      ;; clock_seq_hi_and_reserved
      (extract-bit-field 8  8 n5)
      ;; clock_seq_low
      (extract-bit-field 8  0 n5)
      ;; node
      (extract-bit-field 8  8 n6)
      (extract-bit-field 8  0 n6)
      (extract-bit-field 8  8 n7)
      (extract-bit-field 8  0 n7)
      (extract-bit-field 8  8 n8)
      (extract-bit-field 8  0 n8))))


(define (uuid=? u1 u2)
  (equal? u1 u2))


(define (uuid-prefix uuid)
  (substring uuid 0 8))
