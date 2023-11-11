(define random-integer-65536
  (let* ((rs (make-random-source))
         (ri (random-source-make-integers rs)))
    (random-source-randomize! rs)
    (lambda ()
      (ri 65536))))
