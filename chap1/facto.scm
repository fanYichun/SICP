(define (facto n)
  (define (facto-iter n prod)
    (if (= n 1)
      prod
      (facto-iter (- n 1)
                  (* prod n))))
  (facto-iter n 1))

(define (last-non-zero n)
  (define (last-non-zero-iter n counter)
    (let ((remain (remainder n 10)))
      (if (not (=  remain 0))
        (cons remain counter)
        (last-non-zero-iter (quotient n 10)
                            (+ counter 1)))))
  (last-non-zero-iter n 1))

(define w (facto 2016))

