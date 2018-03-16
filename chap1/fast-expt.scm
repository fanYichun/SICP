(define (fast-expt b n)
  (define (fast-expt a m)
    (cond ((= m 1) a)
          ((= m 0) 1)
          ((even? m) (fast-expt (square a)
                                (/ m 2)))
          (else (* a
                   (fast-expt a (- m 1))))))
  (define (square m) (* m m))

  (fast-expt b n))
