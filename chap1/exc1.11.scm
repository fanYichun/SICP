(define (f n)
  (define (f-iter fn-1 fn-2 fn-3 n)
    (cond ((< n 3) fn-1)
          (else (f-iter (+ fn-1
                           (* 2 fn-2)
                           (* 3 fn-3))
                        fn-1
                        fn-2
                        (- n 1)))))
  (cond ((< n 3) n)
        (else (f-iter 2 1 0 n))))
