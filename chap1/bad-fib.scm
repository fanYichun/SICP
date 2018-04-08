(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (斐波那契 n) (fib n))
