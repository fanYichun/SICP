(define (iterative-improve good-enough? f)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((compose (iterative-improve good-enough? f) f) guess))))

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (good-enough? guess)
    (close-enough? guess (f guess)))
  ((iterative-improve good-enough? f) guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (not (> n 1))
    f
    (compose f (repeated f (- n 1)))))

(define (average-damp f)
  (lambda (x) (/ (+ (f x)
                    x)
                 2)))

(define (nroot x n)
  (define transform (lambda (y) (/ x
                                   (expt y (- n 1)))))
  (fixed-point ((repeated average-damp (/ (log n) (log 2))) transform)
               1.0))

(define tolerance 0.00000001)
