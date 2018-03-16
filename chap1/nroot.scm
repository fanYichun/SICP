(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (not (> n 1))
    f
    (compose f (repeated f (- n 1)))))

(define (fixed-point f guess)
  (define tolerance 0.00000001)
  (define (close-enough? guess next-guess)
    (< (abs (- guess next-guess)) tolerance))

  (define next-guess (f guess))
  (if (close-enough? guess next-guess)
    next-guess
    (fixed-point f next-guess)))

(define (average-damp f)
  (lambda (x) (/ (+ (f x)
                    x)
                 2)))

(define (nroot x n)
  (define transform (lambda (y) (/ x
                                   (expt y (- n 1)))))
  (fixed-point ((repeated average-damp (/ (log n) (log 2))) transform)
               1.0))
