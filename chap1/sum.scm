(define (cube x) (* x x x))
(define (square x) (* x x))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (double x) (* x 2))

(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (filter? a)
                (term a)
                null-value)
              (filtered-accumulate filter? combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (filtered-accumulate (lambda (x) true) combiner null-value term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (pi end)
  (* 4.0
     (product (lambda (x) (/ (* (- x 1)
                                (+ x 1))
                             (square x)))
              3
              (lambda (x) (+ x 2))
              end)))

(define (integral f a b n)
  (define h (/ (- b a) n))

  (if (> a b)
    (- 0
       (integral f b a n))
    (/ (* h
          (+ (f a)
             (f b)
             (* 4
                (sum f
                     (+ a h)
                     (lambda (x) (+ x (* 2 h)))
                     (- b (/ h 2))))
             (* 2
                (sum f
                     (+ a (* 2 h))
                     (lambda (x) (+ x (* 2 h)))
                     (- b (* (/ 3 2) h))))))
       3)))

