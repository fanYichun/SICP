(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; stream-car and stream-cdr would normally be built into
;;  the stream implementation
;; (define (stream-car stream) (car stream))
;; (define (stream-cdr stream) (force (cdr stream)))

;: (stream-car
;:  (stream-cdr
;:   (stream-filter prime?
;:                  (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; force would normally be built into
;;  the stream implementation
;: (define (force delayed-object)
;:   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; EXERCISE 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

;; EXERCISE 3.51

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;: (stream-ref x 5)
;: (stream-ref x 7)


;; EXERCISE 3.52

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;;;SECTION 3.5.2

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;: (stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;: (stream-ref primes 50)

;;;Defining streams implicitly

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


;; EXERCISE 3.53
;; (define s (cons-stream 1 (add-streams s s)))

;; EXERCISE 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials
                                               (integers-starting-from 1))))

;; EXERCISE 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;; EXERCISE 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define s (cons-stream 1 (merge (scale-stream s 2)
                                (merge (scale-stream s 3)
                                       (scale-stream s 5)))))

;; EXERCISE 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


;; EXERCISE 3.59
(define (integrate-series s)
  (div-streams s
               (integers-starting-from 1)))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (minus-stream s)
  (stream-map (lambda (num) (- 0 num))
              s))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (minus-stream sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; EXERCISE 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (mul-series s1 (stream-cdr s2))
                            (scale-stream (stream-cdr s1)
                                          (stream-car s2)))))

;; EXERCISE 3.61
(define (inv-series s)
  (let ((rest-stream (stream-cdr s)))
    (cons-stream 1
                 (minus-stream (mul-series rest-stream
                                           (inv-series s))))))

;; EXERCISE 3.62
(define (div-series s1 s2)
  (let ((scale (stream-car s2)))
    (if (= scale 0)
      (error "s2 start with 0")
      (let ((scaled-s2 (scale-stream s2 (/ 1 scale))))
        (mul-series s1
                    (inv-series scaled-s2))))))

(define tan-series (div-series sine-series cosine-series))

;; show stream
(define (integers-from-to start end)
  (if (= start end)
    (cons start '())
    (cons start
          (integers-from-to (+ start 1)
                            end))))

(define (show-stream s from to)
  (map (lambda (index) (stream-ref s index))
       (integers-from-to from to)))
