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

(define s235 (cons-stream 1 (merge (scale-stream s235 2)
                                   (merge (scale-stream s235 3)
                                          (scale-stream s235 5)))))

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
    (define result
      (cons-stream 1
                   (minus-stream (mul-series rest-stream
                                             result))))
    result))

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


;;;SECTION 3.5.3

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x1 x2)
  (/ (+ x1 x2)
     2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;: (display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;: (display-stream pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))    
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;: (display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;: (display-stream (accelerated-sequence euler-transform
;:                                       pi-stream))

;; EXERCISE 3.63
;: (define (sqrt-stream x)
;:   (cons-stream 1.0
;:                (stream-map (lambda (guess)
;:                              (sqrt-improve guess x))
;:                            (sqrt-stream x))))

;; EXERCISE 3.64
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (let ((this-item (stream-car s))
        (substream (stream-cdr s))
        (next-item (stream-car (stream-cdr s))))
    (if (< (abs (- this-item next-item))
           tolerance)
      next-item
      (stream-limit substream tolerance))))

;; EXERCISE 3.65
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map -
                           (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

;;; Infinite streams of pairs

;: (stream-filter (lambda (pair)
;:                  (prime? (+ (car pair) (cadr pair))))
;:                int-pairs)

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

;: (pairs integers integers)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; EXERCISE 3.67
;: (define (pairs s t)
;:   (cons-stream
;:     (list (stream-car s) (stream-car t))
;:     (interleave
;:       (stream-map (lambda (x) (list (stream-car s) x))
;:                   (stream-cdr t))
;:       (interleave (stream-map (lambda (x) (list x (stream-car t)))
;:                               (stream-cdr s))
;:                   (pairs (stream-cdr s) (stream-cdr t))))))

;; EXERCISE 3.68
;: (define (pairs s t)
;:   (interleave
;:    (stream-map (lambda (x) (list (stream-car s) x))
;:                t)
;:    (pairs (stream-cdr s) (stream-cdr t))))

;; EXERCISE 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted (stream-map (lambda (x) (list (stream-car s) x))
                                (stream-cdr t))
                    (weighted-pairs (stream-cdr s)
                                    (stream-cdr t)
                                    weight)
                    weight)))

(define sum-inc-pairs
  (weighted-pairs integers
                  integers
                  (lambda (x) (+ (car x)
                                 (cadr x)))))

(define spe-pairs
  (weighted-pairs s235
                  s235
                  (lambda (x) (+ (* 2 (car x))
                                 (* 3 (cadr x))
                                 (* 5 (car x) (cadr x))))))

;; EXERCISE 3.69
(define (triples s t u)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (stream-car u))
               (merge-weighted
                 (stream-map (lambda (x)
                               (list (stream-car s) (car x) (cadr x)))
                             (weighted-pairs t
                                             (stream-cdr u)
                                             (lambda (pair) (+ (car pair)
                                                               (cadr pair)))))
                 (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
                 (lambda (triple) (+ (car triple) (cadr triple) (caddr triple))))))

(define triple-integers
  (triples integers integers integers))

(define tri
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple))))
                 triple-integers))

;; EXERCISE 3.71
(define (ramanujan)
  (define (cube x)
    (* x x x))
  (define (weight x)
    (+ (cube (car x))
       (cube (cadr x))))
  (define (reduce-stream s weight)
    (let ((this (weight (stream-car s)))
          (next (weight (stream-car (stream-cdr s)))))
      (if (= this next)
        (cons-stream (stream-car s) (reduce-stream (stream-cdr (stream-cdr s)) weight))
        (reduce-stream (stream-cdr s) weight))))
  (stream-map 
    weight
    (reduce-stream 
      (weighted-pairs integers
                      integers
                      weight)
      weight)))

;; EXERCISE 3.72
(define (ramanujan-like)
  (define (weight x)
    (+ (square (car x))
       (square (cadr x))))
  (define (reduce-stream s weight)
    (let ((this (weight (stream-car s)))
          (next (weight (stream-car (stream-cdr s))))
          (nnext (weight (stream-car (stream-cdr (stream-cdr s))))))
      (if (and (= this next)
               (= this nnext))
        (cons-stream (list (stream-car s)
                           (stream-car (stream-cdr s))
                           (stream-car (stream-cdr (stream-cdr s))))
                     (reduce-stream (stream-cdr (stream-cdr (stream-cdr s))) weight))
        (reduce-stream (stream-cdr s) weight))))
  (stream-map 
    (lambda (l)
      (cons (weight (car l))
            l))
    (reduce-stream 
      (weighted-pairs integers
                      integers
                      weight)
      weight)))

;;; Streams as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; EXERCISE 3.73
(define (RC R C dt)
  (lambda (i-stream v0)
    (add-streams (scale-stream i-stream R)
                 (integral (scale-stream i-stream (/ 1 C))
                           v0
                           dt))))

;; EXERCISE 3.74
(define (sign-change-detector b a)
  (cond ((and (< a 0) (>= b 0)) 1)
        ((and (>= a 0) (< b 0)) -1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

;: (define zero-crossings (make-zero-crossings sense-data 0))

;: (define zero-crossings
;:   (stream-map sign-change-detector
;:               sense-data
;:               (cons-stream 0 sense-data)))

;; EXERCISE 3.75
;: (define (make-zero-crossings input-stream last-value last-avpt)
;:   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;:     (cons-stream (sign-change-detector avpt last-avpt)
;:                  (make-zero-crossings (stream-cdr input-stream)
;:                                       (stream-car input-stream)
;:                                       avpt))))

;; EXERCISE 3.76
(define (smooth input)
  (define (smooth-iter input last-value)
    (let ((ave (/ (+ (stream-car input) last-value) 2)))
      (cons-stream ave
                   (smooth-iter (stream-cdr input)
                                (stream-car input)))))
  (smooth-iter input 0))

(define (make-zero-crossings-filter input-stream smooth)
  (let ((filtered-stream (smooth input-stream)))
    (make-zero-crossings (stream-cdr filtered-stream)
                         (stream-car filtered-stream))))

;;;SECTION 3.5.4

;: (define (solve f y0 dt)
;:   (define y (integral dy y0 dt))
;:   (define dy (stream-map f y))
;:   y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


;: (stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; EXERCISE 3.77
;: (define (integral delayed-integrand initial-value dt)
;:   (let ((integrand (force delayed-integrand)))
;:     (cons-stream initial-value
;:                  (if (stream-null? integrand)
;:                      the-empty-stream
;:                      (integral (stream-cdr integrand)
;:                                (+ (* dt (stream-car integrand))
;:                                   initial-value)
;:                                dt)))))

;; EXERCISE 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; EXERCISE 3.79
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; EXERCISE 3.80
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- 0 (/ 1 C))))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (- 0 (/ R L)))))
    (cons vc il)))

;;;SECTION 3.5.5

;; same as in section 3.1.2
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))


;: (define cesaro-stream
;:   (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
;:                         random-numbers))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))


(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;: (define pi
;:   (stream-map (lambda (p) (sqrt (/ 6 p)))
;:               (monte-carlo cesaro-stream 0 0)))

;; EXERCISE 3.81
(define (rand demand-stream last-random)
  (let ((this-demand (stream-car demand-stream)))
    (cond ((eq? this-demand 'generate)
           (cons-stream last-random
                        (rand (stream-cdr demand-stream)
                              (rand-update last-random))))
          ((eq? this-demand 'generate)
           (cons-stream random-init
                        (rand (stream-cdr demand-stream)
                              (rand-update last-random)))))))

(define random-numbers
  (rand demand-stream random-init))

;; same as in section 3.1.3
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))

