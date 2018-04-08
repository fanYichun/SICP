;; EXERCISE 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;: (define x (list 'a 'b))
;: (define y (list 'c 'd))
;: (define z (append  x y))
;: z
;: (cdr x)
;: 
;: (define w (append! x y))
;: w
;: (cdr x)


;; EXERCISE 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; EXERCISE 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;; Sharing and identity

;: (define x (list 'a 'b))
;: (define z1 (cons x x))
;: (define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;: z1
;: (set-to-wow! z1)
;: z2
;: (set-to-wow! z2)


;; EXERCISE 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

