(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;;;SECTION 2.4.3

;; uses get/put (from 3.3.3) -- see ch2support.scm

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  (define (equ? z1 z2)
    (and (= (real-part z1)
            (real-part z2))
         (= (imag-part z1)
            (imag-part z2))))
  (define (=zero? z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular) equ?)
  (put '=zero? '(rectangular) =zero?)
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (equ? z1 z2)
    (and (= (magnitude z1)
            (magnitude z2))
         (= (angle z1)
            (angle z2))))
  (define (=zero? z)
    (= 0 (magnitude z1)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar) equ?)
  (put '=zero? '(polar) =zero?)
  'done)

;;footnote
;: (apply + (list 1 2 3 4))


(define (apply-generic op . args)
  (display "apply-generic ")
  (display op)
  (newline)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;; Generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))



;; EXERCISE 2.73

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (install-deriv-package)

  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (base ex) (car ex))
  (define (exponent ex) (cadr ex))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list 'exp base exponent))))

  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (product-deriv exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
  
  (define (exponentiation-deriv exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp)
                                                     (make-sum (exponent exp) -1))
                                (deriv (base exp) var))))
 
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  (put 'deriv 'exp exponentiation-deriv)
  'done)

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;: ((get (operator exp) 'deriv) (operands exp) var)


;; Message passing
;: (define (make-from-real-imag x y)
;:   (define (dispatch op)
;:     (cond ((eq? op 'real-part) x)
;:           ((eq? op 'imag-part) y)
;:           ((eq? op 'magnitude)
;:            (sqrt (+ (square x) (square y))))
;:           ((eq? op 'angle) (atan y x))
;:           (else
;:            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;:   dispatch)
;: 
;: (define (make-from-mag-ang r a)
;:   (define (dispatch op)
;:     (cond ((eq? op 'real-part) (* r (cos a)))
;:           ((eq? op 'imag-part) (* r (sin a)))
;:           ((eq? op 'magnitude) r)
;:           ((eq? op 'angle) a)
;:           (else (error "Unkown op"))))
;:   dispatch)
;: 
;: (define (apply-generic op arg) (arg op))

;;;SECTION 2.5.1

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'raise 'scheme-number
       (lambda (x) (make-complex-from-real-imag x 0)))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x1 x2)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))
  (define (=zero? x)
    (= 0 (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise 'rational
       (lambda (x) (/ (numer x) (denom x))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)

  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))
  
  (put 'add '(complex scheme-number)
       (lambda (z x) (tag (add-complex-to-schemenum z x))))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? z1 z2)
  (apply-generic 'equ? z1 z2))
(define (=zero? z)
  (apply-generic '=zero? z))
(define (raise x)
  (let ((tag (type-tag x)))
    ((get 'raise tag) (contents x))))

;;;SECTION 2.5.2
;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                  (error "No method for these types"
                         (list op type-tags))

                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags))))))))

;; EXERCISE 2.81

;: (define (scheme-number->scheme-number n) n)
;: (define (complex->complex z) z)
;: (put-coercion 'scheme-number 'scheme-number
;:               scheme-number->scheme-number)
;: (put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

;: ;;;SECTION 2.5.3
;: 
;: ;;; ALL procedures in 2.5.3 except make-polynomial
;: ;;; should be inserted in install-polynomial-package, as indicated
;: 
;: (define (add-poly p1 p2)
;:   (if (same-variable? (variable p1) (variable p2))
;:       (make-poly (variable p1)
;:                  (add-terms (term-list p1)
;:                             (term-list p2)))
;:       (error "Polys not in same var -- ADD-POLY"
;:              (list p1 p2))))
;: 
;: (define (mul-poly p1 p2)
;:   (if (same-variable? (variable p1) (variable p2))
;:       (make-poly (variable p1)
;:                  (mul-terms (term-list p1)
;:                             (term-list p2)))
;:       (error "Polys not in same var -- MUL-POLY"
;:              (list p1 p2))))
;: 
;: ;; *incomplete* skeleton of package
;: (define (install-polynomial-package)
;:   ;; internal procedures
;:   ;; representation of poly
;:   (define (make-poly variable term-list)
;:     (cons variable term-list))
;:   (define (variable p) (car p))
;:   (define (term-list p) (cdr p))
;:   ;;[procedures same-variable? and variable? from section 2.3.2]
;: 
;:   ;; representation of terms and term lists
;:   ;;[procedures adjoin-term ... coeff from text below]
;: 
;:   ;;(define (add-poly p1 p2) ... )
;:   ;;[procedures used by add-poly]
;: 
;:   ;;(define (mul-poly p1 p2) ... )
;:   ;;[procedures used by mul-poly]
;: 
;:   ;; interface to rest of the system
;:   (define (tag p) (attach-tag 'polynomial p))
;:   (put 'add '(polynomial polynomial) 
;:        (lambda (p1 p2) (tag (add-poly p1 p2))))
;:   (put 'mul '(polynomial polynomial) 
;:        (lambda (p1 p2) (tag (mul-poly p1 p2))))
;: 
;:   (put 'make 'polynomial
;:        (lambda (var terms) (tag (make-poly var terms))))
;:   'done)
;: 
;: (define (add-terms L1 L2)
;:   (cond ((empty-termlist? L1) L2)
;:         ((empty-termlist? L2) L1)
;:         (else
;:          (let ((t1 (first-term L1)) (t2 (first-term L2)))
;:            (cond ((> (order t1) (order t2))
;:                   (adjoin-term
;:                    t1 (add-terms (rest-terms L1) L2)))
;:                  ((< (order t1) (order t2))
;:                   (adjoin-term
;:                    t2 (add-terms L1 (rest-terms L2))))
;:                  (else
;:                   (adjoin-term
;:                    (make-term (order t1)
;:                               (add (coeff t1) (coeff t2)))
;:                    (add-terms (rest-terms L1)
;:                               (rest-terms L2)))))))))
;: 
;: (define (mul-terms L1 L2)
;:   (if (empty-termlist? L1)
;:       (the-empty-termlist)
;:       (add-terms (mul-term-by-all-terms (first-term L1) L2)
;:                  (mul-terms (rest-terms L1) L2))))
;: 
;: (define (mul-term-by-all-terms t1 L)
;:   (if (empty-termlist? L)
;:       (the-empty-termlist)
;:       (let ((t2 (first-term L)))
;:         (adjoin-term
;:          (make-term (+ (order t1) (order t2))
;:                     (mul (coeff t1) (coeff t2)))
;:          (mul-term-by-all-terms t1 (rest-terms L))))))
;: 
;: 
;: ;; Representing term lists
;: 
;: (define (adjoin-term term term-list)
;:   (if (=zero? (coeff term))
;:       term-list
;:       (cons term term-list)))
;: 
;: (define (the-empty-termlist) '())
;: (define (first-term term-list) (car term-list))
;: (define (rest-terms term-list) (cdr term-list))
;: (define (empty-termlist? term-list) (null? term-list))
;: 
;: (define (make-term order coeff) (list order coeff))
;: (define (order term) (car term))
;: (define (coeff term) (cadr term))
;: 
;: 
;: ;; Constructor
;: (define (make-polynomial var terms)
;:   ((get 'make 'polynomial) var terms))
;: 
;: 
;: ;; EXERCISE 2.91
;: 
;: (define (div-terms L1 L2)
;:   (if (empty-termlist? L1)
;:       (list (the-empty-termlist) (the-empty-termlist))
;:       (let ((t1 (first-term L1))
;:             (t2 (first-term L2)))
;:         (if (> (order t2) (order t1))
;:             (list (the-empty-termlist) L1)
;:             (let ((new-c (div (coeff t1) (coeff t2)))
;:                   (new-o (- (order t1) (order t2))))
;:               (let ((rest-of-result
;:                      ??FILL-THIS-IN?? ;compute rest of result recursively
;:                      ))
;:                 ??FILL-THIS-IN?? ;form complete result
;:                 ))))))
;: 
;: 
;: ;; EXERCISE 2.93
;: ;: (define p1 (make-polynomial 'x '((2 1)(0 1))))
;: ;: (define p2 (make-polynomial 'x '((3 1)(0 1))))
;: ;: (define rf (make-rational p2 p1))
;: 
;: 
;: ;; Rational functions
;: 
;: (define (gcd a b)
;:   (if (= b 0)
;:       a
;:       (gcd b (remainder a b))))
;: 
;: (define (gcd-terms a b)
;:   (if (empty-termlist? b)
;:       a
;:       (gcd-terms b (remainder-terms a b))))
;: 
;: 
;: ;; EXERCISE 2.94
;: ;: (define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;: ;: (define p2 (make-polynomial 'x '((3 1) (1 -1))))
;: ;: (greatest-common-divisor p1 p2)
;: 
;: 
;: ;; EXERCISE 2.97
;: 
;: (define (reduce-integers n d)
;:   (let ((g (gcd n d)))
;:     (list (/ n g) (/ d g))))
;: 
;: ;: (define p1 (make-polynomial 'x '((1 1)(0 1))))
;: ;: (define p2 (make-polynomial 'x '((3 1)(0 -1))))
;: ;: (define p3 (make-polynomial 'x '((1 1))))
;: ;: (define p4 (make-polynomial 'x '((2 1)(0 -1))))
;: 
;: ;: (define rf1 (make-rational p1 p2))
;: ;: (define rf2 (make-rational p3 p4))
;: 
;: ;: (add rf1 rf2)
