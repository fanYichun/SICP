(define (equal? a b)
  (if (not (and (pair? a)
                (pair? b)))
    (eq? a b)
    (and (equal? (car a)
                 (car b))
         (equal? (cdr a)
                 (cdr b)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (make-sum (exponent exp) -1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((temp-augend (cddr s)))
    (if (null? (cdr temp-augend))
      (car temp-augend)
      (cons '+ temp-augend))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((temp-multiplicand (cddr p)))
    (if (null? (cdr temp-multiplicand))
      (car temp-multiplicand)
      (cons '* temp-multiplicand))))

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

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x)
            'exp)))

(define (base ex)
  (cadr ex))

(define (exponent ex)
  (caddr ex))

; set
;(define (element-of-set? x set)
;  (cond ((null? set) false)
;        ((equal? x (car set)) true)
;        (else (element-of-set? x (cdr set)))))
;
;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))
;
;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) '())
;        ((element-of-set? (car set1) set2)
;         (cons (car set1)
;               (intersection-set (cdr set1) set2)))
;        (else (intersection-set (cdr set1) set2))))
;
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2)
;         (union-set (cdr set1) set2))
;        (else (cons (car set1)
;                    (union-set (cdr set1) set2)))))

;(define (element-of-set? x set)
;  (cond ((null? set) false)
;        ((= x (car set)) true)
;        ((< x (car set)) false)
;        (else (element-of-set? x (cdr set)))))

;(define (adjoin-set x set)
;  (if (null? set)
;    (list x)
;    (let ((y (car set)))
;      (cond ((= x y) set)
;            ((> x y) (cons y (adjoin-set x (cdr set))))
;            (else (cons x set))))))

; set as tree
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; EXERCISE 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; EXERCISE 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list-1 set1)
                              (tree->list-1 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list-1 set1)
                                     (tree->list-1 set2))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                            (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))

(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                   (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set-list (cdr set1)
                                                          (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set-list (cdr set1) set2)))
                      (else (cons x2 (union-set-list set1 (cdr set2)))))))))

; Excercise 2.66
(define (lookup given-key tree-of-records)
  (let ((this-entity (entity tree-of-records)))
    (cond ((null? tree-of-records) false)
          ((< given-key (key this-entity))
           (lookup given-key (left-tree tree-of-records)))
          ((= given-key (key this-entity))
           this-entity)
          (else (lookup given-key (right-tree tree-of-records))))))

