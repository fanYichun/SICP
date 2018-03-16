(define (reverse a)
  (define (reverse-temp a temp-list)
    (if (null? a)
      temp-list
      (reverse-temp (cdr a) (cons (car a) temp-list))))
  (reverse-temp a '()))

(define (filter-list filter? list-a)
  (define (filter-list-iter filter? list-a temp-list)
    (if (null? list-a)
      temp-list
      (filter-list-iter filter?
                        (cdr list-a)
                        (if (filter? (car list-a))
                          (cons (car list-a) temp-list)
                          temp-list))))
  (reverse (filter-list-iter filter? list-a '())))

(define (same-parity . w)
  (define first-item (car w))
  (filter-list (lambda (x)
                 (if (even? first-item)
                   (even? x)
                   (odd? x)))
               w))

(define (for-each proc items)
  (cond ((null? items) '())
        (true (proc (car items))
              (for-each proc (cdr items)))))

(define (deep-reverse tree)
  (cond ((not (pair? tree)) tree)
        (true (append (deep-reverse (cdr tree))
                      (list (deep-reverse (car tree)))))))

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (true (append (fringe (car tree))
                       (fringe (cdr tree))))))

; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (subtree)
         ; Why can't use cond here?
         (if (pair? subtree)
           (tree-map proc subtree)
           (proc subtree)))
       tree))

(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor)) tree))

(define (square-tree tree)
  (tree-map square tree))

; Exercise 2.32
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest
              (map (lambda (list-x)
                     (cons (car s)
                           list-x))
                   rest)))))
