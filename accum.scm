(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (true (append (fringe (car tree))
                       (fringe (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y))
;              '()
;              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* higher-terms x)))
              0
              coefficient-sequence))

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (fringe tree))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (l-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

; enumerate
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; Excercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; Excercise 2.41
(define (unique-trip n)
  (flatmap (lambda (pair)
             (map (lambda (j) (cons j pair))
                  (enumerate-interval 1 (- (car pair) 1))))
           (unique-pairs n)))

(define (trip-sum n s)
  (filter (lambda (trip)
            (= (+ (car trip)
                  (cadr trip)
                  (caddr trip))
               s))
          (unique-trip s)))

; Excersice 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (filter (lambda (each-row)
                           (not (in-list? each-row (map pos-row rest-of-queens))))
                         (enumerate-interval 1 board-size))))
                 ;(enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (position x y) (list x y))
(define (pos-row pos) (car pos))
(define (pos-col pos) (cadr pos))

(define (in-list? a listx)
  (cond ((null? listx) false)
        ((= a (car listx)) true)
        (else (in-list? a (cdr listx)))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? positions)
  (and true
       ;(part-safe? positions
       ;            (lambda (pos1 pos2)
       ;              (not (= (pos-row pos1)
       ;                      (pos-row pos2)))))
       (part-safe? positions
                   (lambda (pos1 pos2)
                     (not (= (abs (- (pos-row pos1)
                                     (pos-row pos2)))
                             (abs (- (pos-col pos1)
                                     (pos-col pos2)))))))))

(define (part-safe? positions part-safe-i?)
  (let ((k-queen (car positions))
        (rest-queens (cdr positions)))
    (define (part-safe-iter rest-queens)
      (cond ((null? rest-queens) true)
            ((not (part-safe-i? k-queen (car rest-queens))) false)
            (else (part-safe-iter (cdr rest-queens)))))
    (part-safe-iter rest-queens)))

(define (solutions-num board-sizes)
  (map (lambda (board-size)
         (length (queens board-size)))
       (enumerate-interval 1 board-sizes)))
