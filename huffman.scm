;;;SECTION 2.3.3

;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; EXERCISE 2.67

(define sample-tree
  (make-code-tree (make-leaf 'a 4)
                  (make-code-tree
                   (make-leaf 'b 2)
                   (make-code-tree (make-leaf 'd 1)
                                   (make-leaf 'c 1)))))

;: (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; EXERCISE 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
    (if (eq? symbol (symbol-leaf tree))
      '()
      (error "bad symbol: (1)" symbol))
    (cond ((sym-in-list? symbol (symbols (left-branch tree)))
           (cons 0 (encode-symbol symbol (left-branch tree))))
          ((sym-in-list? symbol (symbols (right-branch tree)))
           (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "bad symbol: (2)" symbol)))))

(define (sym-in-list? symbol sym-list)
  (cond ((null? sym-list) false)
        ((eq? symbol (car sym-list)) true)
        (else (sym-in-list? symbol (cdr sym-list)))))

;; EXERCISE 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
    (car leaf-set)
    (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                  (cadr leaf-set))
                                  (cddr leaf-set)))))

;: (define huff-tree
;:   (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
;: 
;: (define text
;:   '(Get a job
;:         Sha na na na na na na na na
;:         Get a job
;:         Sha na na na na na na na na
;:         Wah yip yip yip yip yip yip yip yip yip 
;:         Sha boom))
