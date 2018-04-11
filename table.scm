(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; two-dimensional
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; local tables
(define (make-table same-key?)
  (define (assoc same-key? key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc same-key? key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; EXERCISE 3.24
(define (same-key? key1 key2)
  (let ((tolerance 0.000001))
    (if (and (number? key1)
             (number? key2))
      (< (abs (- key1 key2))
         tolerance)
      (equal? key1 key2))))

;; EXERCISE 3.25
; arbitrary dimention table
(define (lookup key-list table)
  (if (null? key-list)
    (cadr table)
    (let ((subtable (assoc (car key-list)
                           (cddr table)))
          (rest-key (cdr key-list)))
      (if subtable
        (lookup rest-key subtable)
        #f))))

(define (insert! key-list value table)
  (define (make-key-list key-list value)
    (if (null? (cdr key-list))
      (cons (car key-list)
            value)
      (list (car key-list)
            (make-key-list (cdr key-list)
                           value))))
  (if (null? key-list)
    (set-car! (cdr table))
    (let ((subtable (assoc (car key-list)
                           (cddr table)))
          (rest-key (cdr key-list)))
      (if subtable
        (insert! rest-key subtable)
        (set-cdr! (cdr table)
                  (cons (make-key-list key-list value)
                        (cdr table)))))))

;; EXERCISE 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

