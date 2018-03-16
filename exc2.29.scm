(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (let ((left-struct (branch-structure (left-branch mobile)))
        (right-struct (branch-structure (right-branch mobile))))
    (+ (if (not (pair? left-struct))
         left-struct
         (total-weight left-struct))
       (if (not (pair? right-struct))
         right-struct
         (total-weight right-struct)))))

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))
