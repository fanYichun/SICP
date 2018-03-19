(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (split outter inner)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller (split painter (- n 1) outter inner)))
        (outter painter (inner smaller smaller))))))

(define right-split (split beside below))
(define right-split (split below beside))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect . vectors)
  (define (add-cor-vect cor)
    (fold-right + 0 (map cor vectors)))
  (make-vect (add-cor-vect xcor-vect)
             (add-cor-vect ycor-vect)))

(define (minus v)
  (make-vect (- 0 (xcor-vect v))
             (- 0 (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1 (minus v2)))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
;(define (edge2-frame frame)
;  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (make-segment vec1 vec2)
  (cons vec1 vec2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define painter-edge
  (let ((zero-zero (make-vect 0.0 0.0))
        (zero-one (make-vect 0.0 1.0))
        (one-zero (make-vect 1.0 0.0))
        (one-one (make-vect 1.0 1.0)))
    (segments->painter (list (make-segment zero-zero zero-one)
                             (make-segment zero-one one-one)
                             (make-segment one-one one-zero)
                             (make-segment one-zero zero-zero)))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter1)
                     (rotate90 painter2))))
