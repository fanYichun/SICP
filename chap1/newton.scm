(define (sqrt x)
  (define (sqrt-iter guess)
   (define (improve guess)
        (/ (+ guess
              (/ x guess))
           2))
  
   (define (good-enough? guess)
      (define (square x) (* x x))
      (define error 0.001)
    
      (if (< (abs (- (improve guess)
                     guess))
             error)
        true
        false))
    
       (if (good-enough? guess)
          guess
          (sqrt-iter (improve guess))))

 (sqrt-iter 1.0))
