(define (square x) (* x x))

(define (iseven x ) (= remainder n 2) 0))

(define (exp-iter b n a)
   (cond ((= n 0) 1)
         ((iseven n) (exp-iter (square b) (/ n 2) a))
         (else (* b (exp-iter b (- n 1) a)))))

(define (fast-exp b n) (exp-iter b n 1))

