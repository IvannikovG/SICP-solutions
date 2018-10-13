(define (square x) (* x x))
(define (even?n n)
  (= (remainder n 2) 0))

(define (fastest number counter)
  (fast-expt number counter 1))

(define (fast-expt number counter product)
  (cond ((= counter 0) 1)
        ((even?n counter) (square (fast-expt number (/ counter 2) product)))
        (else (* number (fast-expt number (- counter 1) (* number product))))))

(fastest 2 10)
