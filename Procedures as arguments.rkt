#lang racket

(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

(define (search f negativeP positiveP)
  (let ((midpoint (average negativeP positiveP)))
    (if (close-enough? negativeP positiveP)
        midpoint
        (let ((testpoint (f midpoint)))
          (cond ((positive? testpoint) (search f negativeP midpoint))
                ((negative? testpoint) (search f midpoint positiveP))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Y argumrntov odinakovii znak")))))
          
(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)


(define tolerance 0.00001)

(define (good-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))


(define (fixed-point f start-point)
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))
(try start-point))
      

;(fixed-point (lambda (x) (+ 1 (/ 1 x)))
 ;            (fixed-point (lambda (y) (+ (sin y) (cos y)))
  ;                        (fixed-point cos 1.0)))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1)


(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

  
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0)
11)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0)
11)


(define (d i)
  (if (not (= (remainder (+ i 1) 3) 0))
            1
            (* 2 (/ (+ i 1) 3))))

(d 8)

(define e
  (+ 2 (cont-frac (lambda (i) 1.0) d 10)))
e
(define (square x) (* x x))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

(tan-cf 1.0 10)
(tan-cf (/ pi 6) 10)