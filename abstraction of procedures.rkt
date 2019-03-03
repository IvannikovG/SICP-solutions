#lang racket
(define tolerance 0.00001)

(define (sqrt-iter guess x)
   (if (good-enough? guess x)
       guess
       (sqrt-iter (improve guess x)
                  x)))

(define (improve guess x)
   (average guess (/ x guess)))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (cube x)
  (* x x x))

(define (fixed-point f x)
  (define (close-enough? x y)
      (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
    (try x))

(define (average a b)
  (/ (+ a b) 2))
(define (square x)
  (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
                 dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 3.0))

(sqrt 625)

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(newtons-method (cubic 2 3 4) 1)

; 1.41
(define (double procedure)
  (lambda (x) (procedure (procedure x))))

((double square) 2)
(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)

; 1.42
;compose function

(define (compose func1 func2)
  (lambda (x) (func1 (func2 x))))

((compose square inc) 6)

; 1.43
;n-times function aka repeated

;(define (repeated func times)
 ; (define (iter n result)
  ;  (if (= n 0)
   ;     result
    ;    (lambda (x) (iter (- n 1) (func x)))))
  ;(iter times (lambda (x) (func x))))

;(define (repeated func times)
 ; (lambda (x) (define (iter acc counter)
  ;              (if (= counter 0)
   ;                 acc
    ;                (iter (func x) (- counter 1))))
    ;(iter 0 times)))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))

((repeated square 2) 5)

(define (repeated1 f times)
  (lambda (x)
    (if (= times 1)
        (f x)
        (f ((repeated1 f (- times 1)) x)))))


; 1.44

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(define (n-fold-smooth n f dx)
  (repeated (smooth f dx) n)) 

; 1.45
