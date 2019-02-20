#lang racket

(lambda (x) (+ x 4))

(define (square x)
  (* x x))

((lambda (x y z) (+ x y (square z))) 1 2 3)

;(define (f x y)
 ; (let ((a (+ 1 (* x y)))
  ;      (b (- 1 y)))
  ;(+ (* x (square a)) (* y b) (* a b))))

((lambda (x) (* x 3)) 2)

(define g (lambda (y) (* y (+ 5 y))))

(define b ((lambda (b) (* b b b)) 3))

(let ((a (* 4 5))
      (b (+ 14 13)))
  (/ b a))