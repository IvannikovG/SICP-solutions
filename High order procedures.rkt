#lang racket

(define (cube x)
 (* x x x))
(define (identity a) a)
(define (inc a)
  (+ a 1))

(define (accumulate filter combiner nullvalue term a next b)
  (if (> a b)
      nullvalue
      (if (filter a)
          (combiner (term a) (accumulate filter combiner nullvalue term (next a) next b))
          (accumulate filter combiner nullvalue term (next a) next b))))

(define (accumulateIt combiner nullvalue term a next b)
  (define (iter a product)
    (if (> a b)
        product
        (iter (next a) (combiner (term a) product))))
  (iter a nullvalue))

(define (iseven? a)
  (= (remainder a 2) 0))

(define (sumEvens a b)
  (accumulate iseven? + 0 identity a inc b))

(sumEvens 2 10)

(define (product term a next b)
  (accumulate filter * 1 term a next b))

(define (productIt term a next b)
  (accumulateIt filter * 1 term a next b))

;(define (pi-sum a b)
 ; (define (pi-term a)
  ;  (/ 1.0 (* a (+ a 2))))
  ;(define (pi-next a)
    ;(+ a 4))
  ;(sum pi-term a pi-next b))

(define (even? a)
  (= (remainder a 2) 0))

(define (odd? a)
  (not (= (remainder a 2) 0)))

;(define (integral f a b dx)
 ; (define (add-dx x) (+ x dx))
  ;(* (sum f (+ a (/ dx 2)) add-dx b)
   ;  dx))


;(define (simpsonint f a b n)
 ; (define (term-simpson k)
  ; (* (cond ((odd? k) 4)
   ;         ((or (= k 0) (= k n)) 1)
    ;        ((even? k) 2))
     ; (y k)))
  ;(define h (/ (- b a) n))
  ;(define (y k)
   ; (f (+ a (* k h))))
  ;(* (/ h 3) (sum term-simpson 0 inc n)))

;(simpson cube 0 1 1000);
;(simpsonint cube 0 1 1000)
;(integral cube 0 1 0.001)

;(define (productIt term a next b)
 ; (define (iter a product)
  ;  (if (> a b)
   ;     product
    ;    (iter (next a) (* (term a) product))))
     ;   (iter a 1))
;
;(productIt identity 1 inc 3)

(define (dec a)
  (- a 1))

(define (factorialAbstr n)
  (product identity 1 inc n))
 
(define (square x)
  (* x x))

(define (wallisprod n)
  (define (newterm n)
    (/ (* 4 (square n)) (- (* 4 (square n) 1))))
  (product newterm 1 inc n))

;(wallisprod 2)

;(productIt identity 5 inc 6)