#lang racket
;Abstractions 2
(define (Rp r1 r2)
  (/ 1 (+ (/ 1 r1) (/ 1 r2))))

(Rp 6.7 4.8)
;2.7
(define (make-interval x y) (cons x y))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;2.8 interval substraction
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define as (make-interval 3 4))
(define bs (make-interval 1 6))
(define cs (make-interval 2 5))

;2.9 radius

(define (radius interval)
  (/ (abs (- (upper-bound interval) (lower-bound interval))) 2))

(radius (mul-interval bs as))
(radius (mul-interval cs as))

;2.10 Modified division
;(define (mul-interval x y)
 ; (let ((p1 (* (lower-bound x) (lower-bound y)))
  ;      (p2 (* (lower-bound x) (upper-bound y)))
   ;     (p3 (* (upper-bound x) (lower-bound y)))
    ;    (p4 (* (upper-bound x) (upper-bound y))))
    ;(make-interval (min p1 p2 p3 p4)
     ;              (max p1 p2 p3 p4))))

(define (div-interval-modified x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define g (make-interval -2 -5))
(define u (make-interval 1 1))
(div-interval g u)

;2.11

;(define (unimproved-mul-intrevals x y)
 ; (let ((xlo (lower-bound x))
  ;      (ylo (lower-bound y))
    ;    (xhi (upper-bound x))
   ;     (yhi (upper-bound y)))
    ;(cond (and (>= 0 xlo, ylo, xhi, yhi
; (* (

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

;2.12

(define (make-center-percent c e)
  (make-center-width c (* c (/ e 100))))

(define ner1 (make-center-percent 1 2))
(define ber1 (make-center-percent 11 36))
(define zer1 (make-center-percent 13 0.2))

ner1
ber1
zer1

(make-center-percent 400 3)

(define (percent i)
  (* 100 (/ (radius i) (center i))))

(percent (make-center-percent 400 3))

;2.13
;math..

;2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define a (make-center-percent 100 5))
(define b (make-center-percent 200 2))
(define aa (div-interval a a))
(define ab (div-interval a b))
;aa

;(percent ab)
(define bbaa (par1 a b))
(define aabb (par2 a b))
aabb
bbaa

(define consoffour (cons 1 (cons 2 (cons 3 (cons 4 "null")))))

consoffour