#lang racket
; abstractions

;(define (linear-combination a b x y)
  ;(add (mul a x) (mul b y)))

(define x (cons 1 2)) ; car = 1, cdr = 2

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

;(gcd 9 3)

(define (make-rat x y)
  (let ((g (gcd x y)))
        (cons (/ x g) (/ y g))))

(define (numer pair) (car pair))
(define (denom pair) (cdr pair))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;(print-rat (mul-rat (make-rat -3 -3) (make-rat -2 -3)))

;2.2
;x,y points

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x-point y-point)
  (cons x-point y-point))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point  (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
               (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (print-point point)
  (newline)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

;(print-point (make-point 1 2))

;2.3

(define (make-rect-points high-left bottom-right)
  (cons high-left bottom-right))

(define (find-width rectangle)
  (abs (- (x-point (car rectangle)) (x-point (cdr rectangle)))))

(define (find-heigh rectangle)
  (abs (- (y-point (car rectangle)) (y-point (cdr rectangle)))))

(define (find-perimeter rectangle)
  (+ (* 2 (find-heigh rectangle)) (* 2 (find-width rectangle))))

(define (find-square rectangle)
  (* (find-heigh rectangle) (find-width rectangle)))

;> (define a (make-point 0 0))
;> (define b (make-point 2 10))
;> (define r (make-rect-points a b))
;> (display (find-perimeter r))
   
(define (make-rect-using-point-plus-sides point heigh width)
  (cons point (cons heigh width)))

(define (newcons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "ne 0 i ne 1" m)))) dispatch)


(define xer (newcons 121 211))

;(xer 0)

(define (newcar pair) (pair 0))
(define (newcdr pair) (pair 1))
(newcar xer)

;2.4

(define (consy x y)
  (lambda (m) (m x y))) ;cons принимает 2 аргумента и применяет к ним процедуру переданную с
                        ; помощью лямбды

(define (cars z)
  (z (lambda (p q) p)))

;(cars (consy x y)((consy x y) (lambda (p q) p))
;(cars (lambda (m) (m x y)) ((lambda (m) (m x y)) lambda (p q) p)))

(define (cdrs z)
  (z (lambda (p q) q)))

;2.5

(define (pow x n)
  (if (= n 1)
      x
      (* x (pow x (- n 1)))))

;(pow 2 2)

(define (cons1 x y)
  (* (pow 2 x) (pow 3 y)))

(define (car-plus-cdr pair number)
  (define (iter pair2 counter)
    (if (not (= (remainder pair2 number) 0))
        counter
        (iter (/ pair2 number) (+ counter 1))))
  (iter pair 0))

(define (cdr1 pair)
  (car-plus-cdr pair 3))

(define (car1 pair)
  (car-plus-cdr pair 2))

(define ter (cons1 2 3))

;(car-plus-cdr 23328 2)
;(car-plus-cdr 23328 3)
;(cdr1 ter)

;2.6 Isnt it crazy already

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))