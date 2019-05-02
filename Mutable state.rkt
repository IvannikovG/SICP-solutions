#lang racket

(require rnrs/mutable-pairs-6)

(define x (mcons 'a 'b))
(define z1 (mcons x x))
(define z2 (mcons (mcons 'a 'b) (mcons 'a 'b)))

(define (set-wow! x)
  (set-mcar! (mcar x) 'wow)
  x)

(set-wow! z1)
(set-wow! z2)

;3.16
(define (count-pairs-n x)
  (if (not (pair? x))
      0
      (+ (count-pairs-n (car x))
         (count-pairs-n (cdr x))
         1)))

;3.17
(define (count-pairs pairs)
  (let ((checked '()))
    (define (iterator x)
      (if (or (not (pair? x)) (memq x checked))
          0
          (begin (set! checked (cons x checked))
                 (+ (iterator (car x))
                    (iterator (cdr x))
                    1))))
    (iterator pairs)))
   
(define pairs (list null (cons (cons (cons 4 5) 4) (cons 4 (cons 5 null))) 'b))
(count-pairs pairs)

;3.18
(define (has-cycle? list)
  (let ((checked '()))
    (define (iterator x)
      (cond ((null? x) false)
            ((memq (mcar x) checked) true)
            (else (set! checked (mcons (car x) checked))
                  (iterator (mcdr x)))))
    (iterator list)))

;3.19 turtle and hare
(define (hascycle? list)
  (define (chase turt hare)
    (cond ((or (null? turt) (null? hare)) false)
          ((eq? (mcar turt) (mcar hare)) true)
          (else (chase (mcdr turt) (mcdr (mcdr hare))))))
  (chase list (mcdr list)))

(define l1 (mcons 'a (mcons 'b (mcons 'c null))))
(define l2 (mcons 'a (mcons 'b (mcons 'c null))))
(set-cdr! (mcdr (mcdr l2)) l2)
(hascycle? l1)
(hascycle? l2)