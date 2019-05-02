#lang racket

(require rnrs/mutable-pairs-6)

;Environments
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;3.12

(define (type-tag x)
  (if (pair? x)
      (car x)
      (mcar x)))

(define (contents x)
  (if (pair? x)
      (cadr x)
      (mcar (mcdr x))))

(define (apply-generic op args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (print proc)
      (if proc
          (apply proc (map contents args))
          (error "bad conclusion")))))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(put 'append 'mcons append!)
(put 'append 'cons append)

(define (appender x y)
  (apply-generic 'append (list x y)))

(define (make-mcons x y)
  (cons 'mcons (mcons x y)))
(define (make-cons x y)
  (cons 'cons (cons x y)))

;(define x (mcons 'a 'b))
;(define y (mcons 'c 'd))
;(define x2 (make-mcons 'a 'b))
;(define y2 (make-mcons 'a 'b))
;(define z (appender x y))
;(define w (appender x2 y2))

;3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;(define z (make-cycle (list 'a 'b 'c)))
;last cdr doesnt have null at the end, link to the first
;on the list, instead

;3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
v
;(define w (mystery v))

