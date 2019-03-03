#lang racket
;Symbolic data

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(memq 'apple '(pear banana prune))
;(memq 'apple '(pear apple banana c))

(define (memq-true item x)
  (cond ((null? x) false)
        ((eq? item (car x)) true)
        (else (memq-true item (cdr x)))))

;(memq-true 'apple '(pear apple banana c))
;(memq-true 'apple '(pear banana prune))

;2.53

;(list 'a 'b 'c)  ;'(a b c)
;(cdr '((x1 x2) (y1 y2)))  ;'((y1 y2))
;(cadr '((x1 x2) (y1 y2)))  ;'(y1 y2)
;(pair? (car '(a short list)))  ;#f
;(memq 'red '((red shoes) (blue socks)))  ;#f
;(memq 'red '(red shoes blue socks))  ;'(red shoes blue socks)

;2.54

(define (equal? list1 list2)
  (cond ((and (pair? list1) (pair? list2)) (if (eq? (car list1)
                                                    (car list2))
                                               (equal? (cdr list1) (cdr list2))
                                               false))
        ((eq? list1 list2) true)
        (else false)))

;(equal? '(this is a list) '(this is a list))

;2.55

;(car ''abracadabra) ; = (car (quote (quote ''abracadabra)))

;Symbolic differentiation

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else (list '** base exp))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product
                        (exponent exp)
                         (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
          
        (else
         (error "undefined type -- DERIV" exp))))

;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
;(deriv '(* (* x y) (+ x 3)) ' x)
;(deriv '(** x 2) 'x)
;(deriv '(* x y (+ x 3)) 'x)

;2.58
(define (make-sum1 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum?1 x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend1 s)
  (car s))

(define (augend1 s)
  (caddr s))

(define (make-product1 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product?1 x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier1 p)
  (car p))

(define (multiplicand1 p)
  (caddr p))

(define (deriv1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?1 exp)
         (make-sum1 (deriv (addend1 exp) var)
                   (deriv (augend1 exp) var)))
        ((product?1 exp)
         (make-sum1 (make-product1 (multiplier1 exp)
                                 (deriv (multiplicand1 exp) var))
                   (make-product1 (deriv (multiplier1 exp) var)
                                 (multiplicand1 exp))))
        ((exponentiation? exp)
         (make-product1 (make-product1
                        (exponent exp)
                         (make-exponentiation (base exp) (make-sum1 (exponent exp) -1)))
                       (deriv (base exp) var)))
          
        (else
         (error "undefined type -- DERIV" exp))))

;(deriv1 '(x + 3) 'x)
;(deriv1 '(x * y) 'x)


;2.3.3 Sets

(define (element-of-set?-unsorted x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set?-unsorted x (cdr set)))))

(define (adjoin-set-unsorted x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set-unsorted set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set?-unsorted (car set1) set2)
         (cons (car set1) (intersection-set-unsorted (cdr set1) set2)))
        (else (intersection-set-unsorted (cdr set1) set2))))

(intersection-set-unsorted (list 1 2 3) (list 2 3 4))

;2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set?-unsorted (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 1 4 5 6))

;2.60

(define (adjoin-set1 x set)
  (cons x set))

(define (union-set1 set1 set2)
  (append set1 set2))


;Sets as sorted structures

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))))))

;(intersection-set (list 12 13 14 15 16) (list 13 14 16 15))

;2.61

(define (adjoin-set set x)
  (cond ((null? set) (list '() x))
        ((= x (car set)) set)
        ((> (car set) x) (cons x set))
        ((< (car set) x) (cons (car set) (adjoin-set (cdr set) x)))))

(adjoin-set (list 2 4 6 8) 4)
(adjoin-set (list 2 4 6 8) 1)
(adjoin-set (list 2 4 6 8) 9)

;2.62

(define (union-set-sorted set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((and (not (null? set1)) (not (null? set2)))
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set-sorted (cdr set1) (cdr set2))))
                 ((> x1 x2) (cons x1 (union-set-sorted set1 (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set-sorted (cdr set1) set2))))))))
          
;(union-set-sorted (list 1 2 3 5 6) (list 1 2 7 8 9))