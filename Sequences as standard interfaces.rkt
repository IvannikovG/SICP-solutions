#lang racket

(define (fib n)
   (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
   (cond ((= count 0) b)
         ((even? count)
          (fib-iter a
                    b
                    (+ (* p p) (* q q))     ; compute p'
                    (+ (* 2 p q) (* q q))   ; compute q'
                    (/ count 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

;Sequences as standart interfaces

(define (square x)(* x x))

(define (sum-odd-squares tree)
  (cond ((null? tree) null)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))


(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 10 (list 1 2 3 4 55))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares1 tree)
  (accumulate + 0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

;(define (even-fibs n)
 ; (accumulate cons
   ;           null
  ;            (filter even?
    ;                  (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1
              (map square
                      (filter odd? sequence))))

;2.33

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(map1 square (list 1 2 3))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 ))

;2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;2.35

(define (count-leaves tree)
  (accumulate (lambda (t acc) (if (list? t)
                              (+ acc (count-leaves t))
                              (+ acc 1)))
              0
              tree))

(count-leaves (list (list 1 2) (list 1 2 3) 1))

;2.36

(define (accumulate-n op init sequence)
  (if (null? (car sequence))
      null
      (cons (accumulate op init (map car sequence))
            (accumulate-n op init (map cdr sequence)))))

;2.37

(define (dot-product x y)
  (accumulate + 0 (map * x y)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) matrix-*-vector cols row) m)))

;(define v (list 1 3 -5))
;(define w (list 4 -2 -1))
;(dot-product v w)

;(define m (list (list 1 2 3) (list 4 5 6)))
;(define v1 (list 1 2 3))
;(matrix-*-vector m v1)

 ;(define a (list (list 14 9 3) (list 2 11 15) (list 0 12 17) (list 5 2 3)))
 ;(define b (list (list 12 25) (list 9 10) (list 8 5)))
 ;(matrix-*-matrix a b)

;2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(fold-left / 1 (list 1 2 3))
(fold-right / 1 (list 1 2 3))

;2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse1 sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse1 (list 1 2 3))

