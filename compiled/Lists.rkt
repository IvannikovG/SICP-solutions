#lang racket

;Sequences

(define consoffour1 (cons (cons 1 2) (cons 3 4)))
(define consoffour2 (cons (cons 1 (cons 2 3)) 4))
(define newlist (list "a" "b" "c" "abc"))
(define one-through-four (list 1 2 3 4))
(define biglist (list 1 2 3 4 45 32 54323 23422 3422 "c" "fd" "ns" "we" 12 3 21 "21" "cd" 213 "rre"))
(define list1 (list 1 2 3 4))
(define list2 (list 5 6 7 8))

(define (list-ref list position)
  (if (= position 0)
      (car list)
      (list-ref (cdr list) (- position 1))))

(list-ref biglist 5)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (iter-length items)
  (newlength items 0))

(define (newlength items acc)
  (define (iter elements counter)
    (if (null? elements)
        counter
        (iter (cdr elements) (+ counter 1))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append list1 list2)

;2.17
(define (last-pair3 list)
  (list-ref list (- (iter-length list) 1))) ;just returns the position

(define (last-pair2 items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items))))

(last-pair2 list1)

;2.18

(define (reverse list)
  (define (iter items acc)
  (if (null? items)
      acc
      (iter (cdr items) (cons (car items) acc))))
  (iter list '()))

;2.19
(define (except-first-denomination coin-values)
  (cdr (coin-values)))

(define (first-denomination coin-values)
  (car (coin-values)))

(define (no-more? coins)
  (if (null? coins)
      true
      false))

;2.20
(define (iseven? n)
  (= (remainder n 2) 0))

(define (same-parity first . rest)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items) (if (= (remainder first 2) (remainder (car items) 2))
                              (append acc (list (car items)))
                              acc))))
  (iter rest (list first)))

(same-parity 1 2 3 4 5 6 7)

;MAP

(define (scale-list-old list factor)
  (if (null? list)
      null
      (cons (* (car list) factor) (scale-list-old (cdr list) factor))))

;(scale-list-old list1 2)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))

(define (scale-list list factor)
  (map (lambda (x) (* x factor)) list))

(define (square x)
  (* x x))
;(scale-list list1 4)

;2.21

(define (square-list-old items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list-old (cdr items)))))
;(square-list-old list1)

(define (square-list1 items)
  (map (lambda (x) (* x x)) items))
;(square-list list1)

;2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items null))

(square-list list1)

;2.23
(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items)) (for-each proc (cdr items)))))