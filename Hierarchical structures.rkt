#lang racket

;Hierarchical structures
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
         (else (+ (count-leaves (car x))
                  (count-leaves (cdr x))))))

;(display (list 1 (list 2 (list 3 4))))

;2.25
(define list1 (list 1 3 (list 5 7) 9))

(define list2 (list (list 7)))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr list1)))))
;(cdaddr list1)
(caar list2)
(car(cdr(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

;(append x y)
;(cons x y)
;(list x y)

;2.27

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (cond ((null? items) null)
        ((pair? (car items)) (append (deep-reverse (cdr items))
                                     (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car items))))))
        

(define xl (list (list 1 2) (list 3 4)))
(reverse xl)
(deep-reverse xl)

;2.28
;(define (fringe list)
 ; (define (iter items acc)
  ;  (cond ((null? items) acc)
   ;       ((pair? (car items)) (append (list (fringe (car items))) (fringe (cdr items))


(define (fringe node)
  (cond ((null? node) null)
        ((pair? (car node)) (append (car node) (fringe (cdr node))))
        (else (list node))))
  
(define t (list (list 1 2) (list 3 4)))
(fringe t)
                    
;2.29

;a
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define fm (make-mobile 3 5))

(right-branch fm)

;b
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
   
(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))

(total-weight a)

;c

(define (branch-rmoment branch)
  (* (branch-weight branch)
     (branch-length branch)))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (isbalanced? (branch-structure branch))
      (true)))

(define (isbalanced? mobile)
  (and (= (branch-rmoment (left-branch mobile))
          (branch-rmoment (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((pair? tree) (cons (scale-tree (car tree) factor)
                            (scale-tree (cdr tree) factor)))
        (else (* factor tree))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree-map tree factor)
  (map (lambda (el)
         (if (pair? el)
             (scale-tree-map el factor)
             (* el factor))) tree))

(scale-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;2.30

(define (square x)
  (* x x))

(define (square-tree-map tree)
  (map (lambda (el)
         (if (pair? el)
             (square-tree-map el)
             (square el))) tree))

(define (square-tree tree)
  (cond ((null? tree) null)
        ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (square tree))))
        

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
(list 6 7)))

;2.31

(define (tree-map func tree)
  (map (lambda (el)
         (if (pair? el)
             (tree-map func el)
             (func el))) tree))

(define (square-tree1 tree) (tree-map square tree))

(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
(list 6 7)))

;2.32

(define (subsets s)
   (if (null? s)
       (list null)
       (let ((rest (subsets (cdr s))))
          (append rest (map (lambda (x) (cons (car s) x))
                            rest)))))