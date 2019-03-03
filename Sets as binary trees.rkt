#lang racket
;Sets as Binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set) true))
        ((< x (entry set) (element-of-set? x (left-branch set))))
        ((> x (entry set) (element-of-set? x (right-branch set))))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set) set))
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                       remaining-elts))))))))


;2.65
(define (union-set tree1 tree2)
  (define (union-list set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (cons (car set1) (union-list (cdr set1) (cdr set2))))
          ((> (car set1) (car set2))
           (cons (car set2) (union-list set1 (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-list (cdr set1) set2)))))
  (list->tree (union-list (tree->list-2 tree1) (tree->list-2 tree2))))
          
(union-set (list->tree (list 1 2 3 4 5)) (list->tree (list 1 6 7 8 9)))

(define (intersection-set tree1 tree2)
  (define (intersection-list set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((= (car set1) (car set2))
           (cons (car set1) (intersection-list (cdr set1) (cdr set2))))
          ((> (car set1) (car set2))
           (intersection-list set1 (cdr set2)))
          ((< (car set1) (car set2))
           (intersection-list (cdr set1) set2)))) ; what is the point of returning intersection
  (list->tree (intersection-list (tree->list-2 tree1) (tree->list-2 tree2)))) ;as tree?

(intersection-set (list->tree (list 1 2 3 4 5)) (list->tree (list 1 6 7 8 9)))

;Sets and data search

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
        (car set-of-records))
         (else (lookup given-key (cdr set-of-records)))))

;2.66

(define (key record) (car record))
(define (data record) (cdr record))
(define (make-record key data) (cons key data))

(define database
  (list (make-record 1 'Zallin)
        (make-record 2 'Demash)
        (make-record 3 'Georgii)
        (make-record 4 'Ivannikov)))

(lookup 1 database)
(lookup 2 database)

(define (lookup-tree given-key set-of-records-tree)
  (cond ((null? set-of-records-tree) false)
        ((= given-key (key (car set-of-records-tree))) (car set-of-records-tree))
        ((> given-key (key (car set-of-records-tree)))
            (lookup-tree given-key (right-branch set-of-records-tree)))
        ((< given-key (key (car set-of-records-tree)))
            (lookup-tree given-key (left-branch set-of-records-tree)))))

(define tree-db (list->tree database))

(lookup-tree 2 tree-db)
(lookup-tree 4 tree-db)
