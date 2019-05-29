#lang racket
;Queues

(define queue (cons 1 (cons 2 (cons 3 (cons 4 null)))))

;(define q ("front-ptr" (cons a
;          ("rear-ptr" cons c))

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (rear-queue queue)
  (if (empty-queue? queue)
      null
      (mcar (rear-ptr queue))))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT is called with empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue queue)
  (cond ((empty-queue? queue)
         (error "Delete is called with an empty queue"))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

(define q (make-queue))
(insert-queue q 'a)
(insert-queue q 'b)
;(delete-queue q)
;(delete-queue q)
(empty-queue? q)

;3.21
;(define (print-queue queue)
  ;(if (empty-queue? queue)
 ;     null
   ;   (display (front-ptr queue))))

(define (print-queue queue)
  (if (empty-queue? queue)
      null
      (begin (front-queue queue) (rear-queue queue))))
;(print-queue q)

;3.22

(define (make-queue-n)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (mcar front-ptr))
    (define (insert-queue item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair))
            (else (set-mcdr! rear-ptr new-pair)
                  (set-rear-ptr! new-pair)))))
    (define (delete-queue)
      (cond ((empty-queue?)
            (error "DELETE is called with an empty queue"))
            (else (set-front-ptr! (mcdr front-ptr)))))
    (define (print-queue)
      (front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'print-queue) print-queue)
            ((eq? m 'delete-queue) delete-queue)
            ((eq? m 'insert-queue) insert-queue)
            ((eq? m 'front-queue) front-queue)
            (else (error "Call of undefined operation" m))))
    dispatch))
   
;3.23

(define (make-deque)
  (mcons '() '()))

(define (rear-dtr deque)
  (cdr deque))

(define (front-dtr deque)
  (car deque))

(define (empty-deque? deque)
  (null? (front-dtr deque)))

(define (set-front-dtr! deque item)
  (set-mcar! deque item))

(define (set-rear-dtr! deque item)
  (set-mcdr! deque item))

(define (insert-deque deque item edge)
  (let ((new-item (cons (cons item '()) '())))
    (cond ((and (empty-deque? deque) (or (eq? edge 'front) (eq? edge 'rear)))
                (set-front-dtr! deque item)
                (set-rear-dtr! deque item))
          ((eq? edge 'front)
           (set-mcdr! new-item (front-dtr deque))
           (set-mcdr! (car (front-dtr deque)) new-item))
           (set-front-dtr! deque new-item)
          ((eq? edge 'rear)
           (set-mcdr! (rear-dtr deque) new-item)
           (set-mcdr! (car new-item) (rear-dtr deque))
           (set-rear-dtr! deque new-item))
          (else (error "unbound identifier")))))
           
           
(define (insert-front deque item)
  (insert-deque deque item 'front))

(define (insert-rear deque item)
  (insert-deque deque item 'rear))

; Representation of tables

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (mcar (mcar records)) key) (mcar records))
        (else (assoc key (mcdr records)))))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        (false))))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (mcons (mcons key value)
                                (mcdr table))))))

(define (make-table)
  (mcons '*table* null))

(define t (make-table))
(insert! 'a 4 t)
(insert! 'b 5 t)
t

(define (lookup2 key1 key2 table)
  (let ((subtable (assoc key1 (mcdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert2 key1 key2 value table)
  (let ((subtable (assoc key1 (mcdr table))))
    (if subtable
        (let ((record (assoc key2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                         (mcons (mcons key2 value)
                               (mcdr subtable)))))
        (set-mcdr! table
                  (mcons key1 (mcons key2 value) (mcdr table))))))

(define (make-table2)
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
              false)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key2 value) (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mcons key1
                                     (mcons key2 value))
                              (mcdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'insert!) insert!)
            ((eq? m 'lookup) lookup)
            (else (error "Unbound method"))))
    dispatch))
    
    


(define operation-table (make-table))
(define ta operation-table)
ta
;(define get (operation-table 'lookup-proc))
;(define put (operation-table 'insert-proc))

;3.24
(define (make-table3)
  (let ((local-table (mcons '*table* null)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (mcar (mcar records)) (mcar records)))
            (else (assoc key (mcdr records)))))

    (define (lookup key-list)
      (define (iter keys table)
        (cond ((null? keys) false)
              ((null? (mcdr keys))
               (let ((record (assoc (mcar keys) (mcdr table))))
                 (if record
                     (mcdr record)
                     false)))
              (else (let ((subtable (assoc (mcdr keys) (mcdr table)))) ;;??
                      (if subtable
                          (iter (mcdr keys) subtable)
                          (iter (mcdr keys) table))))))
      (iter key-list local-table))

    (define (insert! key-list value)
      (define (iter keys table)
        (cond ((null? keys) false)
              ((null? (mcdr keys))
               (let ((record (assoc (mcar keys) (mcdr table))))
                 (if record
                     (set-mcdr! record value)
                     (set-mcdr! table
                                (mcons (mcons (mcar keys) value) (mcdr table))))))
              (else
               (let ((subtable (assoc (mcar keys) (mcdr table))))
                 (if subtable
                     (iter (mcdr keys) subtable)
                     (set-mcdr! table
                                (mcons (mcons (mcar keys) (iter (mcdr keys) '()))
                                       (mcdr table))))))))
      (iter key-list local-table) 'ok)
    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
            ((eq? m lookup) lookup)
            (else (error "Unbound method"))))
    dispatch))
                      
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (entry tree)
  (mcar tree))

(define (left-branch tree)
  (mcar (mcdr tree)))

(define (right-branch tree)
  (mcar (mcdr (mcdr tree))))

(define (adjoin-set x set)
  (cond ((null? set)
         (make-tree '() '()))
        ((= (entry x) (mcar (entry set)) set)
        ((> (entry x) (mcar (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))
        ((< (entry x) (mcar (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))))))

(define (make-table4)
  (let ((local-table '()))
    
  (define (lookup set key)
    (cond ((< key (mcar set)) (lookup (left-branch set) key))
          ((> key (mcar set)) (lookup (right-branch set) key))
          (eq? (mcar set) key) (entry set)))

  (define (insert! key value)
    (let ((record (lookup local-table key)))
      (if record
          (set-mcdr! record value)
          (set! local-table
                (adjoin-set (mcons key value)
                            local-table)))))
    (define (get key)
      (lookup key local-table))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            (else (error "unbound method"))))
    dispatch))


(define (memo-fib n)
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((p-c-r (lookup table x)))
        (if p-c-r
            p-c-r
            (let ((result (f x)))
              (insert! result table)
              result))))))


