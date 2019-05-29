#lang sicp

(define (make-queue) (cons '() '()))
(define newQ (make-queue))

(define (front-ptr queue) (car queue))
;;(front-ptr newQ)
(define (rear-ptr queue) (cdr queue))
;;(rear-ptr newQ)

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
;;(set-rear! newQ 'b)

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (front-queue queue)
  (if (null? (front-ptr queue))
      (error "Procedure is called if an empty queue")
      (car (front-ptr queue))))
;;(front-queue newQ)

(define (empty-queue? queue)
  (null? (front-ptr queue)))
;;(empty-queue newQ)

(define (insert-queue! queue item)
  (let ((new-item (cons item '())))
    (cond ((empty-queue? queue)
           (set-car! queue new-item)
           (set-cdr! queue new-item)
           queue)
          (else (set-cdr! (rear-ptr queue) new-item)
                (set-rear-ptr! queue new-item)
                queue))))

(insert-queue! newQ 'hu)
(insert-queue! newQ 'io)
(insert-queue! newQ 'jk)

(define (delete-queue queue)
  (if (empty-queue? queue)
      (error "Procedure is called with an empty procedure")
      (set-front-ptr! queue (cdr (front-ptr queue))))
  queue)

(delete-queue newQ)

;; deque representation - (cons (cons 'b '()) '())

(define (make-deque)
  (cons '() '()))

(define (front-dtr deque)
  (car deque))

(define (rear-dtr deque)
  (cdr deque))

(define (empty-deque? deque)
  (null? (front-dtr deque)))

(define (set-front-dtr! deque item)
  (set-car! deque item))

(define (set-rear-dtr! deque item)
  (set-cdr! deque item))

(define (front-insert-deque! deque item)
  (let ((new-item (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-car! deque new-item)
           (set-cdr! deque new-item)
           deque)
          (else (begin (set-cdr! new-item (front-dtr deque))
                        (set-cdr! (car (front-dtr deque)) new-item)
                        (set-front-dtr! deque new-item)
                        deque)))))
                        
(define (rear-insert-deque! deque item)
  (let ((new-item (cons (cons 'b '()) '())))
    (cond ((empty-deque? deque)
           (set-car! deque new-item)
           (set-cdr! deque new-item)
           deque)
          (else (begin
                  (set-cdr! (car new-item) (rear-dtr deque))
                  (set-cdr! (rear-dtr deque) new-item)
                  (set-rear-dtr! deque new-item)
                  deque)))))


(define newD (make-deque))
(front-insert-deque! newD 'f)
(front-insert-deque! newD 'g)
(front-insert-deque! newD 'm)
(rear-insert-deque! newD 'n)
(rear-insert-deque! newD 'p)

;Representation of tables

(define (make-table) (list '*table*))

(define (lookup key table)
(let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
false)))

(define (assoc key records)
  (cond ((null? records) #false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define tabel (make-table))

(define (insert-table! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (begin (set-cdr! table (cons (cons key value) (cdr table)))
               table)))
  'ok)

(insert-table! 'n 10 tabel)
(insert-table! 'k 13 tabel)
tabel
(insert-table! 'n 11 tabel)
tabel

(define (look-up key1 key2 table)
  (let ((subtable (assoc key1 table)))
    (if subtable
        (let ((record (assoc key2 subtable)))
        (if record
            (cdr record)
            false)))))

(define (insert-table2! value key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr table))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key2 value) subtable))))
        (set-cdr! table (cons (list key1 (cons key2 value)) (cdr table)))))
  'ok)

(define (local-table) (list '*table*))

(define (lookup-n key-list)
      (define (iter keys table)
        (cond ((null? keys) false)
              ((null? (cdr keys))
               (let ((record (assoc (car keys) (cdr table))))
                 (if record
                     (cdr record)
                     false)))
              (else (let ((subtable (assoc (car keys) (cdr table)))) ;;??
                      (if subtable
                          (iter (cdr keys) subtable)
                          (iter (cdr keys) table))))))
      (iter key-list local-table))

(define (insert! keys-list value table)
  (define (iter keys tab)
    (cond ((null? keys) false)
          ((null? (cdr keys))
           (let ((record (assoc (car keys) (cdr tab))))
             (if record
                 (set-cdr! record value)
                 (set-cdr! tab
                           (cons (cons (car keys) value) (cdr table))))))
          (else (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                      (iter (cdr keys) subtable)
                      (set-cdr! table
                                (cons (cons (car keys) value) (cdr table))))))))
  (iter keys-list table))

