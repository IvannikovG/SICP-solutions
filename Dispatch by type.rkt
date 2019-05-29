#lang sicp

;structure of a record -> (cons KEY (cons value ...CDR))

;(define concer (cons 'a (cons 'b (cons 'y 'y))))

(define (summate x y)
  (+ x y))

(define (multiplicate x y)
  (* x y))

(define (summator x y)
  (+ x x y y))

(define (multiplicator x y)
  (* x x y y))


(define (first-element table)
  (car table))

(define (type-tag element)
  (car element))

(define (type-proc element)
  (car (cdr element)))

(define (assoc key table)
  (cond ((null? table) false)
        ((eq? key (type-tag (first-element table)))
         (cdar table))
        (else (assoc key (cdr table)))))

(define (finder name table)
  (cond ((null? table) false)
        ((eq? name (car (first-element table)))
         (cdr (first-element table)))
        (else ((finder name (cdr table))))))

(define (get type proc)
  (let ((type-table (assoc type local-table)))
    (if type-table
        (finder proc type-table)
        false)))

(define (sum x y)
 ((get 'standart 'sum) x y))

;(sum 10 12)

(define local-table (list (cons 'standart (list (cons 'sum summate)
                                                (cons 'multiply multiplicate)))
                          (cons 'nonstandart (list (cons 'sum summator)
                                                   (cons 'multiply multiplicator)))))

(define (put type proc-name proc-body)
  (let ((type-table (assoc type local-table)))
    (display type-table)
    (if type-table
        (begin
          (set! type-table (list (cons proc-name proc-body) type-table))
          (newline)
          (display type-table))
        (begin
          (set! local-table (list (cons type (cons proc-name proc-body)) local-table))
          (newline)
          (display local-table)))))
    
(define (division x y)
  (/ x y))

(put 'standat 'division division)

;(put 'super 'proff proff)
;local-table
