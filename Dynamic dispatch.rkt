#lang racket

;Dynamic dispatch

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))


;for the first department

(define (make-employee name position salary)
  (cons name (cons position salary)))

(define (get-name employee)
  (car employee))

(define (get-position employee)
  (cadr employee))

(define (get-salary employee)
  (cddr employee))

(define new-employee (make-employee 'test-name 'test-position 'test-salary))

(get-name new-employee)
(get-position new-employee)
(get-salary new-employee)

(define employees (list (make-employee 'GregHouse 'doctor 15000)
                        (make-employee 'LisaCuddy 'doctor 17000)
                        (make-employee 'OmarEpps 'butcher 10000)
                        (make-employee 'GeorgiiIvannikov 'laundryOperator 1000)))

(define (get-employee name)
  (define (find-employee employees)
    (cond ((null? employees) 'not-found)
          ((eq? name (get-name (car employees))) (car employees))
          (else (find-employee (cdr employees)))))
  (find-employee employees))

(get-employee 'LisaCuddy)
(get-employee 'JamesWilson)

;for second department

(define (make-record name salary experience)
  (list name salary experience))

(define (get-record-name record)
  (car record))

(define (get-record-salary record)
  (cadr record))

(define (get-record-experience record)
  (caddr record))

(define test-record (make-record 'Dmitrii 'Medvedev 'NoSignificantExperience))
(get-record-name test-record)
(get-record-salary test-record)
(get-record-experience test-record)

(define employee-records (list (make-record 'AlexisTexas 150000 '5Years)
                                (make-record 'ElsaJean 50000 '3Years)
                                (make-record 'JaydenJames 50000 '8Years)))

(define (find-record name records)
  (cond ((null? records) 'notfound)
        ((eq? (get-record-name (car records)) name) (car records))
        (else (find-record name (cdr records)))))

(find-record 'AlexisTexas employee-records)
(find-record 'Elsa employee-records)

(define (install-clinic)
   (define (make-employee name position salary)
    (cons name (cons position salary)))
   (define (get-name employee)
    (car employee))
   (define (get-position employee)
     (cadr employee))
   (define (get-salary employee)
     (cddr employee))
   (define (get-employee name)
     (define (find-employee employees)
       (cond ((null? employees) 'not-found)
             ((eq? name (get-name (car employees))) (car employees))
             (else (find-employee (cdr employees)))))
     (find-employee employees))
  (define employees (list (make-employee 'GregHouse 'doctor 15000)
                        (make-employee 'LisaCuddy 'doctor 17000)
                        (make-employee 'OmarEpps 'butcher 10000)
                        (make-employee 'GeorgiiIvannikov 'laundryOperator 1000)))
 (put 'get-employee 'clinic get-employee)
  (put 'get-salary 'clinic get-salary)
  'done)

(define (install-my-personal-team)
  (define (make-record name salary experience)
    (list name salary experience))
  (define (get-record-name record)
    (car record))
  (define (get-record-salary record)
    (cadr record))
  (define (get-record-experience record)
    (caddr record))
  (define (find-record name)
    (define records (list (make-record 'AlexisTexas 150000 '5Years)
                                 (make-record 'ElsaJean 50000 '3Years)
                                 (make-record 'JaydenJames 50000 '8Years)))
    (define (finder name employee-records)
    (cond ((null? employee-records) 'notfound)
          ((eq? (get-record-name (car employee-records)) name) (car employee-records))
          (else (finder name (cdr employee-records)))))
    (finder name records))
  (put 'get-employee 'my-personal-team find-record)
  (put 'get-salary 'my-personal-team get-record-salary)
  'done)

(install-clinic)

;a).
(define (get-employee-record name department)
  ((get 'get-employee department) name))

(get-employee-record 'LisaCuddy 'clinic)

;b).

(define (get-employee-salary name department)
  ((get 'get-salary department)
   (get-employee-record name department)))

;c).

(define (find-employee-record name departments)
  (if (null? departments) null
      (let ((record (get-employee-record name (car departments))))
                    (if (null? record)
                        (find-employee-record name (cdr departments))
                        record))))

(find-employee-record 'GregHouse (list 'clinic 'my-persona-team))