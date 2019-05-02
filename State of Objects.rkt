#lang racket
;3.1 State of objects
;3.1
(define (make-accumulator amount)
  (lambda (plus)
    (set! amount (+ amount plus))
  amount))

(define A (make-accumulator 5))
(A 10)
(A 10)

;3.2

(define (make-monitored function)
  (let ((counter 0))
   (lambda (arg)
        (cond ((eq? arg 'how-many-calls?) counter)
              ((eq? arg 'reset-count) (set! counter 0))
              (else (begin (set! counter (+ counter 1)) (function arg)))))))

(define (make-monitored-x f)
  (let ((counter 0))
    (define (mf p)
      (cond ((eq? p 'how-many-calls?) counter)
            ((eq? p 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) (f p)))))
    mf))
(define s (make-monitored-x sqrt))
(s (s 25))
(s 'how-many-calls?)

;3.3

(define (make-account balance password)
  (define (call-the-cops) "Call the cops")
  (let ((counter 0)
        (limit 7))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
               balance)
          "not enough money"))
    (define (deposit amount)
      (set! balance (+ balance amount))
       balance)
    (define (dispatch pass message)
      (if (not (eq? pass password))
          (lambda (amount)
            (if (>= counter limit)
                (call-the-cops)
                (begin (set! counter (+ counter 1)) (error "Wrong password"))))
          (begin (set! counter 0) (cond ((eq? message 'withdraw) withdraw)
                                       ((eq? message 'deposit) deposit)
                                       (else (error "Undefined method for the procedure"))))))
      dispatch))
    
(define acc (make-account 100 'secret-password))
(print "f")
(newline)
((acc 'secret-password 'withdraw) 10)
((acc 'secret-password 'withdraw) 10)

;((acc 'some-other-password 'deposit) 50)

;(define rand
 ; (let ((x random-init))
  ;      (lambda ()
   ;       (set! x (rand-update x))
    ;      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random 1000000000) (random 100000000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;(estimate-pi 450000)

;3.5
(define (rand-update x) (random (expt 2 31)))
(define (random-init) (rand-update 0))
(define (random-in-range low high) 
   (let ((range (- high low))) 
     (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda () (P (random-in-range x1 y1)
                                    (random-in-range x2 y2)))))
(define (P x y) 
   (< (+ (expt (- x 5) 2) 
         (expt (- y 7) 2)) 
      (expt 3 2)))
;(estimate-integral P 2 8 4 10 100) 

(define (uno-circle-predicate x y)
  (>= 1 (+ (expt (random-in-range -1 1) 2)
           (expt (random-in-range -1 1) 2))))

(define (square x)
  (* x x))

(define (estimate-pi-n trials)
  (* 4 (estimate-integral uno-circle-predicate 1 1 3 3 trials)))
(estimate-pi-n 100000)

;3.6

(define rand
  (let ((counter random-init))
     (define (dispatch m)
       (cond ((eq? m 'generate)
              (begin (set! counter (random (expt 2 21)))
                     counter))
             ((eq? m 'reset)
              (lambda (value) (set! counter value)))
             (else (error "Unbound identifier"))))
     dispatch))

;Weakness and triumph of models
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount)) balance))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;3.7

(define (make-joint account password new-pass)
  (lambda (pass m)
    (cond ((eq? new-pass pass)
           (account password m))
          (else (error "Unbound method")))))


(define peter-acc (make-account 100 'secret))
((peter-acc 'secret 'withdraw) 10)
(define paul-acc (make-joint peter-acc 'secret 'secret2))
((paul-acc 'secret2 'withdraw) 10)
((paul-acc 'secret2 'withdraw) 10)

;3.8

(define (predicate x)
  (> x 0))

(define f
  (let ((count 1))
    (lambda (x)
      (set! count (* count x))
      count)))

(+ (f 1) (f 0))