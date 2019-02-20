#lang racket

(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (prime? n)
(= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (runtime) 
  (current-milliseconds))

(define (start-prime-test n start-time) 
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      (report-prime n runtime start-time)))
      

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

( provide divides? prime? smallest-divisor runtime start-prime-test report-prime timed-prime-test )
           
      
                      
                          