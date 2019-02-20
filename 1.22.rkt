#lang racket
(define (square x) (* x x))
(define (divides a b) (= (remainder b a ) 0))

(define (iseven? n)
  (= (remainder n 2) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((= n test-divisor) n)
        ((> (square test-divisor) n) n)
        ((divides test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime)
  (current-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime (- (runtime) start-time)))
        (else 0)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)
  

(define (search-for-primes number counter)
  (if (= counter 0) (timed-prime-test number)
     (if (iseven? number)(search-for-primes (+ number 1) counter)
         (cond ((timed-prime-test number) (search-for-primes (+ number 2) (- counter 1)))
         (else (search-for-primes (+ number 1) counter))))))
            
                                                        

         
(search-for-primes 1000 15)