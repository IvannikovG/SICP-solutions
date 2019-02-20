;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname uprajnenie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (square x) (* x x))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (test a n) 
  (= (expmod a n n) a))

(define (test-all n) 
  (test-all-from n 1))

(define (test-all-from n start) 
  (if (< start n) 
        (if (test start n) 
            (test-all-from n (+ start 1)) 
            false) 
        true))

