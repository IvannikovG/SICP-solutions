;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sicp_1.16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (square x) (* x x))
(define (even?n n)
    (= (remainder n 2) 0))

(define (fastest number counter)
      (fast-expt number counter 1))
(define (fast-expt number counter product)
    (cond ((= counter 0) 1)
          ((even?n counter) (square (fast-expt number (/ counter 2) (* number product))))
          (else (* number (fast-expt number (- counter 1) (* number product))))))
  
(fastest 2 10)
