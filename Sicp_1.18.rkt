;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Sicp_1.18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (double a) (+ a a))

(define (halve a) (/ a 2))



(define (fast-iter number counter)
  (expt-iter number counter 0))

(define (expt-iter number counter product)
        (cond ((= counter 0) product)
              ((iseven? counter) (expt-iter (double number) (halve counter) product))
              (else (expt-iter number (- counter 1) (+ number product)))))

(define (iseven? x)
  (= (remainder x 2) 0))

(fast-iter 3 4)
        