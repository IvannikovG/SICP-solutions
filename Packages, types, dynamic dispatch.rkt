#lang racket

;Polynoms arythmetics

(define (square x)
  (* x x))

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "error")))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (negate-scheme scheme-number)
    (- (contents scheme-number)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'negate '(scheme-number) negate-scheme)
  'done)

(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
   ;; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (negate-rational rational)
    (make-rational (- (numer rational)) (denom rational)))
;; интерфейс к остальной системе
(define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'negate '(rational) negate-rational)
'done)


(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures 
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(install-rectangular-package)

;; Polar Complex Package

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(install-polar-package)

;; Complex number package

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (negate-complex complex)
      (make-complex-from-real-imag (- (real-part (cdr complex)))
                                   (- (imag-part (cdr complex)))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'negate '(complex) negate-complex)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)


(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (install-zero-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put '=zero? '(scheme-number) (lambda (n) (= 0 n)))
  (put '=zero? '(rational) (lambda (n) (= 0 (numer n))))
  (put '=zero? '(complex) (lambda (n) (and (= (imag-part n) 0)
                                      (= (real-part n) 0)))))

(define (=zero? n) (apply-generic '=zero? n))

(define (install-polynomial-package)

  
  ;Inner procedures
  ;Representation of Poly
  
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ;Representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  (define (rest-terms term-list) (cdr term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

 
  (define (the-empty-termlist term-list)
    (apply-generic 'the-empty-termlist term-list))
  (put 'the-empty-termlist 'dense '(dense))
  (put 'the-empty-termlist 'sparce '(sparce))
  
  (define (empty-termlist? term-list)
    (apply-generic 'the-empty-termlist term-list))
  (put 'empty-termlist? 'dense (lambda (term-list)
                                    (null? term-list)))
  (put 'empty-termlist? 'sparce (lambda (term-list)
                                    (null? term-list)))
  
  
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (put 'first-term 'dense (lambda (term-list) (list (- (length
                                                        (cdr term-list)) 1)
                                                    (car (cdr term-list)))))
  (put 'first-term 'sparce (lambda (term-list) (cadr term-list)))

  

  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polynoms from different variables" (list p1 p2))))

  ;procedures which are used by addpoly
  
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1))
                 (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms l1 (rest-terms l2))))
                   (else (adjoin-term
                          (make-term (order t1) (add (coeff t1)
                                                     (coeff t2)))
                          (add-terms (cdr l1) (cdr l2)))))))))


  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  ;procedures used by mulpoly

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))

  (define (mul-term-by-all-terms t1 l)
      (if (empty-termlist? l)
          (the-empty-termlist)
          (let ((t2 (first-term l)))
            (adjoin-term (make-term (+ (order t1)
                                       (order t2))
                                    (mul (coeff t1)
                                         (coeff t2)))
                         (mul-term-by-all-terms t1 (rest-terms l))))))
                         
  
  ;Interface for the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (define (negate-termlist term-list)
  (if (empty-termlist? term-list)
      the-empty-termlist
      (let ((f (first-term term-list)))
        (adjoin-term (make-term (order f) (negate (coeff f)))
                     (negate-termlist (cdr term-list))))))
  (put 'negate 'polynomial
       (lambda (polynomial) (make-polynomial (variable polynomial)
                                             (negate-termlist (term-list polynomial)))))
  (put 'sub '(polynomial polynomial)
       (lambda (x y) (tag (add-poly x (negate y)))))

  (define (zero-all-terms? terms)
    (cond ((empty-termlist? terms) #t)
          ((= 0 (coeff (first-term terms)))
           (zero-all-terms? (rest-terms terms)))
          (else false)))

  (define (=zero-poly? polynomial)
    (zero-all-terms? (term-list polynomial)))

  (put '=zero? '(polynomial) =zero-poly?)
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


(install-polynomial-package)

 (define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

;2.87

;(define (zero? polynomial)
 ; (if (null? polynomial)
  ;    true
   ;   (let ((coefficients (map coeff polynomial)))
    ;    (cond ((= 0 (car coefficients)) (zero? (cdr polynomial)))
     ;         (else false)))))

(=zero? (make-polynomial 'x '()))
(=zero? (make-polynomial 'x (list (list 4 (make-scheme-number 3))
                                    (list 2 (make-scheme-number 1))
                                    (list 0 (make-rational 2 3)))))

;2.88

(define (negate number)
  (apply-generic 'negate number))

(negate (make-scheme-number 5))
(negate (make-rational 5 6))
(negate (make-complex-from-real-imag 4 9))

;2.89

;(define (first-term term-list) 
 ;    (make-term (- (length term-list) 1) (car term-list))) 
  
 ;(define (adjoin-term term term-list) 
  ; (cond ((=zero? term) term-list) 
   ;      ((=equ? (order term) (length term-list)) (cons (coeff term) term-list)) 
    ;     (else (adjoin-term term (cons 0 term-list)))))

;2.90