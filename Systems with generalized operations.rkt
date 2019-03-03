#lang racket

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;Messages

;2.75

(define (square x) (* x x))

(define (make-from-real-imag-d x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "... is undefined " op))))
  dispatch)

(define (make-from-mag-ang-d r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else (error "... is undefined" op))))
  dispatch)

(define a (make-from-real-imag-d 1 2))
(a 'real-part)
(define b (make-from-mag-ang-d 4 5))
(b 'real-part)

;Systems with 'generalized' operations

(define (apply-generic-f op . args)
   (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
       (if proc
           (apply proc (map contents args))
           (error
            "Нет метода для этих типов -- APPLY-GENERIC"
            (list op type-tags))))))

(define (add x y) (apply-generic-f 'add x y))
(define (sub x y) (apply-generic-f 'sub x y))
(define (mul x y) (apply-generic-f 'mul x y))
(define (div x y) (apply-generic-f 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
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

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

(define g (make-complex-from-real-imag 3 4))
g

;2.77

(define (magnitude1 z) (apply-generic 'magnitude z))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;2.78

(define (attach-tag1 type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons (type-tag contents))))

(define (type-tag1 datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Некорректные помеченные данные -- TYPE-TAG" datum)))

(define (contents1 datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Некорректные помеченные данные -- CONTENTS" datum)))

(contents (cons 'scheme-number 4))
(contents1 4)
(contents1 (cons 'scheme-number 4))

;2.79
(define (equ? x y)
  (apply-generic 'equ? x y))

(define (equ?-s x y)
  (if (= x y)
      #t
      false))
(put 'equ? '(scheme-number scheme-number) equ?-s)

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (equal-rat? x y)
  (cond ((= (* (numer x) (denom y))
            (* (numer y) (denom x))) true)
        (else false)))

(put 'equ? '(rational rational) equal-rat?)

(define (equ?-complex x y)
  (if (and (= (real-part x) (real-part y))
           (= (imag-part x) (imag-part y)))
      true
      false))

(put 'equ? '(complex complex) equ?-complex)

;2.80

(define (=zero? x)
  (apply-generic '=zero? x))

;normal-number
(define (=zero-s x)
  (if (= x 0)
      #t
      false))
(put '=zero? '(scheme-number) =zero-s)

(define (=zero-rat r)
  (if (= (numer r) 0)
      true
      false))
(put '=zero? '(rational) =zero-rat)

(define (=zero-complex c)
  (if (and (= (real-part c) 0)
           (= (imag-part c) 0))
      true
      false))
(put '=zero? '(complex) =zero-complex)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;2.5.2

(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2) coercion-list 
      (set! coercion-list
            (cons (list type1 type2 item)
                  coercion-list))))

(define (get-coercion type1 type2) 
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list) #f
        (let ((top (car list)))
          (if (and (equal? type1 (get-type1 top))
                   (equal? type2 (get-type2 top))) (get-item top)
                   (get-coercion-iter (cdr list) type1 type2)))))
  (get-coercion-iter coercion-list type1 type2))

(define gh (scheme-number->complex (make-scheme-number 7)))
gh

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags)
                             (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method" (list op type-tags))))))
              (error "No method" (list op type-tags)))))))

;2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;2.82

(define (coerce arg type)
  (cond ((eq? (type-tag arg) type) arg)
        ((not (eq? (type-tag arg) type))
         ((get-coercion (type-tag arg) type) arg))
        (else (error " No possible coercion"))))

(define (coerce-all type args)
  (cond ((null? args) '())
        ((eq? (car args) type) (cons (car args) (coerce-all type (cdr args))))
        ((not (eq? (type-tag (car args)) type))
         (cons (coerce (car args) type) (coerce-all type (cdr args))))
        (else (error " No possible coercion"))))

(define (apply-with-coercion op args types)
  (if (null? types)
      (error "No possible method for these types")
      (let ((type (car types)))
        (let ((coerced-args (coerce-all type args)))
          (if coerced-args
              (apply apply-generic (cons op coerced-args))
              (apply-with-coercion op args (cdr types)))))))
     

;2.83

(define (make-real x)
  (attach-tag 'real x))

(define (scheme->rational n)
  (make-rational n 1))

(define (rational->real n)
  (make-real (/ (numer n) (denom n))))

(define (real->complex n)
  (make-complex-from-real-imag n 0))

(define (raise x)
  (apply-generic 'raise x))

(put 'raise '(scheme-number) scheme->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)

(define gg (raise (raise (make-rational 1 7))))
gg

;2.84

(define (higher?-monkey a1 a2)
  (cond ((eq? (type-tag a1) (type-tag (raise a2))) #t)
        ((eq? (type-tag a1) (type-tag a2)) #f)
        ((eq? (type-tag (raise a1)) (type-tag a2)) #f)
        (else (higher?-monkey a1 (raise a2)))))

(define (higher? a1 a2)
  (let ((raised (raise a2)))
    (and raised
         (or (eq? (type-tag a1) (type-tag raised))
             (higher? a1 raised)))))

(define (apply-generic-n op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (if (eq? (type-tag a1) (type-tag a2))
                    (apply-generic-n op a1 a2)
                    (cond ((higher? a1 a2)
                           (apply-generic-n op a1 (raise a2)))
                          ((higher? a2 a1)
                           (apply-generic-n op (raise a1) a2))
                          (else (error "No type")))))
              (error "No type"))))))

;(apply-generic-n 'equ? (make-scheme-number 1) (make-complex-from-real-imag 1 1))

;2.85

(define (project obj)
  (apply-generic-n 'project obj))

;(define int (make-scheme-number 7))
;(define rat (make-rational 6 7))

(define (rational->integer r)
  (let ((number (cdr r)))
    (make-scheme-number (/ (numer number) (denom number)))))

(put 'project 'rational rational->integer)
;(rational->integer rat)

(define (real->rational re)
  (let ((number (cdr re)))
    (make-rational (numerator number) (denominator number)))) 

;(define real (make-real 76.6))
;(real->rational real)
(put 'project 'real real->rational)

(define (complex->real c)
  (let ((number (caddr c)))
    (make-real (real-part number))))

(put 'project 'complex complex->real)
(define complex (make-complex-from-real-imag 5 6))
;(complex->real complex)

(define (drop number)
  (let ((op (get 'project (type-tag number))))
    (if op
        (let ((projected (project number)))
          (if (eq? number (raise projected))
              (drop projected)
              number))
          number)))

(drop complex)            
        

;2.86
