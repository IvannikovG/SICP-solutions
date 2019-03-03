#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;Nested maps

;(accumulate append null
 ;           (map (lambda (i)
  ;                 (map (lambda (j) (list i j))
   ;                     (enumerate-interval 1 (- i 1))))
    ;             (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;(define (prime-sum? pair)
 ; (prime? (+ (car prime) (cadr pair))))

;(define (make-pair-sum pair)
 ; (list (car pair) (cadr pair)  (+ (car pair) (cadr pair))))

;(define (prime-sum-pairs n)
 ; (map make-pair-sum
  ;      (filter prime-sum?
   ;             (flatmap
     ;            (lambda (i)
    ;               (map (lambda (j) (list i j))
      ;                  (enumerate-interval 1 (- i 1))))
       ;          (enumerate-interval 1)))))


;2.40

(define (smallest-divisor n)
   (find-divisor n 2))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
   (= (remainder b a) 0))

(define (square x)
   (* x x))

(define (prime? n)
   (= n (smallest-divisor n)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;(prime-sum-pairs 7)

;2.41

(define (triples-generator n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (equal-to-a-sum n s)
  (filter (lambda (x) (= (accumulate + 0 x) s)) (triples-generator n)))

;(triples-generator 4)
;(equal-to-a-sum 15 16)


;2.42 - 2.43 undone

;(define empty-board (list))

;(define (queens board-size)
 ; (define (queen-cols k)
  ;  (if (= k 0)
   ;     (list empty-board)
    ;    (filter
     ;    (lambda (positions) (safe? k positions))
      ;;   (flatmap
        ;  (lambda (rest-of-queens)
         ;   (map (lambda (new-row)
          ;         (adjoin-position new-row k rest-of-queens))
           ;      (enumerate-interval 1 board-size)))
          ;(queen-cols (- k 1))))))
  ;(queen-cols board-size))


;2.2.4 Paitings

;(define wave2 (beside wave (flip-vert wave)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define double (beside (flip-horiz (flip-vert einstein)) einstein))

;(paint (below double double))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                  (bottom-right (below right right))
                  (corner (corner-split painter (- n 1))))
            (beside (below painter top-left)
                    (below bottom-right corner))))))

                   
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;(paint (square-limit einstein 2))

;(paint (up-split einstein 3))

;(paint (corner-split einstein 2))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs1 painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))

;(paint (flipped-pairs1 einstein))

(define (square-limit1 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;(paint (square-limit1 einstein 2))

;2.45

(define (split dir1 dir2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split dir1 dir2) painter (- n 1))))
          (dir1 painter (dir2 smaller smaller))))))

(define right-split1 (split beside below))
;(paint (right-split1 einstein 2))


;Borders - Ramki.

;2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define cc (make-vect 1 2))
;(define yy (make-vect 2 3))

;2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;(define (make-frame1 origin edge1 edge2)
 ; (cons origin (cons edge1 edge2)))

;(define (find-edge2 frame)
 ; (cddr frame))

(define vv (make-frame (cons 1 1) (cons 2 2) (cons 3 3)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

((frame-coord-map vv) (make-vect 1 1))

;2.48
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;(define (segments->painter segment-list)
 ; (lambda (frame)
  ;  (for-each
   ;  (lambda (segment)
    ;   (draw-line
      ;  ((frame-coord-map frame) (start-segment segment))
       ; ((frame-coord-map frame) (end-segment segment))))
     ;segment-list)))

(define contour-square 
         (list
                        (make-segment (make-vect 0 0)
                                      (make-vect 0 1))
                        (make-segment (make-vect 0 1)
                                      (make-vect 1 1))
                        (make-segment (make-vect 1 1)
                                      (make-vect 0 1))
                        (make-segment (make-vect 0 1)
                                      (make-vect 0 0))))

;(define outline (segments->painter contour-square))
;(paint outline)

;(define (transform-painter painter origin corner1 corner2)
 ; (lambda (frame)
  ;  (let ((m (frame-coord-map frame)))
   ;   (let ((new-origin (m origin)))
    ;    (painter
     ;    (make-frame new-origin
      ;               (sub-vect (m corner1) new-origin)
       ;              (sub-vect (m corner2) new-origin)))))))

(define (flip-vert1 painter)
  ((transform-painter 
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0))
  painter))

(paint (flip-vert1 einstein))

;2.50

(define (flip-horiz1 painter)
  ((transform-painter 
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0))
  painter))

(define (shrink painter)
  ((transform-painter
    (make-vect 0.25 0.25)
    (make-vect 0.5 0.25)
    (make-vect 0.25 0.5))
   painter))

(define (rotate-90 painter)
  ((transform-painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0))
   painter))

(define (rotate-270 painter)
   ((transform-painter (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))

(paint (flip-horiz1 einstein))
(paint (shrink einstein))

;(define (beside1 painter1 painter2)
 ; (let ((split-point (make-vect 0.5 0.0)))
  ;  (let ((paint-left
   ;        (transform-painter painter1
    ;                          (make-vect 0.0 0.0)
     ;                         split-point
      ;                        (make-vect 0.0 1.0)))
      ;(paint-right
       ;(transform-painter painter2
        ;                  split-point
         ;                 (make-vect 1.0 0.0)
          ;                (make-vect 0.5 1.0))))
      ;(lambda (frame)
       ; (paint-left frame)
        ;(paint-right frame)))))

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)
            painter1))
           (paint-up
            ((transform-painter split-point
                                (make-vect 1.0 0.5)
                                (make-vect 0.0 1.0))
             painter2)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-up frame)))))

(paint (below1 einstein einstein))

(define (below2 painter1 painter2)
  (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))

(paint (below2 einstein einstein))

;2.52a

