#lang racket

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue))) 

;3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1
      0))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (else 0)))
                    

;3.29
;(define (or-gate2 p1 p2 output)
 ; (define (or-gate-procedure)
  ;  (let ((p3 (make-wire))
   ;       (p4 (make-wire))
    ;      (p5 (make-wire)))
     ; (and-gate (invert p3) (invert p4) p5)
      ;(inverter p5 output)))
      ;(add-action! p1 or-gate-procedure)
      ;(add-action! p2 or-gate-procedure)
      ;'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;; *following uses logical-and -- see ch3support.scm

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)                                                   ; <------------------- without adding probes, changes propagate SILENTLY
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;;; Sample simulation
(define (make-agenda) (list 0))

 (define the-agenda (make-agenda))
 (define inverter-delay 2)
 (define and-gate-delay 3)
 (define or-gate-delay 5)
 
 (define input-1 (make-wire))
 (define input-2 (make-wire))
 (define sum (make-wire))
 (define carry (make-wire))
 
 (probe 'sum sum)
 (probe 'carry carry)
 
 (half-adder input-1 input-2 sum carry)
 (set-signal! input-1 1)
 (propagate)
 
 (set-signal! input-2 1)
 (propagate)

(define (make-time-segment time queue)
  (mcons time queue))

(define (segment-time s) (mcar s))

(define (segment-queue s) (mcdr s))

(define (current-time agenda) (mcar agenda))

(define (set-current-time! agenda time)
  (set-mcar! agenda time))

(define (segments agenda) (mcdr agenda))

(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))

(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (< time (segment-time (mcar segments)))
        (null? segments)))
  (define (make-new-time-segment! time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments))
           time)
        (insert-queue! (segment-queue (mcar segments)) action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr! segments
                         (mcons (make-new-time-segment! time action)
                                (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment! time action)))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
        (agenda))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Adenga is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))