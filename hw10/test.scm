; #lang planet neil/sicp
#lang racket
(require berkeley)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))




(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (unroll-stream s n)
  (if (< n 0)
      '()
      (cons (stream-car s)
            (unroll-stream (stream-cdr s) (- n 1)))))

(define (display-limited-stream s n)
  (for-each
    display-line (unroll-stream s n))
)


(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define s (stream-enumerate-interval 10000 1000000))
; s
; (stream-filter prime? s)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; (define integers (integers-starting-from 1))


(define integers (cons-stream 1 integers))
(define x (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
x
stream-map
; (stream-map square x)
; (display-limited-stream integers 10)