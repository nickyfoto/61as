#lang planet neil/sicp

(define (make-accumulator init)
    (lambda (x) (begin (set! init (+ init x))
                       (+ x init)))
)

(define f (make-accumulator -1))


; (f 0)
; (f 1)

(f 1)
(f 0)