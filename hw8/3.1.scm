#lang planet neil/sicp
(define (make-accumulator init)
    (lambda (x) (begin (set! init (+ init x))
                       init))
)

(define A (make-accumulator 5))
(A 10)
(A 10)


