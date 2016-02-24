#lang racket

(require berkeley)


(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define zero 
  (lambda (f) 
    (lambda (x) x)
  )
)


(define one
  (lambda (f) 
    (lambda (x) (f x))
  )
)


(define two
  (lambda (f) 
    (lambda (x) (f (f x)))
  )
)

(define (try num)                       ; convert Church to
  ((num (lambda (x) (+ x 1))) 0))       ; ordinary number

(try two)

;http://sicp.readthedocs.org/en/latest/chp2/6.html