#lang racket

(define x

'(let ((x 1)
      (y 2)
      (z 3))
 (+ x y z))

)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (get-clause exp) (cdr exp))
(define (let-variables exp)
  (if (null? exp)
      '()
      (cons (caar exp) (let-variables (cdr exp)))))

(define (let-body exp) (cdr exp))

(define (let-exp exp)
  (if (null? exp)
      '()
      (cons (cadar exp) (let-exp (cdr exp)))))

(define (let->combination exp)
  (cons
    (make-lambda (let-variables (car (get-clause exp)))
                 (let-body (get-clause exp)))
    (let-exp (car (get-clause exp)))))


(let->combination x)