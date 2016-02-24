#lang racket
(require berkeley)
(define (get-clause exp) (cdr exp))

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))


(let ((x 3)) 
  (let ((y (+ x 2))) 
    (let ((z (+ x y 5))) 
      (* x z))))


(define x '(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z)))

; x

(define (let*? exp) (tagged-list? exp 'let*))

(define (make-let declaration body)
  (if (null? declaration)
      body
      (cons 'let (list (list (car declaration)) (make-let (cdr declaration) body)))
))

(define (let*->nested-lets exp)
  (make-let (car (get-clause exp)) (cadr (get-clause exp)))
)

(let*->nested-lets x)