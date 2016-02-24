#lang planet neil/sicp
; #lang racket


(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (count-pairs2 x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x) (equal? #f (memq x memo-list)))
        (inner (car x) (inner (cdr x) (cons x memo-list)))
        memo-list))



(define l (list 1 2 3))
(count-pairs l)
(count-pairs2 l)

; (cddr l) ;'(3)
; (cdr l)  ;'(2 3)
; (car (cdr l)) 2

(set-car! (cdr l) (cddr l))
(count-pairs l)
(count-pairs2 l)

(set-car! l (cdr l))
(count-pairs l)
(count-pairs2 l)





