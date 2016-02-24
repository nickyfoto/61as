#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (if (> (car interval) (cdr interval))
      (car interval)
      (cdr interval)
  )
)

(define (lower-bound interval)
  (if (< (car interval) (cdr interval))
      (car interval)
      (cdr interval)
  )
)

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))
  )
)

; SICP 2.10 - Modify div-interval

; (define (div-interval x y)
;   (mul-interval x 
;                 (make-interval (/ 1 (upper-bound y))
;                                (/ 1 (lower-bound y)))))


(define (div-interval x y)
  (if (and 
        (>= (upper-bound y) 0) 
        (<= (lower-bound y) 0))
    (error "Denominator spans zero")
    (mul-interval 
      x
      (make-interval  (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))))
;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (make-interval (* c (- 1 (/ tol 100))) (* c (+ 1 (/ tol 100))))
)

(define (percent i) 
  (* (/ (/ (- (upper-bound i) (lower-bound i)) 2) (center i)) 100)
)
; SICP 2.17 - Define last-pair

(define (last-pair lst)
  (cons (list-ref lst (- (length lst) 1)) '())
)



; SICP 2.20 - Define same-parity

(define (same-parity . args)
  (if (even? (car args))
      (filter even? args)
      (filter odd? args)
  )
)


; SICP 2.22 - Write your explanation in the comment block:

#|
Your explanation here
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (define (inner-rec e)
    (cond ((null? e) null)
          ((list? e) (cons (inner-rec (car e)) (inner-rec (cdr e))))
          ((equal? old e) new)
          (else e)
    )
  )
  (inner-rec lst)
)

; Exercise 3 - Define my-substitute2

(define (substitute2 lst old new)
  (define (change wd lst1 lst2)
    (cond ((equal? wd (car lst1)) (car lst2))
          (else (change wd (cdr lst1) (cdr lst2)))
    )
  )

  (define (inner-rec e)
    (cond ((null? e) null)
          ((list? e) (cons (inner-rec (car e)) (inner-rec (cdr e))))
          ((member? e old) (change e old new))
          (else (word e))
    )
  )
  (inner-rec lst)
)

; Exercise 4

(define (make command x)
  (cond ((equal? (last command) 'r) (make (bl command) x))
        ((equal? (last command) 'd) (make (bl command) (cdr x)))
        ((equal? (last command) 'a) (make (bl command) (car x)))
        ((equal? (last command) 'c) x)
  )
)

(define (cxr-function command)
  (lambda (x) (make command x)
  )
)

; Exercise 6

(define (my-reverse lst)
  (if (null? lst)
      null
      (append (my-reverse (cdr lst)) (list (car lst)))
  )
)

; ((cxr-function 'caadaar) '(((sub (here there)) sub sub here) (((sub)))))
; (cadaar '(((sub (here there)) sub sub here) (((sub))))) ; 4 maximum
