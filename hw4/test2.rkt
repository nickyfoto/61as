#lang racket

(require berkeley)

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
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))
  )
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (< (lower-bound x) 0) (< (lower-bound y) 0))
      (error "lower-bound x or y < 0")
      (mul-interval x 
                (make-interval (/ 1 (upper-bound y))
                                 (/ 1 (lower-bound y))))
  )
)

; (sub-interval (make-interval 20 30) (make-interval 10 12))
; (mul-interval (make-interval 20 30) (make-interval 10 12))
; (div-interval (make-interval 20 30) (make-interval 10 12))
; (div-interval (make-interval -5 5) (make-interval 10 20))
(define (make-center-percent c tol)
  (make-interval (* c (- 1 (/ tol 100))) (* c (+ 1 (/ tol 100))))
)
; (make-interval 6 14)
; (make-center-percent 20 5) 

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))



(define res (add-interval (make-center-percent 20 5) (make-interval 6 14)))
; res
; (center res)
(define (percent i) 
  (* (/ (/ (- (upper-bound i) (lower-bound i)) 2) (center i)) 100)
)

; (make-center-percent 20 5) 
; (percent (make-center-percent 20 5))
; res
; (percent res)



(define (last-pair lst)
  (list-ref lst (- (length lst) 1))
)

; (last-pair (list 23 72 149 34))


(define (same-parity . args)
  (if (even? (car args))
      (filter even? args)
      (filter odd? args)
  )
)

; (same-parity 1 2 3 4 5 6 7)

; (define (square-list items)
;   (map square items)
; )
; (square-list (list 1 2 3 4))

; (define (square-list items)
;   (if (null? items)
;       nil
;       (cons (square (car items)) (square-list (cdr items)))
;   )
; )


(define (square-list items)
  (define (iter counter res)
    (if (= counter 0)
        res
        (iter (- counter 1) (cons (square (list-ref items (- counter 1))) res))
    )
  )
  (iter (length items) nil)
)



(square-list (list 1 2 3 4))
(cons 1 (cons 2 (cons 3 (cons 4 '()))))

; (list-ref (list 1 2 3 4) 3)




