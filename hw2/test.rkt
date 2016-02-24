#lang racket

(require berkeley)

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubs a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (pi-sum2 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; (sum-cubs 1 10)
; (sum-integers 1 10)
; (* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; ((lambda (x y z) (+ x y (square z))) 1 2 3)


(define (f x y)

    (define (f-helper a b)
        (+ (* x (square a))
           (* y b)
           (* a b))
    )

    (f-helper (+ 1 (* x y))
              (- 1 y))
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
       (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))

(fixed-point cos 3.0)

(cos 0.7390822985224023)
