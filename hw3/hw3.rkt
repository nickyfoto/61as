#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter-helper b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter-helper (square b) (/ n 2) a))
        ((odd? n) (fast-expt-iter-helper b (- n 1) (* a b)))
  )
)

(define (fast-expt-iter b n)
  (fast-expt-iter-helper b n 1)
)

; Exericse 2 - Define phi

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))))
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

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac N D k)
  
    (define (cf i)
        (if (= k i)
            (/ (N k) (D k))
            (/ (N i)
               (+ (D i) (cf (+ i 1.0))))))
    (cf 1)
)

;; Iterative version
(define (cont-frac-iter n d k)
  ; Your code here
  (error "Not yet implemented")
)

(define (e k)
  (+ (cont-frac (lambda (x) 1.0)
           (lambda (x) (if (= (remainder (+ x 1) 3) 0)
                 (- (+ x 1) (/ (+ x 1) 3))
                 1
             )
           )
           k
  ) 2)
)

; Exercise 4 - Define next-perf
(define (find-factors n)

  (define col '())
  (define (inner-rec n factor)
    (cond ((= factor n) col)
          ((= (remainder n factor) 0) (se col factor (inner-rec n (+ factor 1))))
          (else (inner-rec n (+ factor 1)))
    )
  )
  (inner-rec n 1)
)

(define (sum-of-factors factors)
  (if (empty? factors)
      0
      (+ (first factors) (sum-of-factors (bf factors)))
  )
)

(define (next-perf n)
  (if (and (not (= n 0))
        (= n (sum-of-factors (find-factors n)))
      )
      n
      (next-perf (+ n 1))
  )
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter:

|#
