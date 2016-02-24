#lang racket

(require berkeley)


(define (identity x) x)

; (define (product term a next b)
;   (if (> a b)
;       1
;       (* (term a)
;          (product term (next a) next b))))


; (product identity 1 inc 3)

(define (factorial x)
  (product identity 1 inc x)
)

; (factorial 4)

; (define (sum term a next b)
;   (if (> a b)
;     0
;     (+ (term a) (sum term (next a) next b))
;   )
; )

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ 2 x)))
  )

  (define (pi-next x)
    (+ x 4)
  )
  (sum pi-term a pi-next b)
)

(define (inc2 x) (+ x 2))


(define (estimate-pi)
  (* 2001 2.0 (/ (product square 2 inc2 2000) (product square 1 inc2 2001)))
)

; (estimate-pi)

(define (my-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (my-accumulate combiner null-value term (next a) next b))))


(define (sum term a next b)
    (my-accumulate + 0 term a next b)
)

(define (product term a next b)
    (my-accumulate * 1 term a next b))

; (sum identity 1 add1 4)
; (product identity 1 add1 4)
; (my-accumulate + 0 identity 1 add1 4)


(define (filtered-accumulate combiner null-value term a next b pred)
   (if (> a b)
      null-value
      (let ((rest-terms (filtered-accumulate combiner null-value term (next a)
                                             next b pred)))
        (if (pred a)
            (combiner (term a) rest-terms)
            rest-terms
        )
      )
   ) 
)

(define (filtered-sum a b)
    (filtered-accumulate + 0 identity a add1 b prime?)
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (filtered-accumulate * 1 identity 1 add1 n (lambda (x) (rel-prime? x n)))
)

; (filtered-sum 1 10)
; (prod-of-some-numbers 10)


(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (square x)) (* b x)c))
)

(define (double fn)
  (lambda (x) (fn (fn x)))
)

(define (compose fn1 fn2)
  (lambda (x) (fn1 (fn2 x)))
)

; (inc 1)
; ((double inc) 1)
; (((double (double double)) inc) 5)
; ((compose square inc) 6)



(define (my-repeated fn n)
   (if (= n 1)
       fn
       (lambda (x) (fn ((my-repeated fn (- n 1)) x)))
   )
)

(define (my-repeated2 f n)
    (if (= n 1)
        f
        (compose f (my-repeated2 f (- n 1))))
)

; (trace my-repeated)
; ((my-repeated square 2) 5)


(define (my-every proc sent)
  (if (empty? sent)
      '()
      (se (proc (first sent)) (my-every proc (bf sent)))
  )
)


(my-every square '(1 2 3 4))
(my-every first '(nowhere man))

