#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  (cond ((empty? sent) '())
        ((equal? (first sent) old-word) (se new-word (substitute (bf sent) old-word new-word)))
        (else (se (first sent) (substitute (bf sent) old-word new-word)))
  )
)


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: procedure

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns: 10

(define (square x) (* x x)) 
(square 5)
-> returns: 25

(define square (lambda (x) (* x x))) 
(square 5)
-> returns 25

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|

Number of arguments g has: 0

Type of value returned by g: procedure

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5

(define (f1) 3)
(define (f2) 3)
(define (f3 x) x)
(define (f4) word)
(define (f5)
  (lambda () word)
)

; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9

3. (((t t) add1) 0) returns: 27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  (lambda (x) (equal? x wd))
)

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (inc n) (+ n 1))
(define (identity x) x)

(define (product term a next b)
    (if (> a b)
      1
      (* (term a)
         (product term (next a) next b)))
)

(define (factorial x)
  (product identity 1 inc x)
)

(define (inc2 x) (+ x 2))


(define (estimate-pi)
  (* 2001 2.0 (/ (product square 2 inc2 2000) (product square 1 inc2 2001)))
)

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (my-accumulate combiner null-value term (next a) next b)))
)

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  (my-accumulate + 0 term a next b)
)

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  (my-accumulate * 1 term a next b)
)


; SICP 1.33

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

(define (sum-sq-prime a b)
  (filtered-accumulate + 0 square a add1 b prime?)
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (filtered-accumulate * 1 identity 1 add1 n (lambda (x) (rel-prime? x n)))
)

; SICP 1.40 - Define cubic


(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (square x)) (* b x)c))
)

; SICP 1.41 - Define double

(define (double proc)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.43 - Define repeated

(define (my-repeated proc n)
  (if (= n 1)
       proc
       (lambda (x) (proc ((my-repeated proc (- n 1)) x)))
   )
)

; Exercise 9 - Define my-every

(define (my-every proc sent)
  (if (empty? sent)
      '()
      (se (proc (first sent)) (my-every proc (bf sent)))
  )
)

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns: 'ppuurrppllee

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns: '(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns: '(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns: 'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns: ""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns: ""

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns: '(purple)
|#
