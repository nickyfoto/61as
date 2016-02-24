#lang racket
(require berkeley)

(define (display-line x)
  (newline)
  (display x))

(define (unroll-stream s n)
  (if (< n 0)
      '()
      (cons (stream-car s)
            (unroll-stream (stream-cdr s) (- n 1)))))

(define (display-limited-stream s n)
  (for-each
    display-line (unroll-stream s n))
)

; 1.2.1节中介绍了猜测平方根的操作。

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (sqrt 9)  ;3.00009155413138

;用stream来表示guesses

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (improve guess x))
                             guesses)))
  guesses)


; (display-limited-stream (sqrt-stream 2) 5)

;在1.3.1节中我们还讲了把过程作为参数进行抽象。

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; (* 8 (pi-sum 1 1000)) ;3.139592655589783

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (cons-stream 1 (add-streams (partial-sums stream) (stream-cdr stream))))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

; (display-limited-stream pi-stream 10)


; (display-limited-stream (pi-summands 1) 9)   
;可以生成
; 1.0
; -1/3
; 1/5
; -1/7
; 1/9
; -1/11
; 1/13
; -1/15
; 1/17
; -1/19
; 这里stream-map - 可以把生成的负数变为正数，所以会形成正负交替
; 再利用partial-sum把前几项的和生成数值返回，得到了对pi的逼近。

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(display-limited-stream (pairs integers integers) 15)



















