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

; (define (sqrt x)
;   (sqrt-iter 1.0 x))

; (sqrt 9)  ;3.00009155413138

;用stream来表示guesses

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (improve guess x))
                             guesses)))
  guesses)


; (display-limited-stream (sqrt-stream 20) 10)



(define (stream-limit s tolerance)
  (if (> tolerance (abs (- (stream-car s) (stream-car (stream-cdr s)))))
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)
  )
)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
)

(sqrt 99 0.01)













