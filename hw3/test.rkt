#lang racket

(require berkeley)

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
  )
)

; 要计算6的阶乘，先计算 1 * 2，在把它的结果 * 3，在把它的结果 * 4，一直到n。
; 也就是说，我们保持一个一直在运算的积，以及一个counter，从1到n。
; 每一步这个counter和积都在变。

; (fact-iter product counter max-counter)

; (fact-iter   1 1 6)
; (fact-iter   1 2 6)
; (fact-iter   2 3 6)
; (fact-iter   6 4 6)
; (fact-iter  24 5 6)
; (fact-iter 120 6 6)
; (fact-iter 720 7 6)

(define (fact-iter product counter max-counter)
  (if (> counter max-counter)
      product
      (fact-iter (* counter product) 
                 (+ counter 1)
                 max-counter
      )
  )
)

; (define (factorial-iter n)
;   (fact-iter 1 1 n)
; )



; (factorial-iter 4)

(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))
    )
  )
  (iter 1 1)
)

; (factorial-iter 4)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
  )
)



; (fib-iter a b count)

; (fib-iter  1  0 7)  fib(1) fib(0) 
; (fib-iter  1  1 6)  fib(2) fib(1)
; (fib-iter  2  1 5)  fib(3) fib(2)
; (fib-iter  3  2 4)  fib(4) fib(3)
; (fib-iter  5  3 3)  fib(5) fib(6)
; (fib-iter  8  5 2)  fib(6) fib(5)
; (fib-iter 13  8 1)  fib(7) fib(6)
; (fib-iter 21 13 0)  fib(8) fib(7)



; (define (fib-iter-helper a b count)
;   (if (= count 0)
;       b
;       (fib-iter-helper (+ a b) a (- count 1))
;   )
; )

; (define (fib-iter n)
;   (fib-iter-helper 1 0 n)
; )

(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))
    )
  )
  (iter 1 0 n)
)


; (fib 7)
; (fib-iter 7)


; 也就是说递归和迭代可以解决同样的问题。

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond 
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))



(trace cc)
(count-change 100)

(define (own-expt b n)
  (if (= n 0)
      1
      (* b (own-expt b (- n 1)))))

(define (expt-iter b n)
  (expt-iter-helper b n 1))

(define (expt-iter-helper b counter product)
  (if (= counter 0)
      product
      (expt-iter-helper b
                (- counter 1)
                (* b product))))

; (own-expt 222 100000)
; (expt-iter 222 100000)
; (expt 222 1000000)

(define (count-stairs n)
  (cond ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (count-stairs (- n 1))
                 (count-stairs (- n 2)))
        )
  )
)

; (count-stairs 5)


(define (fast-expt-iter b n)
  (fast-expt-iter-helper b n 1)
)

(define (fast-expt-iter-helper b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter-helper (square b) (/ n 2) a))
        ((odd? n) (fast-expt-iter-helper b (- n 1) (* a b)))
  )
)

; (fast-expt-iter 2 6)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next)
      )
    )
  )
  (try first-guess)
)

; (fixed-point cos 1.0)
; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(define (phi)
  ; (lambda (x) (+ x 1))
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
)
; (phi)
; ((phi) 1.0)

; (define (cont-frac n d k)
;   (if (= k 1)
;       1
;       (/ (n 1.0) (+ (d 1.0) (cont-frac n d (- k 1))))
;   )
; )

(define (cont-frac N D k)

    (define (cf i)
        (if (= k i)
            (/ (N k) (D k))
            (/ (N i)
               (+ (D i) (cf (+ i 1.0))))))

    (cf 1))

; (cont-frac (lambda (x) x) (lambda (x) x) 1)
; (cont-frac (lambda (x) x) (lambda (x) x) 10)

; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            10
; )

; (/ 1 1.6180327868852458)

(define (find-k k)
  (if (> 0.0001 (abs (- (cont-frac (lambda (i) 1.0) 
                                (lambda (i) 1.0) k) 
                     (/ 1 1.6180327868852458)))
                 )
      k
      (find-k (+ k 1))
  )
)

; (find-k 1)

; ((lambda (x) (if (= (remainder (+ x 1) 3) 0)
;                  (- (+ x 1) (/ (+ x 1) 3))
;                  1
;              )

; ))

; (cont-frac (lambda (x) 1.0)
;            (lambda (x) (if (= (remainder (+ x 1) 3) 0)
;                  (- (+ x 1) (/ (+ x 1) 3))
;                  1
;              )
;            )
;            k
; )



; (sum-of-factors '(1 2 4 7 14))

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

; (find-factors 1)



(define (sum-of-factors factors)
  (if (empty? factors)
      0
      (+ (first factors) (sum-of-factors (bf factors)))
  )
)

; (empty? '(0))

; (sum-of-factors '(0))
; (sum-of-factors (find-factors 28))

; (find-factors 28)

; (remainder 0 1)
(define (next-perf n)
  (if (and (not (= n 0))
        (= n (sum-of-factors (find-factors n)))
      )
      n
      (next-perf (+ n 1))
  )
)

; (next-perf 0)