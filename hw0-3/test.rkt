#lang racket


; 0.3 - Recursion and Racket: Test Your Understanding
(require berkeley)

(define (sum-sent sent)
  (if (empty? sent)
      0
      (+ (first sent) (sum-sent (bf sent)))
  )
)

; (trace sum-sent)
; (sum-sent '(1 2 3 4 5))



(define (count-ums sent)
  (cond ((empty? sent) 0)
        ((equal? 'um (first sent)) 
          (+ 1 (count-ums (bf sent)))
        )
        (else (count-ums (bf sent)))
  )
)


; (trace count-ums)
; (count-ums '(today um we are going to um talk about the um combining method))


(define (countdown num)
  (if (= 0 num)
    'blastoff!
    (se num (countdown (- num 1)))
  )
)
; (trace countdown)
; (countdown 10)

(define (initials sent)
  (if (empty? sent)
    '()
    (se (first (first sent)) (initials (bf sent)))
  )
)

; (initials '(if i needed someone))


(define (numbers sent)
  (cond ((empty? sent) '())
        ((number? (first sent))
         (se (first sent) (numbers (bf sent)))
        )
        (else (numbers (bf sent)))
  )
)

(numbers '(76 trombones and 110 cornets))