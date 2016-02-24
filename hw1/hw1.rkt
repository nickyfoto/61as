#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  (cond ((member? (first sent) (bf sent)) (dupls-removed (bf sent)))
        ((= (count sent) 1) sent)
        (else (se (first sent) (dupls-removed (bf sent))))
  )
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (cond ((not (member? wd sent)) 0)
        ((empty? sent) 0)
        ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
        (else (count-word (bf sent) wd))
  )
)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here

|#

; Exercise 4 - Define squares

(define (squares sent)
  (define (square x) (* x x))
  
  (if (empty? sent)
    '()
    (se (square (first sent)) (squares (bf sent)))
  )
)

; Exercise 5 - Define switch

(define (switch sent)
  (define (change wd)
    (cond ((or (equal? wd 'I) (equal? wd 'me)) 'you)
          ((equal? wd 'you) 'me)
          (else wd)
    )
  )

  (define (inner_switch sent)
    (cond ((empty? sent) '())         
          (else (se (change (first sent)) (inner_switch (bf sent))))
    )
  )

  (if (equal? (first sent) 'you)
    (se 'I (inner_switch (bf sent)))
    (inner_switch sent)
  )
)

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (define (differences nums)
    (if (= 1 (count nums))
      '()
      (se 
        (- (first (bf nums)) (first nums))
        (differences (bf nums))
      )
    )
  )

  (cond ((empty? (differences sent)) #t)
        ((> 0 (first (differences sent))) #f)
        (else (ordered? (bf (differences sent))))
  )
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (define (words-ends-e wd)
    (equal? 'e (last wd))
  )
  
  (cond ((empty? sent) '())
        ((words-ends-e (first sent)) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))
  )
)

; Exercise 8

#|

Your explanation here

|#
