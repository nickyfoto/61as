#lang racket

(require berkeley)


; racket -tm grader.rkt -- hw0-1-tests.rkt hw0-1.rkt fizzbuzz


(define (describe-time secs)
  (cond ((>= secs 86400) (se 
                          (quotient secs 86400)
                          'days
                          (quotient (remainder secs 86400) 3600)
                          'hours 
                          (quotient (remainder secs 3600) 60)
                          'minutes
                          (remainder secs 60)
                          'seconds
                          ))
        ((> secs 3600) (se 
                          (quotient secs 3600) 
                          'hours 
                          (quotient (remainder secs 3600) 60)
                          'minutes
                          (remainder secs 60)
                          'seconds
                       )
        )
        ((< secs 60) (se secs 'seconds))
        ((> secs 60) (se (/ secs 60.0) 'minutes))
  )
)

; (describe-time 22222)
; (describe-time 550441)

(define (remove-once wd sent)
  (if (or (equal? wd (first sent)) (empty? sent))
    (bf sent)
    (se (first sent) (remove-once wd (bf sent)))
  )
)

; (trace remove-once)
; (remove-once 'morning '(good morning good morning))


(define (differences nums)
  (if (= 1 (count nums))
    '()
    (se 
      (- (first (bf nums)) (first nums))
      (differences (bf nums))
    )
  )
)

; (trace differences)
; (differences '(4 23 9 87 6 12))


(define (location small big)


  (define (re small big)
    (cond ((empty? big) 1)
      ((equal? small (first big)) 1)
      (else (+ 1 (re small (bf big))))
    )
  )

  (if ( > (re small big) (count big))
    #f
    (re small big)
  )
  
)


; (trace location)
; (location 'me '(you never give me your money))
; (location 'i '(you never give me your money))
; (location 'the '(the fork and the spoon))


(define (copies num wd)
  (if (= 0 num)
    '()
    (se wd (copies (- num 1) wd))
  )
)

; (copies 8 'spam)

(define (base-grader x)
  (cond ((equal? 'A x) 4)
        ((equal? 'B x) 3)
        ((equal? 'C x) 2)
        ((equal? 'D x) 1)
        ((equal? 'F x) 0)
  )
)

(define (grade-modifier symbol)
  (cond ((equal? '+ symbol) 0.33)
        ((equal? '- symbol) -0.33)
        ((equal? "" symbol) 0)
  )
)

(define (inner grades)
    (cond ((empty? grades) 0)
          ((= (count(first grades)) 1) (+ (base-grader (first grades))
                                        (inner(bf grades))
                                     )
          )
          ((= (count(first grades)) 2) (+ (base-grader (first (first grades)))
                                        (grade-modifier (last (first grades)))
                                        (inner(bf grades))
                                     )
        )
    )
)


(define (gpa grades)
  (/ (inner grades) (count grades))
)

; (trace gpa)
; (gpa '(A A+ B+ B))

(define (repeat-words sent)
  (cond ((empty? sent) '())
        ((number? (first sent)) (se (first sent) 
                                    (copies (- (first sent) 1) (first (bf sent)))
                                    (repeat-words (bf sent))
                                )
        )
        (else (se (first sent) (repeat-words (bf sent))))
  )
)

; (copies 7 'sumurai)

; (repeat-words '(the 7 samurai))
; (repeat-words '(4 calling birds 3 french hens))

(define (convertSent sent)
  (if (empty? sent)
    '()
    (se (count (first sent)) (convertSent(bf sent)))
  )
)

; (convertSent '(the fool on the hill))
; (convertSent '(you like me too much))



(define (same-shape? sent1 sent2)
  (equal? (convertSent sent1) (convertSent sent2))
)

; (same-shape? '(the fool on the hill) '(you like me too much))
; (same-shape? '(the fool on the hill) '(and your bird can sing))


(define (max-sum-squares a b c) 
  (max (+ (square a) (square b)) 
       (+ (square b) (square c)) 
       (+ (square a) (square c))
  )
)


(define (no_vowel wd)
  (cond ((member? 'a wd) #f)
        ((member? 'e wd) #f)
        ((member? 'i wd) #f)
        ((member? 'o wd) #f)
        ((member? 'u wd) #f)
        (else #t)
  )
)

(define (vowel l)
  (member? l 'aeiou)
)

(define (add_ay wd)
  (word wd 'ay)
)

(define (pigl wd)
  (cond ((no_vowel wd) wd)
        ((vowel (first wd)) (add_ay wd))
        (else (pigl (word (bf wd) (first wd))))
  )
)

(pigl 'open)
(pigl 'hello)
(pigl 'scheme)
; (trace pigl)
; (no_vowel 'my)
(pigl 'my)

























