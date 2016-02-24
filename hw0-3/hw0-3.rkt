#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
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

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
 (if (or (equal? wd (first sent)) (empty? sent))
    (bf sent)
    (se (first sent) (remove-once wd (bf sent)))
  )
)

; Exercise 3 - Define differences
(define (differences nums)
  (if (= 1 (count nums))
    '()
    (se 
      (- (first (bf nums)) (first nums))
      (differences (bf nums))
    )
  )
)

; Exercise 4 - Define location
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

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
    '()
    (se (first (first sent)) (initials (bf sent)))
  )
)

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= 0 num)
    '()
    (se wd (copies (- num 1) wd))
  )
)

; Exercise 7 - Define gpa
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

; Exercise 8 - Define repeat-words

; test-check require no number appear in final result

(define (repeat-words sent)
   (cond ((empty? sent) '())
          ((number? (first sent)) (se
                                      (copies (- (first sent) 1) (first (bf sent)))
                                      (repeat-words (bf sent))
                                  )
          )
          (else (se (first sent) (repeat-words (bf sent))))
    )
)

(define (convertSent sent)
  (if (empty? sent)
    '()
    (se (count (first sent)) (convertSent(bf sent)))
  )
)

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  (equal? (convertSent sent1) (convertSent sent2))
)
