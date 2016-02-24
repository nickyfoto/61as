#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom

;2. Compound Expression (3 Atoms)

;3. Compound Expression (4 Atoms)

;4. Compound Expression (1 Atom and 2 subexpressions)

;5. Any Other Kind Expression


;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (second wd))
  ; (error "Not yet implemented")
)

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y))
  ; (error "Not yet implemeted")
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word
    (first (first sent))
    (first (first (bf sent)))
  )
  ; (error "Not yet implemented")
)

;Exercise 2 - Define teen?
(define (teen? num)
  (and (>= num 13) (<= num 19))
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  (if (member? (first wd) 'aeiou)
    (se 'an wd)
    (se 'a wd)
  )
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (se (bl sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  (se (first (bf sent)) (first sent) (bl (bf (bf sent))) (word (last sent) '?)) 
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (cond ((and (equal? 'am (last time)) (= 12 (first time))) 0)
        ((equal? 'am (last time)) (first time))
        ((and (equal? 'pm (last time)) (= 12 (first time))) 12)
        ((equal? 'pm (last time)) (+ 12 (first time)))  
  )
)

(define (american-time time)
  (cond ((= time 12) (se 12 '(pm)))
        ((> time 12) (se (- time 12) '(pm)))
        ((= time 0) (se 12 '(am)))
        ((< time 12) (se time '(am)))
  )
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond ((>= secs 86400) (se (/ secs 86400) 'days))
        ((> secs 3600) (se (/ secs 3600.0) 'hours))
        ((< secs 60) (se secs 'seconds))
        ((> secs 60) (se (/ secs 60.0) 'minutes))
  )
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective word)
  (se (word adjective 'est) word))

#|

Explanation here.

|#