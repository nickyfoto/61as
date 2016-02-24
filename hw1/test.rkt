#lang racket

(require berkeley)




(define (ends-e sent)

  (define (words-ends-e wd)
    (equal? 'e (last wd))
  )
  
  (cond ((empty? sent) '())
        ((words-ends-e (first sent)) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))
  )
)

; (trace ends-e)
(ends-e '(please put the salami above the blue elephant))