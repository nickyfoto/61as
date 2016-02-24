#lang planet neil/sicp

(define (make-monitored fn)
  (define count 0)
  (define (perform x) 
    (fn x))
  (define (dispatch m)
    (cond ((equal? m 'how-many-calls?) count)
          ((equal? m 'reset-count) (set! count 0))
          (else (begin (set! count (+ count 1))
                       (perform m)))
    )
  )
  dispatch
)

(define s (make-monitored sqrt))

(s 100)
(s 200)

(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)