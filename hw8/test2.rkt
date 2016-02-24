#lang racket
(require berkeley)


(define (foo bar)
  (if (< 0 bar)
      13
  )
)

; (foo 1)