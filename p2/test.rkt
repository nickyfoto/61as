#lang racket

(require berkeley)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (split major minor)
  (lambda (painter n)
    (let ((smaller ((split major minor) painter (- n 1))))
      (major painter (minor smaller smaller)))
  )
)

(define right-split (split beside below))
(define up-split (split below beside))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller))))
)





