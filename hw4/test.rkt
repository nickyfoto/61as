#lang racket

(require berkeley)



(define one-through-four (list 1 2 3 4))
; (display one-through-four)
; (car one-through-four)
; (cdr one-through-four)

(define (leng items)
  (if (null? items)
      0
      (+ 1 (leng (cdr items)))
  )
)
; (trace leng)
; (leng one-through-four)

(define z (cons (cons 1 2) 4))

; (cons 4 5)
; (cons (cons 2 (cons 4 5)) (cons 6 7))
; (cons 3 (cons (cons 1 4) (cons 5 '())))
; (cons 1 (cons 2 (cons 3 '())))


; (car (cons 4 5))  ;4
; (car (cdr (car (cons (cons (cons 4 5) (cons 6 7)) (cons 1 (cons 2 3)))))) ;6



; (cdr (cdr (cdr (cons 1 (cons 2 (cons 3 '())))))) ;'()

; (cons 5 (cons 6 (cons 7 (cons 8 '()))))


; (foldl (lambda (x y) (se x y)) 1 (list 1 2 3 4))

(define (make-point x y) (cons x y))
(define (x-coord point) (car point))
(define (y-coord point) (cdr point))

(define (make-segment x y)
  (cons x y)
)

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (segment-length segment)
  (- (y-coord (end-segment segment)) (x-coord (start-segment segment)))
)

(define (make-card rank suit)
  (cons rank (first suit)))

(define (rank card)
  (car card))

(define (suit card)
  (cdr card))

(define make-hand list) ;; constructor creates a list of cards

(define first-card car) ;; returns the first card in hand

(define rest-hand cdr) ;; returns the rest of the hand

(define empty-hand? null?) ;; checks if you have no cards in your hand

(define my-hand (make-hand (make-card 1 'heart)
                           (make-card 5 'diamond)
                           (make-card 10 'diamond)
                           (make-card 13 'club)))




my-hand
(first-card my-hand)
(rest-hand my-hand)
