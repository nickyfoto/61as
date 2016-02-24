#lang racket
(require berkeley)

(define (make-from-real-imag-mp x y)
  (define (dispatch op)
    (cond ((eq? op 'real-val) x)
          ((eq? op 'imag-val) y)
          ((eq? op 'mag)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'ang) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; ((make-from-real-imag-mp 1 2) 'real-val)

; ((lambda (f n)  ; this lambda is defining MAP 
;     ((lambda (map) (map map f n)) 
;     (lambda (map f n) 
;         (if (null? n) 
;             '() 
;             (cons (f (car n)) (map map f (cdr n))) )) )) ;end of lambda defining MAP 
; first              ; the argument f for MAP
; '(the rain in spain))

(define map
  (lambda (f n)  ; this lambda is defining MAP 
      ((lambda (map) (map map f n)) 
      (lambda (map f n) 
          (if (null? n) 
              '() 
              (cons (f (car n)) (map map f (cdr n))) )) )))

(map first '(the rain in spain))
(map (lambda (x) (first x)) '(the rain in spain))
; (let ((x 1) (y 2)) (+ x y))

; (let ((x 1) (y 2)) (+ x y))

















