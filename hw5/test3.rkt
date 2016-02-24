#lang racket

(require berkeley)

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                        (+ this-coeff (* x higher-terms))
              )
              0
              coefficient-sequence))

; (horner-eval 2 (list 1 3 0 5 0 1))


;我们可以把它想成

;2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; (define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; (trace accumulate-n)
; (accumulate-n + 0 s)


;2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; (dot-product '(1 2 3 4) '(1 2 3 4))

; '((1 2 3 4) (4 5 6 6) (6 7 8 9))
; '(1 2 3 4)
(define (matrix-*-vector m v)
    (map (lambda (col)
             (dot-product col v))
         m))


; (matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '(1 2 3 4))


(define (transpose mat)
  (accumulate-n cons nil mat))

; (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
      (map (lambda (y) (dot-product x y)) cols))
    m)))


(define s '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
; s
; (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
; (matrix-*-matrix s (transpose s))



;2.38



(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

; (fold-left / 1 (list 1 2 3 4))
; (foldl / 1 (list 1 2 3 4))

; (accumulate / 1 (list 1 2 3 4))
; (foldr / 1 (list 1 2 3 4))
; (foldr / 1 (list 1 2 3 4))

; (foldr list nil (list 1 2 3)) ; '(3 2 1)   
; (foldl list nil (list 1 2 3)) ; '(1 2 3)


; (define (accumulate op initial sequence)
;   (if (null? sequence)
;       initial
;       (op (car sequence)
;           (accumulate op initial (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (reverse2 sequence)
  (accumulate (lambda (x y)  (append y (list x))) nil sequence))
  ; (accumulate (lambda (x y)  (list x)) nil sequence))

(define (reverse3 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(trace accumulate)
(reverse2 (list 1 2 3 4))
; (cons 4 (cons 3 (cons 2 (cons 1 nil))))

; (cons 1 '())
; (cons nil 1)
