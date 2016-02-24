#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.
; (append x y) ;'(1 2 3 4 5 6)
; (cons x y)   ;'((1 2 3) 4 5 6)
; (list x y)   ;'((1 2 3) (4 5 6))

; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (list-ref mobile 0)
)

(define (right-branch mobile)
  (list-ref mobile 1))

(define (branch-length branch)
  (list-ref branch 0))

(define (branch-structure branch)
  (list-ref branch 1))

; b. Define total-weight.


(define (total-weight mobile)

  (define (weight branch)
    (if (number? (branch-structure branch))
        (+ (branch-structure branch))
        (total-weight (branch-structure branch))
    )
  )
  (trace weight)
  (+ (weight (left-branch mobile)) (weight (right-branch mobile)))
)

; c. Define balanced?

(define (total branch)
  (if (number? (branch-structure branch))
      (* (branch-length branch) (branch-structure branch))
      (if (balanced? (branch-structure branch))
          (* (branch-length branch) (total-weight (branch-structure branch)))
          #f
      )
      
  )
)

(define (balanced? mobile)
  (equal? (total (left-branch mobile)) (total (right-branch mobile)))
)

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))


;Exercise 3a - Define square-tree

(define (square-tree tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree sub-tree)
              (square sub-tree)
          )
       ) 
  tree)
)

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
    (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map fn sub-tree)
              (fn sub-tree)
          )
       )
       tree
  ) 
)

;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (col)
             (dot-product col v))
         m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
      (map (lambda (y) (dot-product x y)) cols))
    m)))


;Exercise 6 - Give the property that op should satisfy:

#|

Your property here

|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (cond ((and (symbol? l1) (symbol? l2)) (eq? l1 l2))
        ((and (pair? l1) (pair? l2)) 
           (if (my-equal? (car l1) (car l2))
               (my-equal? (cdr l1) (cdr l2))
               #f
           )
        )
        ((and (empty? l1) (empty? l2)) #t)
        (else #f)

  )
)

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
  ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
  (else exp)))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
  ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
         ((= (length args) 1) (- (car args)))
         (else (- (car args) (foldr + 0 (cdr args))))))
  ((eq? fn '*) (foldr * 1 args))
  ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
         ((= (length args) 1) (/ (car args)))
         (else (/ (car args) (foldr * 1 (cdr args))))))
  ((eq? fn 'first) (first (car args)))
  ; ((eq? fn 'first) args)
  ((eq? fn 'butfirst) (bf (car args)))
  ((eq? fn 'bf) (bf (car args)))
  ((eq? fn 'word) (foldr word "" args))
  ((eq? fn 'bl) (bl (car args)))
  ((eq? fn 'butlast) (bl (car args)))
  ((eq? fn 'last) (last (car args)))

  (else (error "Calc: bad operator:" fn))))
