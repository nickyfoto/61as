#lang racket

(require berkeley)

; (cons (list 1 2) (list 3 4))

; (pair? (cons (cons 1 (cons 2 '())) (cons 3 (cons 4 '()))))

; (pair? '(1 2))

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 (list 2 (list 3 4))))
(count-leaves '((1 2) 3 4))
(count-leaves (cons (list 1 2) (list 3 4)))
; (list 1 (list 2 (list 3 4)))

(define (make command x)
  (cond ((equal? (last command) 'r) (make (bl command) x))
        ((equal? (last command) 'd) (make (bl command) (cdr x)))
        ((equal? (last command) 'a) (make (bl command) (car x)))
        ((equal? (last command) 'c) x)
  )
)

(define (cxr-function command)
  (lambda (x) (make command x)
  )
)


; ((cxr-function 'cadaddr) '(1 3 (5 7) 9))

; (caar '((7)))

; ((cxr-function 'cadadadadadadr) '(1 (2 (3 (4 (5 (6 7)))))))
; (define x (list 1 2 3))
; (define y (list 4 5 6))


; (append x y) ;'(1 2 3 4 5 6)
; (cons x y)   ;'((1 2 3) 4 5 6)
; (list x y)   ;'((1 2 3) (4 5 6))


; (length (cons x y))
; (length (list x y))

(define x (list (list 1 2) (list 3 4)))

(define (my-reverse lst)
  (if (null? lst)
      null
      (append (my-reverse (cdr lst)) (list (car lst)))
  )
)




; x
; (my-reverse x)
; (trace deep-reverse)
; (deep-reverse x)
; (car (car x))
; (car (cdr (car x)))
; (list (car (cdr (car x))) (car (car x)))
; (list? (car (car x)))

(define (combiner lst element)
  (if (null? lst)
      element
      (list lst element)
  )
)

(define (substitute lst)
  (define (inner-rec e)
    (cond ((null? e) null)
          ((list? e) (combiner (inner-rec (cdr e)) (inner-rec (car e))))
          (else e)
    )
  )
  ; (trace inner-rec)
  (inner-rec lst)
)


; (substitute (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; (list? nil)
; (substitute x)


; (length '(() 4))
; (reverse '(() 4))

; 2.27
(define (deep-reverse L) 
  (if (pair? L) 
      (reverse (map deep-reverse L))
      L
  )
)

; (trace deep-reverse)
; (deep-reverse (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;2.28
(define (fringe L)
  (if (pair? L)
      (se (fringe (car L)) (fringe (cdr L)))
      L
  )
)


(define (sum L)
  (accumulate + 0 L)
)

; x
(fringe (list 1 (list 2 (list 3 4)) 5))
(sum (fringe x))
(sum (fringe (list x x)))

;2.29

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
  ; (trace weight)
  (+ (weight (left-branch mobile)) (weight (right-branch mobile)))
)



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


; (balanced?
;       (make-mobile
;        (make-branch 3 (make-mobile (make-branch 4 6) (make-branch 3 8)))
;        (make-branch 6 7)))

; (balanced? (make-mobile (make-branch 3 6) (make-branch 9 2)))


; (balanced?
;     (make-mobile
;      (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 3 9)))
;      (make-branch 6 7)))

; (total-mobile (make-mobile (make-branch 3 6) (make-branch 9 2)))



; (total-mobile
;       (make-mobile
;        (make-branch 3 (make-mobile (make-branch 4 6) (make-branch 3 8)))
;        (make-branch 6 7)))


; (number? 3)




















