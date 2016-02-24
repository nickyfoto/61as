#lang racket

(require berkeley)



(define (my-substitute lst old new)

  (define (inner-rec e)
    (cond ((null? e) null)
          ; ((empty? e) '())
          ((list? e) (cons (inner-rec (car e)) (inner-rec (cdr e))))
          ((equal? old e) (word new))
          (else (word e))
          
    )
  )
  ; (trace inner-rec)
  (inner-rec lst)
  
)


; (my-substitute '((lead guitar) (bass guitar) (rhythm guitar) drums)
;                   'guitar
;                   'axe)

; (my-substitute '(((sub here) sub sub here) (((sub)))) 'sub 'dub)


(define (my-substitute2 lst old new)

  (define (change wd lst1 lst2)
    (cond ((equal? wd (car lst1)) (car lst2))
          (else (change wd (cdr lst1) (cdr lst2)))
    )
  )

  (define (inner-rec e)
    (cond ((null? e) null)
          ; ((empty? e) '())
          ((list? e) (cons (inner-rec (car e)) (inner-rec (cdr e))))
          ((member? e old) (change e old new))
          (else (word e))
          
    )
  )
  ; (trace inner-rec)
  (inner-rec lst)
  
)



; (my-substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
;                    '(1 2 3 4)
;                    '(one two three four))

; (substitute '(((sub here) sub sub here) (((sub)))) 'sub 'dub)
; (cadaar '(((sub (here there)) sub sub here) (((sub)))))




; (foldl cons '() '(1 2 3 4))
; (cons 4 (cons 3 (cons 2 (cons 1 '()))))

; (define combiner (lambda (x y) (cons (add1 x) y)))

; (combiner 4 (combiner 3 (combiner 2 (combiner 1 '()))))
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

; ((cxr-function 'caadaar) '(((sub (here there)) sub sub here) (((sub)))))
; (cadaar '(((sub (here there)) sub sub here) (((sub)))))


; (reverse '(1 2 3))
(define (my-reverse lst)
  (if (null? lst)
      null
      (append (my-reverse (cdr lst)) (list (car lst)))
  )
)

(my-reverse '(1 2 3 4 abc))

