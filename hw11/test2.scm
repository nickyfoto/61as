#lang racket


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'cadr cadr)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'list list)
        (list 'append append)
        (list 'equal? equal?)
        (list 'integer? integer?)
        (list 'number? number?)
        (list 'list? list?)
        (list 'pair? pair?)
        (list 'not not)
        (list 'list-ref list-ref)
        (list 'assoc assoc)
;;      more primitives
  ))

; primitive-procedures

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


; (primitive-procedure-objects)
(map (lambda (x) (+ x 1)) '(1 2 3))

(car (cdr '(map (lambda (x) (+ x 1)) '(1 2 3))))



(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))