#lang planet neil/sicp

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-actions clause) (cdr clause))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses)))
              ((extended-cond-syntax? first)
               (make-if
                 (extended-cond-test first)
                 (list 
                   (extended-cond-recipient first)
                   (extended-cond-test first))
                 (expand-clauses rest)))
              (else 
                   (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest)))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (extended-cond-syntax? clause)
  (and
    (list? clause)
    (> (length clause) 2)
    (eq? (cadr clause) '=>)))

(define (extended-cond-test clause)
  (car clause))

(define (extended-cond-recipient clause)
  (caddr clause))



(define x '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false)))
; (cond->if x)
(if (assoc (quote b) (quote ((a 1) (b 2)))) (cadr (assoc (quote b) (quote ((a 1) (b 2))))) false)



