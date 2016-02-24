#lang planet neil/sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (get-exps exp) (cdr exp))


(define (eval-and exp env)
  (if (null? exp)
      true
      (if (car exp)
          (eval-and (cdr exp) env)
          false)))

(define (eval-or exp env)
  (if (null? exp)
      false
      (if (car exp)
          true
          (eval-or (cdr exp) env))))

; ((and? exp) (eval-and exp env))
; ((or? exp) (eval-or exp env))


(define (and-expand exps)
  (if (null? exps)
      false
      (if (car exps)
          (and-expand (cdr exps))
          false)))

(define (or-expand exps)
  (if (null? exps)
      false
      (if (car exps)
          true
          (or-expand (cdr exps)))))

(define (and->if exp)
  (and-expand (get-exps exp)))
(define (or->if exp)
  (or-expand (get-exps exp)))



((and? exp) (eval (and->if exp) env))
((or? exp) (eval (or->if exp) env))



