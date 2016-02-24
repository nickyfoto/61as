#lang racket

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;当把map安装到解释器的基本过程时。如果遇到

(define exp '(map (lambda (x) (* x x))
     (list 1 2 3 4)))

;这样的exp
;eval首先判断它是application，然后
; (mc-apply (mc-eval (operator exp) env)
;       (list-of-values (operands exp) env)))

(define (operator exp) (car exp))
(operator exp) ;的结果是   'map 是一个symbol

;(mc-eval (operator exp) env) 需要在环境中查找'map对应的值
;(lookup-variable-value exp env)) 得到的应该是

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(primitive-procedure? (list 'primitive map)) ;#t
(primitive-implementation (list 'primitive map)) ;#<procedure:map>

;对内置apply的理解见
;http://stackoverflow.com/questions/27488723/what-is-the-difference-between-map-and-apply-in-scheme