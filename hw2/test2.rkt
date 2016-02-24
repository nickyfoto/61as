#lang racket

(require berkeley)

(define (t f)
  (lambda (x) (f (f (f x)))) 
)
 
t ;#<procedure:t>
;(t) 需要给参数
(t t) ;其实就是 (lambda (x) (t (t (t x))))
(lambda (x) (t (t (t x)))) ; 就是个没有执行的lambda

((t t) add1) ;就是指我要执行(lambda (x) (t (t (t x))))了，给了一个x的值，给出的是(t (t (t add1)))
(t (t (t add1))) ;还是个procedure。就相当于 ((lambda (x) (t (t (t x)))) add1)
((lambda (x) (t (t (t x)))) add1) ;所以如果要执行它，要给个x赋值。

(((t t) add1) 0) ;27
((t (t (t add1))) 0) ;27
(((lambda (x) (t (t (t x)))) add1) 0) ;27




