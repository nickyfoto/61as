; 原来的参数求值表不能确定用cons构建pair时哪个先求值。
; 通过let引入局部变量之后，过程先对变量求值，从而控制求值顺序。

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let  ((left (mc-eval (first-operand exps) env))
            (right (list-of-values (rest-operands exps) env)))
      (cons left right))))