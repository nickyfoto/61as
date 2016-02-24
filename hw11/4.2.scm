把application?提前到assignment?以及definition?之前会把任意的exp判断成application。

(define (application? exp) (pair? exp))

如果exp是(define x 3)，它会去调用apply，把define当做用户定义的procedure；x和3被当做
参数。

如果要提前，可以根据标志call来判断，执行相应的过程。


(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))