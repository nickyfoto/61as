#lang racket
(require berkeley)


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))


; (trace accumulate)
; (accumulate + 0 (list 1 2 3 4 5))
; 15
; (accumulate * 1 (list 1 2 3 4 5))
; 120
; (accumulate cons nil (list 1 2 3 4 5))
; (1 2 3 4 5)

;accumulate op 后面一定是跟两个parameter，一个是(car sequence), 
;一个是 initial 和 (cdr sequence）递归回来的结果。

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))))
)



(define (count-leaves2 tree)
    (accumulate +
                0
                (map (lambda (sub-tree)
                         (if (pair? sub-tree)           ; 如果这个节点有分支
                             (count-leaves sub-tree)    ; 那么这个节点调用 count-leaves 的结果就是这个节点的树叶数量
                             1))                        ; 遇上一个叶子节点就返回 1
                     tree)))

; (count-leaves (list 1 (list 2 (list 3 4))))
; ; (trace count-leaves2)
; (count-leaves2 '((1 2) 3 4))
; (count-leaves (cons (list 1 2) (list 3 4)))


; (accumulate + 0 (list 1 2 3 4))
(foldr cons '() (list 1 2 3 4))

(foldl cons '() '(1 2 3 4))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-left cons '() '(1 2 3 4))

(foldr / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))








































