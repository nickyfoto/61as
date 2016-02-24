#lang racket
(require berkeley)

;2.30 a

(define (square-tree tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree sub-tree)
              (square sub-tree)
          )
       ) tree)
)


;2.31

(define (tree-map fn tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map fn sub-tree)
              (fn sub-tree)
          )
       )
       tree
  ) 
)


(define (square-tree2 tree) 
  (tree-map square tree)
)

; (square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))


;2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))

(trace subsets)
(subsets '(1 2 3))

; 为什么这个有效？

; 取一个set的所有子集(powerset)的办法：

; 1. 计算除第一个元素之外，这个set的所有子集
; 2. 把第一个元素prepend在上述子集的前面。
; 3. 把这两部分和在一起

; 例如，我有一个set，（1），那么它的所有子集是(nil (1))。
; 如果我的set是(2 1)，那么，除了2的所有子集我们已经知道了， 是(nil (1))。
; 把2加在这上面，就是((2) (2 1))。
; 再与(1)的所有子集合并(nil (1) (2) (2 1))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree)) (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))
  )
)
; (trace sum-odd-squares)
; (sum-odd-squares (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
  )
)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  ; (trace next)
  (next 0))


; (even-fibs 10)




;2.33

;原始map定义

(define (map p sequence)
    (if (null? sequence)
        '()
        (cons (p (car sequence))
              (map p (cdr sequence)))))

;用accumulate定义map

; (define (map p sequence)
;   (accumulate (lambda (x y) <??>) nil sequence))


;原始accumulate定义，
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;依照它把上面的fn展开得到

; (define (map p sequence)
;   (if (null? sequence)
;       nil
;       ((lambda (x y) <??>) (car sequence)
;           (accumulate (lambda (x y) <??>) nil (cdr sequence)))))

;(car sequence)是x
;(accumulate (lambda (x y) <??>) nil (cdr sequence)) 是y

;所以(cons (p x) y)


(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map2 square (list 1 2 3 4))

; (define (append list1 list2)
;     (if (null? list1)
;         list2
;         (cons (car list1)
;               (append (cdr list1) list2))))

; (append (list 1 2) (list 3 4))

; (define (append seq1 seq2)
;   (accumulate cons <??> <??>))

; (define (accumulate op initial sequence)
;   (if (null? sequence)
;       initial
;       (op (car sequence)
;           (accumulate op initial (cdr sequence)))))



; (define (append seq1 seq2)
;   (if (null? sequence)
;       initial <seq2>
;       (cons (car sequence) <seq1>     
;           (accumulate cons initial (cdr sequence)))))
;                             <seq2>        <seq1>



(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))


; (append2 (list 1 2) (list 3 4))





(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))



; (define (length sequence)
;   (if (null? sequence)
;       0
;       (<??> (car sequence) (accumulate <??> 0 (cdr sequence)))))

; ; (lambda (x y) (+ 1 y)
      
; (define (length sequence)
;     (if (null? sequence)
;         0
;         (+ 1 (length (cdr sequence)))))

; (length '(1 2 3 4))
; (length2 '(1 2 3 4))

; (define (accumulate op initial sequence)
;   (if (null? sequence)
;       initial
;       (op (car sequence)
;           (accumulate op initial (cdr sequence)))))























