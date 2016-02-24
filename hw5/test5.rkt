#lang racket
(require berkeley)

(define (scale-tree x factor)
  (cond ((pair? x) (cons (scale-tree (car x) factor) (scale-tree (cdr x) factor)))
        ((null? x) nil)
        (else (* x factor))

  )
)

; (trace scale-tree)
; (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)


(define (deep-reverse L)
  (if (pair? L)
      (reverse (map deep-reverse L))
      L
  )
)

(define x (list (list 1 2) (list 3 4)))
; (deep-reverse x)

(define (breadth-first-search tree)
    (bfs-iter (list tree)))

(define (bfs-iter queue)
    (if (null? queue)
        'done
        (let ((task (car queue)))
            (print (datum task))
            (bfs-iter (append (cdr queue) (children task))))))

; (for-each (lambda (arg)
;               (printf "Got ~a\n" arg)
;               23)
;             '(1 2 3 4))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; (map    (lambda (x) (cons x 1))
;         (enumerate-interval 1 4))


; (map (lambda (y) 
;        (map (lambda (x) (cons x y)) 
;             (enumerate-interval 1 4)))
;      (enumerate-interval 1 4))

; (define (flatmap proc seq)
;   (accumulate append nil (map proc seq)))

; (flatmap (lambda (y)
;              (map (lambda (x) (cons x y))
;                   (enumerate-interval 1 4)))
;          (enumerate-interval 1 4))

; (accumulate append
;             nil
;             (map (lambda (y)
;                     (map (lambda (x) (cons x y))
;                 (enumerate-interval 1 4)))
;             (enumerate-interval 1 4)))



; 



; (memq 'chicken '(cow chicken cow (and chicken)))








; (list 'a 'b 'c) ;'(a b c)
; (list (list 'george)) ;'((george))
; (cdr '((x1 x2) (y1 y2))) ;'((y1 y2))
; (cadr '((x1 x2) (y1 y2))) ;'(y1 y2)
; (pair? (car '(a short list))) ;#f
; (memq 'red '((red shoes) (blue socks))) ;#f
; (memq 'red '(red shoes blue socks)) ;'(red shoes blue socks)







(define (my-equal? l1 l2)
  (cond ((and (symbol? l1) (symbol? l2)) (eq? l1 l2))
        ((and (pair? l1) (pair? l2)) 
           (if (my-equal? (car l1) (car l2))
               (my-equal? (cdr l1) (cdr l2))
               #f
           )
        )
        ((and (empty? l1) (empty? l2)) #t)
        (else #f)

  )
)


; (trace my-equal?)
; (my-equal? '(foo) '(foo))
; (eq? '() '())
; (pair? '(foo))
; (eq? '(this is a list) '(this is a list))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; (subsets '(1 2 3))


(define (calc-eval exp)
  (cond ((number? exp) exp)
  ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
  (else exp)))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
  ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
         ((= (length args) 1) (- (car args)))
         (else (- (car args) (foldr + 0 (cdr args))))))
  ((eq? fn '*) (foldr * 1 args))
  ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
         ((= (length args) 1) (/ (car args)))
         (else (/ (car args) (foldr * 1 (cdr args))))))
  ((eq? fn 'first) (first (car args)))
  ; ((eq? fn 'first) args)
  ((eq? fn 'butfirst) (bf (car args)))
  ((eq? fn 'bf) (bf (car args)))
  ((eq? fn 'word) (foldr word "" args))
  ((eq? fn 'bl) (bl (car args)))
  ((eq? fn 'butlast) (bl (car args)))
  ((eq? fn 'last) (last (car args)))

  (else (error "Calc: bad operator:" fn))))


; (calc-eval 'foo)
; (calc-eval '(first foo))
; (calc-eval '(first (butfirst hello)))
; (calc-eval '(word (bl penny) ies))
; (calc-eval '(last (butlast flower)))
; (calc-eval '(+ 2 (word (first 987) (bf 56))))
(calc-eval '(word (first butter) (* 2 (last 52))))

(define pairup (regroup '((1 2) (3 4) ...)))
; (bf 'hello)