#lang racket
(require berkeley)



; for-each

(define (my-for-each proc L)
  (cond ((not (null? L)) 
         (proc (car L))
         (my-for-each proc (cdr L)))))

(define (combiner l1 l2)
    (if (null? l2)
        l1
        (cons (+ (car l1) (car l2))
              (combiner (cdr l1) (cdr l2)))))
; (my-for-each (lambda (x) (newline) (display x)) (list 1 2 3))
(define (my-map proc . items-list)
  (define (combiner l1 l2)
    (if (null? l2)
        l1
        (cons (proc (car l1) (car l2))
              (combiner (cdr l1) (cdr l2)))))
 
  
  (accumulate combiner '() items-list)
)

; (my-map + '(1 2 3) '(4 5 6) '(7 8 9))


; (accumulate combiner (list '(1 2 3) '(4 5 6) '(7 8 9)))


; accumulate
; foldl

; (accumulate append
;             nil
;             (map (lambda (i)
;                    (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 3)))

; (map (lambda (i) 
;        (map (lambda (j) (list i j))
;             (enumerate-interval 1 (- i 1))))
;      (enumerate-interval 1 3))

; (map (lambda (j) (list 1 j)) (enumerate-interval 1 (- 1 1)))
; (map (lambda (j) (list 2 j)) (enumerate-interval 1 (- 2 1)))
; (map (lambda (j) (list 3 j)) (enumerate-interval 1 (- 3 1)))


; (accumulate append '() '(() ((2 1)) ((3 1) (3 2))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

; (prime-sum-pairs 6)


(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations '(1 2 3))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))
(subsets '(1 2 3))
                   