#lang planet neil/sicp
; #lang racket

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; (memo-fib 0)
; (memo-fib 1)
; (memo-fib 2)
; (memo-fib 3)
; (memo-fib 4)
; (memo-fib 5)
; (memo-fib 6)

(define v1 (make-vector 5))

(vector-set! v1 0 1)
(vector-set! v1 1 2)
(vector-set! v1 2 3)
(vector-set! v1 3 4)
(vector-set! v1 4 5)


(define v2 (make-vector 4))

(vector-set! v2 0 4)
(vector-set! v2 1 5)
(vector-set! v2 2 6)
(vector-set! v2 3 7)
; v1
; v2

(define (vector-append vec1 vec2)
  (let ((res (make-vector (+ (vector-length vec1) (vector-length vec2)))))
    (define (loop newvec i)
      (cond ((< i 0) newvec)
            ((< i (vector-length vec1)) 
              ; newvec)
             (begin (vector-set! newvec i (vector-ref vec1 i))
                    (loop newvec (- i 1))))
            (else (begin (vector-set! newvec i (vector-ref vec2 (- i (vector-length vec1))))
                  (loop newvec (- i 1))))))  
  (loop res (- (vector-length res) 1))))


; (vector-append v1 v2)
; (vector-append v2 v1)
























(define (vector-map fn v) 
    (define (loop newvec i) 
        (if (< i 0) 
            newvec 
           (begin (vector-set! newvec i (fn (vector-ref v i))) 
                  (loop newvec (- i 1))))) 
    (loop (make-vector (vector-length v))
          (- (vector-length v) 1)))

; (vector-map odd? v2)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; (filter even? '(1 2 3 4))

(define (vector-filter pred vec)
  (let ((true-index '())
        (l (vector-length vec)))
       (define (loop index)
         (cond ((= index l) true-index)
               ((not (pred (vector-ref vec index)))
                (loop (+ index 1)))
               ((pred (vector-ref vec index)) 
                (begin (set! true-index (cons index true-index))
                       (loop (+ index 1)))))

       )
       (build-vector (reverse (loop 0)) vec)
  )
)

(define (build-vector index-list vec)
  (let ((res (make-vector (length index-list))))
    (define (loop newvec i)
      (if (= i (length index-list))
          newvec
          (begin (vector-set! newvec i (vector-ref vec (list-ref index-list i)))
                 (loop newvec (+ i 1)))))
    (loop res 0))
)

; v1
; (vector-filter odd? v1)


; (vector-filter odd? v3)
; (vector-set! v3 0 10)

(define v3 (make-vector 8))

(vector-set! v3 0 1)
(vector-set! v3 1 8)
(vector-set! v3 2 6)
(vector-set! v3 3 1)
(vector-set! v3 4 6)
(vector-set! v3 5 5)
(vector-set! v3 6 7)
(vector-set! v3 7 9)

v3

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (bubble-sort! vec)
  (define (loop i end)
    (if (= i end)
        vec
        (let ((temp 0))
          (if (> (vector-ref vec i) (vector-ref vec (+ i 1)))
              (begin (set! temp (vector-ref vec i))
                     (vector-set! vec i (vector-ref vec (+ i 1)))
                     (vector-set! vec (+ i 1) temp)
                     (loop (+ i 1) end))
              (loop (+ i 1) end)))))
  (define (start num)
    (if (= 1 num)
        (loop 0 1)
        (begin (loop 0 num)
          (start (- num 1)))))
  (start (- (vector-length vec) 1)))


(bubble-sort! v3)








