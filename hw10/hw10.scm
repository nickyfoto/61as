;;Lesson 10

;;Exercise 1
;;SICP 3.5.1
#| Explain



|#

;;Exercise 2
;;SICP 3.5.1
#| Explain
(cdr (cons-stream 1 '(2 3))) is a #<promise>, so 
(stream-cdr (cons-stream 1 '(2 3))) could work

but 
(cdr (stream-cdr (cons-stream 1 '(2 3)))) is '(3), is not a promise,
so we couldn't use stream-cdr on it

|#

;;Exercise 3
#| Explain
(define (enumerate-interval low high) 
  (if (> low high) 
      '() 
      (cons low (enumerate-interval (+ low 1) high)) ) )

(delay (enumerate-interval 1 3)) ;#<promise>

(define (stream-enumerate-interval low high) 
  (if (> low high) 
      the-empty-stream 
      (cons-stream low (stream-enumerate-interval (+ low 1) high)) ) )

(stream-enumerate-interval 1 3) ;(mcons 1 #<promise>)

|#

;;Exercise 4
;a.

(define (num-seq n)
  (cons-stream n
               (num-seq (if (even? n)
                            (/ n 2)
                            (+ 1 (* n 3))))))

;b.
(define (seq-length s)
  (define (inner s n)
    (if (= (stream-car s) 1)
        n
        (inner (stream-cdr s) (+ n 1))))
  (inner s 1))

;寻找stream的第一个等于1的数的index
(= (seq-length (num-seq 7)) 17)

;;Exercise 5
;;3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;3.51
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
#| Returns:


|#
(stream-ref x 7)
#| Returns:


|#


;;3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

#| What is the value for 'sum'?


|#

#| What is the printed response to evaluating the stream-ref and display-stream?


|#

#| Will it be diffferent if we implemented (delay <exp>) as (lambda () <exp>)


|#
;;3.53
#|Describe the elements of the stream


|#

;;3.54
(define (mul-streams s1 s2)
  (error "Not yet implemented"))

;;3.55
(define (partiam-sums stream)
  (error "Not yet implemente"))

;;3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

#| Uncomment this after you defined it 
(define S (cons-stream 1 (merge <??> <??>)))
|#

;;3.64
(define (stream-limit stream tolerance)
  (error "Not yet implemented"))

;;3.66
#| Explain


|#

;;3.68
#| Explain



|#


;;Exercise 6
(define (fract-stream lst)
  (error "Not yet implemented"))


;;Optional Challenge Problem.
;;Note: this is NOT extra credit. Only do it if you want more practice.
#|
  Your explanation Here
|#
