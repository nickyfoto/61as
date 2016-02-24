#lang planet neil/sicp
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))


(define (unroll-stream s n)
  (if (< n 0)
      '()
      (cons (stream-car s)
            (unroll-stream (stream-cdr s) (- n 1)))))

(define (display-limited-stream s n)
  (for-each
    display-line (unroll-stream s n))
)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (my-map proc . items-list)
  (define (combiner l1 l2)
    (if (null? l2)
        l1
        (cons (proc (car l1) (car l2))
              (combiner (cdr l1) (cdr l2)))))
 
  
  (accumulate combiner '() items-list)
)

(my-map + '(1 2 3) '(4 5 6) '(7 8 9))


; (define (stream-map proc s)
;   (if (stream-null? s)
;       the-empty-stream
;       (cons-stream (proc (stream-car s))
;                    (stream-map proc (stream-cdr s)))))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define s1 (stream-enumerate-interval 1 3))
(define s2 (stream-enumerate-interval 4 6))
(define s3 (stream-enumerate-interval 7 9))
; (display-stream s1)
; (display-stream s2)
; (display-stream s3)

; (define (square x) (* x x))
; (define s1-square (stream-map square s1))

; (display-stream s1-square)

; (define (stream-map proc . argstreams)
;   (if (<??> (car argstreams))
;       the-empty-stream
;       (<??>
;        (apply proc (map <??> argstreams))
;        (apply stream-map
;               (cons proc (map <??> argstreams))))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


; (stream-map + s1 s2 s3)
(display-stream (stream-map + s1 s2 s3))




