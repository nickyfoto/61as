#|
Exercise 1. Why did the student get an error?


|#

; Exercise 2
; Exercise 2a. Fill in the ?? so that the calls produce the desired effect.

(define list1 (list (list 'a) 'b))
(define list2 (list (list 'x) 'y))
(define (answer3)
  (set-cdr! ?? ??)
  (set-cdr! ?? ??))
(answer3)
list1 ; Should output ((a x b) b)
list2 ; Should output ((x b) y)
(set-cdr! (car list1) (list (caar list2) (cadr list1)))
(set-cdr! (car list2) (list (cadr list1)))

              b            y
(set-car! (cdr list1) (cadr list2))
list1 ;((a x b) y)
;Exercise 2b.  Draw a box-and-pointer diagram that explains the effect 
;              of evaluating
;(set-car! (cdr list1) (cadr list2)).
;(Reminder:  You can use ASCII art or submit a jpg or pdf file.)


;Exercise 3. 
;SICP 3.13
;Draw the box-pointer diagram of z
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;What happens if we try to compute (last-pair z)?

;SICP 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
  
;What does mystery do in general?


(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;Draw the box-pointer diagram of v before the call to mystery, 
;v after the call to mystery, and w


;What would be printed as the values of v and w?

;(d c b a)

;Exercise 4.
;SICP 3.16 Draw the 4 box-and-pointer diagrams.
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
		 
 #|
a. Returns 3:

b. Returns 4:

c. Returns 7:

d. Never returns:

|#

;SICP 3.17 Write a correct version of count-pairs.
(define (count-pairs x)
	(error "Not implemented yet!"))


;SICP 3.21 Explain what Eva Lu Ator is talking about, and what happened with
;Ben's examples.  Then define print-queue.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
	  
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 
#| What happened with Ben's examples?




|#
; Implement the definition of print-queue
;Make sure you use display to print the queue.
(define (print-queue queue)
	(error "Not implemented yet!"))


;SICP 3.25 Write lookup and insert!

(define (lookup keys table)
	(error "Not implemented yet!")
)

(define (insert! keys value table)
	(error "Not implemented yet!")
)

#|
SICP 3.27

Explain why the number of steps is proportional to n (you may want to
include a trace to explain).

Would it still work (efficiently) if we define memo-fib as (memoize
fib)?  Why or why not?

|#

;Exercise 5. Write vector-append.
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

;Exercise 6. Write vector-filter.

(define (build-vector index-list vec)
  (let ((res (make-vector (length index-list))))
    (define (loop newvec i)
      (if (= i (length index-list))
          newvec
          (begin (vector-set! newvec i (vector-ref vec (list-ref index-list i)))
                 (loop newvec (+ i 1)))))
    (loop res 0))
)

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
       (build-vector (reverse(loop 0)) vec)
  )
)



;Exercise 7. Write bubble-sort!
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

; The order of growth of the running time of bubble sort is Theta(??)
