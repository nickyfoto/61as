#lang racket
(require berkeley)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define empty-board '())

(define (adjoin-position row col queens)
  (adjoin-set (cons col row) queens))

(define (cross+horizontal col p)
  (define (inner-rec row)
    (cond ((<= row 0) (inner-rec (+ row (- col (car p)))))
          ((< (- col (car p)) (- row (cdr p))) nil)
          ((< 8 row) nil)
          (else 
            (cons (cons col row) (inner-rec (+ row (- col (car p))))))))
  (inner-rec (- (cdr p) (- col (car p)))))

(define (total col positions)
  (if (null? positions)
      nil
      (append (cross+horizontal col (car positions))
            (total col (cdr positions)))))

(define (safe? col positions)
  (if (null? (cdr positions))
      #t
      (not (element-of-set? (car positions) (total col (cdr positions))))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))


