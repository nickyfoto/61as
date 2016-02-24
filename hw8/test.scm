; #lang planet neil/sicp

(define (make-account balance) 
  (define (withdraw amount) 
    (set! balance (- balance amount)) balance) 
  (define (deposit amount) 
    (set! balance (+ balance amount)) balance) 
  (define (dispatch msg) 
    (cond ((eq? msg 'withdraw) withdraw) 
          ((eq? msg 'deposit) deposit) ) ) 
  dispatch)

(define (make-account2 init-amount) 
  (let ((balance init-amount)
       (transactions '()))
    (define (withdraw amount) 
      (set! balance (- balance amount))
      (set! transactions (cons (list 'withdraw amount) transactions))
      balance) 
    (define (deposit amount) 
      (set! balance (+ balance amount)) 
      (set! transactions (cons (list 'deposit amount) transactions))
      balance) 
    (define (dispatch msg) 
      (cond ((eq? msg 'withdraw) withdraw) 
            ((eq? msg 'deposit) deposit)
            ((eq? msg 'balance) balance)
            ((eq? msg 'transactions) transactions)
      ) 
    ) 
    dispatch) )




(define acc (make-account2 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 60)
((acc 'deposit) 60)
; (acc 'balance)
; (acc 'transaction)
