#lang planet neil/sicp
; (require berkeley)

(define (make-account balance pw)

  (define wrong-pw-count 0)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (response w)
    (if (<= 7 wrong-pw-count)
        "call-police"
        "wrong password")
  )

  (define (call-police w) "call-police")

  (define (dispatch password m)
    (if (eq? password pw)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        (begin (set! wrong-pw-count (+ 1 wrong-pw-count))
               response)
    )
  )
  dispatch)

(define acc (make-account 100 'secret-password))

; ((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
