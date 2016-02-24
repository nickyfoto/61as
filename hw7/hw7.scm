;(load "obj.scm")

; 1 - Modify the person class.

(define-class (person name)
  (instance-vars (sth '()))
  (method (repeat)
    sth)
  (method (say stuff)
      (set! sth stuff)
      stuff
  )
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name))))


; 2 - Determine which definition works as intended.
; In particular, make sure the repeat method works.

; (define-class (double-talker name)
;   (parent (person name))
;   (method (say stuff) (se (usual 'say stuff) (ask self 'repeat))) )

; (define-class (double-talker name)
;   (parent (person name))
;   (method (say stuff) (se stuff stuff)) )

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))) )

;#|
;Definition number ?? works as intended.
;Your explanation here.
;|#


; 3 - Write the random-generator class.

(define-class (random-generator num)
  (class-vars (random-done 0))
  (method (number)
    (set! random-done (+ random-done 1))
    (random num)
  )

  (method (count)
    random-done
  )
)

; 4 - Write the coke-machine class.
; For compatibility with the autograder, make sure that you display
; error messages.  That means you should say:
; (display "Not enough money") and
; (display "Machine empty") when appropriate.

(define-class (coke-machine max-hold unit-price)
  (instance-vars (qty 0)
                 (money 0)
                 (change 0))

  (method (fill num)
    (set! qty (+ qty num))
  )
  (method (deposit x)
    (set! money (+ money x))
  )

  (method (coke)
    (cond ((= qty 0) (display "Machine empty\n"))
          ((< money unit-price) 
           (display "Not enough money\n"))
          (else (begin (set! qty (- qty 1))
                       (set! change (- money unit-price))
                       (set! money (- money unit-price change))
                       change
                )
          )
    )
  )
)



; 5 - Write the deck class.

(define ordered-deck
  (accumulate append '()
	      (map (lambda (suit)
		     (map (lambda (value) (word value suit))
			  '(A 2 3 4 5 6 7 8 9 10 J Q K)))
		   '(s d c h))))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
	(cons card (shuffle (remove card deck))) )))


; 6 - Write the miss-manners class.

