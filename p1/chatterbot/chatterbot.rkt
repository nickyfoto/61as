#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))
; (require berkeley)
;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    sent
  )

;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    (lambda (x) (se motto))
    ; (error "not yet implemented")
  )

;;Q3 - matcherbot-creator
  (define (first-n sent n)
    (if (= n 0)
        '()
        (se (first sent) (first-n (bf sent) (- n 1)))
    )
  )

  (define (include? s1 s2)
    (cond ((equal? s1 (first-n s2 (count s1))) #t)
          ((= (count s1) (count s2)) (equal? s1 s2))
          (else (include? s1 (bf s2)))
    )
  )

; (include? '(d e f) '(a b c d e f g))

; (include? '(hufflepuffs are great) '(hufflepuffs are great finders))

  (define (after-n sent n)
    (if (= n 0)
        sent
        (after-n (bf sent) (- n 1))
    )
  )

; (after-n '(a b c d e f g) 4)


  (define (after-pattern pattern sent)
    (cond ((equal? pattern (first-n sent (count pattern))) (after-n sent (count pattern)))
          ((= (count pattern) (count sent)) (equal? pattern sent))
          (else (after-pattern pattern (bf sent)))
    )
  )
  (define (matcherbot-creator pattern)
    (lambda (SENT) (cond ((include? pattern SENT) (after-pattern pattern SENT))
                         ((not (include? pattern SENT)) #f)
                         ((empty? pattern) SENT)
                   )
    )
  )

;;Q4 - substitutebot-creator
  (define (correspond-substitute from to sub)
    (cond ((empty? from) '())
          ((equal? (first from) sub) (first to))
          (else (correspond-substitute (bf from) (bf to) sub))
    )
  )

  ; (correspond-substitute '(indonesia winter noodles) '(texas summer steak) 'noodles)


  (define (substitutebot-helper from to sent)
    (cond ((empty? sent) '())
          ((member? (first sent) from) (se 
                    (correspond-substitute from to (first sent)) 
                    (substitutebot-helper from to (bf sent))
                                        )
          )
          (else (se (first sent) (substitutebot-helper from to (bf sent)))) 
    )
  )


  (define (substitutebot-creator from to)
    (lambda (sent) (substitutebot-helper from to sent))
  )

;;Q5 - switcherbot
  (define (switcherbot sent)
      (let ((res
            (substitutebot-helper 
              '(me I am was my yours you you are were your mine) 
              '(you you are were your mine me I am was my yours)
              sent
            )
          )
         ) 
      (cond ((equal? 'me (first res)) (se 'I (bf res)))
            (else (se res))
      )
    )
  )


;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    (if (empty? (se sent))
        '()
        (se (switcherbot sent) '?)
    )
  )
  
;;Q7 - eliza
  (define eliza-iam (matcherbot-creator '(I am)))

  (define (eliza sent)
    (cond ((empty? sent) '(how can I help you ?))
        ((equal? 'hello (first sent)) '(hello there!))
        ((include? '(I am) sent) (se '(why are you) (eliza-iam sent) '?))
        ((equal? '? (last sent)) '(I can not answer your question.))
        (else (switcherbot sent))  
  )
  )

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    (lambda (x) ( if(equal? x pat)
                  out
                  (bot x)
                )
    )
  )

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    (lambda (x) (if(include? pat x) 
                    (se before (eliza-iam x) after)
                    (bot x)
                )
    )
  )

(define (add-very sent)
  (cond ((empty? sent) '())
        ((adjective? (first sent)) (se 'very (first sent) (add-very (bf sent))))
        (else (se (first sent) (add-very (bf sent))))
  )
)

;;Q10 - exagerate
  (define (exaggerate bot n)
    (if (= n 0)
       bot
       (lambda (x) (bot ((exaggerate bot (- n 1)) (add-very x))))
   )
  )

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
