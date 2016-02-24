#lang racket
(require berkeley)
(require "adjectives.rkt") 

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

; (trace after-pattern)
; (after-pattern '(e f g) '(a b c d e f g))



(define (matcherbot-creator pattern)
    (lambda (SENT) (cond ((include? pattern SENT) (after-pattern pattern SENT))
                         ((not (include? pattern SENT)) #f)
                         ((empty? pattern) SENT)
                   )
    )
)

; (define cedric (matcherbot-creator '(hufflepuffs are great)))
; (cedric '(hufflepuffs are great finders))
; (cedric '(what the heck is a hufflepuff))
; (cedric '(slytherins hate hufflepuffs but hufflepuffs are great finders))

; (define cedric2 (matcherbot-creator '()))
; (cedric2 '(hegngn))




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

(define marions-vacay 
          (substitutebot-creator 
              '(indonesia winter noodles) '(texas summer steak)
          )
)

(define empty-sentence (substitutebot-creator '() '()))

; (marions-vacay '(I visited indonesia this summer and ate noodles))
; (empty-sentence '(abc))


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


; (switcherbot '(you are reia but I am a bird))

(define (inquisitivebot sent)
  (if (empty? sent)
      '()
      (se (switcherbot sent) '?)
  )
)

; (inquisitivebot '(I am happy))
; (inquisitivebot '(I can see you))
; (inquisitivebot '())


(define (eliza sent)
  (cond ((empty? sent) '(how can I help you ?))
        ((equal? 'hello (first sent)) '(hello there!))
        ((include? '(I am) sent) (se '(Why are you) (eliza-iam sent) '?))
        ((equal? '? (last sent)) '(I can not answer your question.))
        (else (switcherbot sent))  
  )
)

(define eliza-iam (matcherbot-creator '(I am)))


; (eliza '(hello))
; (eliza '(I am excited to finish unit 1))
; (eliza '(Why are you excited to finish unit 1 ?))
; (eliza '())
; (eliza '(you are reia but she is a bird))

(define (stupidbot-creator motto)
  (lambda (x) (se motto))
)

(define stupidbot (stupidbot-creator '(I am Groot)))

(define (reactorbot-creator bot pat out)
    (lambda (x) ( if(equal? x pat)
                  out
                  (bot x)
                )
    )
)

(define (replacerbot-creator bot pat before after)
    (lambda (x) (if(include? pat x) 
                    (se before (eliza-iam x) after)
                    (bot x)
                )
    )
)


; (define groot (reactorbot-creator stupidbot '(no Groot youll die why are you doing this) '(WE are Groot)))
; (groot '(whats up groot))
; (groot '(no Groot youll die why are you doing this))

(define (babybot sent)
  sent
)

; (define (exaggerate bot n)
;   (lambda (sent) (bot sent))
; )



(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
    (hash-set! hash adj #t))
        adjectives)
    (lambda (wd) (hash-ref hash wd #f))))

(define (add-very sent)
  (cond ((empty? sent) '())
        ((adjective? (first sent)) (se 'very (first sent) (add-very (bf sent))))
        (else (se (first sent) (add-very (bf sent))))
  )
)


(define (exaggerate bot n)
  (if (= n 0)
       bot
       (lambda (x) (bot ((exaggerate bot (- n 1)) (add-very x))))
   )
)



(define exaggerated-babybot1 (exaggerate babybot 2))
(exaggerated-babybot1 '(this soup is hot and tasty))

; (add-very '(this soup is hot and tasty))





