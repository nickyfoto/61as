#lang planet neil/sicp

;;Place
;;(ask Dormitory 'type) -> place
;;(ask Dormitory 'exits) -> (down)
;;(ask Dormitory 'things) -> ()


; STk> (ask bagel 'possessor)
; no-one

; (load "start.scm")

; (define Tester (instantiate person 'Tester s-h))
; (ask s-h 'exits)
; (ask Tester 'go 'west)

; (sproul-hall-exit 1)

; (define warehouse (instantiate locked-place 'warehouse))
; (can-go Telegraph-Ave 'down warehouse)
; (ask street-person 'go 'down)


; (check HQ-auto 61A-Lab-Garage)

; (ask 61A-Lab-Garage 'park HQ-auto)
; (ask 61A-Lab-Garage 'unpark (show-ticket HuangQiang))

; (if (and #t #f)
;     "yes"
;     "no"
; )

; (define HQ-auto2 (instantiate thing 'HQ-auto2))
; (ask 61A-Lab-Garage 'appear HQ-auto2)
; (ask 61A-Lab-Garage 'park HQ-auto2)


; (filter (lambda (x) (remainder (= 0 (/ x 2)))) (list 1 2 3 4))



(ask Brian 'put 'strength 100)

(define library (instantiate hotspot 'library 1234))
(ask library 'appear somelaptop)




(define jail (instantiate place 'jail))



(define false-police (instantiate person 'false-police Telegraph-Ave))
(ask false-police 'go 'north)

(define foodbuyer (instantiate person 'foodbuyer somerestaurant))








; (define somepolice (instantiate police 'grammarpolice Pimentel))
; (define somepolice2 (instantiate police 'somepolice2 s-h))
; (define somepolice3 (instantiate police 'somepolice3 Telegraph-Ave))

(ask foodbuyer 'go 'down)










