;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (basic-object)
  (instance-vars
    (properties (make-table)))
  (method (put key value)
    (insert! key value properties)
  )
  (method (get key)
    (lookup key properties)
  )
  (default-method (ask self 'get message))
)



(define-class (place name)
  (parent (basic-object))
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (initialize
    (ask self 'put 'place? #t))
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	      (error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (for-each (lambda (p) (ask p 'notice new-person)) (cdr people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)
  
  (method (may-enter? person)
    (if (person? person)
        #t
    )
  )

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )

(define-class (hotspot name pswd)
  (parent (place name))
  (instance-vars 
    (connected '()))
  (method (connect laptop password)
    (if (eq? password pswd)
        (if (memq laptop connected)
            (error "laptop already connected" (list name laptop))
            (set! connected (cons laptop connected)))
        (error "Wrong Password for Hotspot")
    )
  )
  (method (surf laptop url)
    (if (memq laptop connected)
        (system (string-append "lynx " url))
        (error "laptop not connected")
    )
  )
)


(define-class (garage name)
  (parent (place name))

  (method (save vehicle)
    (let ((sn (sn-generator))
          (p (ask vehicle 'possessor)))
      (insert! sn vehicle park-table)
      (ask p 'lose vehicle)
      (define tkt (instantiate ticket sn))
      (ask self 'appear tkt)
      (ask p 'take tkt)
    )
  )

  (method (park vehicle)
    (if (check vehicle self)
        (ask self 'save vehicle)
        (error "Vehicle not here")
    )
  )

  (method (unsave tkt)
    (let ((v (lookup (ask tkt 'sn) park-table)))
      (ask (ask tkt 'possessor) 'take v)
      (ask (ask tkt 'possessor) 'lose tkt)
      (insert! (ask tkt 'sn) #f park-table)
    )
  )

  (method (unpark tkt)
    (if (and (eq? 'ticket (ask tkt 'name))
             (lookup (ask tkt 'sn) park-table))
        (ask self 'unsave tkt)
        (error "not a ticket or no vehicle found")
    )
  )
)

(define-class (locked-place name)
  (parent (place name))
  (instance-vars (locked #t))
  (method (unlock)
    (set! locked #f)
  )
  (method (may-enter? person)
    (if locked
        #f
        #t)
  )
)


(define-class (person name place)
  (parent (basic-object))
  (instance-vars
   (possessions '())
   (saying ""))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'person? #t)
   (ask self 'put 'strength 50)
   (ask self 'put 'money 100))
  (method (get-money num)
    (ask self 'put 'money (+ (ask self 'money) num))
  )
  (method (pay-money num)
    (let ((balance (ask self 'money)))
         (if (< balance num)
             #f
             (begin (ask self 'put 'money (- balance num))
              #t)
         )
    )
  )
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
    ((ask thing 'may-take? self)
	  ; (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)
	   'taken)))

  (method (take-all)
    (for-each (lambda (thing) (ask self 'take thing))
      (filter (lambda (obj) (eq? (ask obj 'possessor) 'no-one))
              (ask place 'things))))

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))

  (method (go-directly-to plc)
    (ask place 'exit self)
    (announce-move name place plc)
    (set! place plc)
    (ask plc 'enter self)
  )

  (method (buy foodname)
    (let ((obj (ask place 'sell self foodname)))
         (if obj
             (begin (ask place 'appear obj)
                    (ask self 'take obj))
             (error "no food returned")
         )
    )
  )


  (method (eat)
    (let ((total-calories 0))
      (for-each 
        (lambda (obj) 
          (set! total-calories (+ total-calories (ask obj 'calories)))
          (ask self 'lose obj)
          (ask (ask self 'place) 'gone obj))
            (filter edible? possessions))
      (ask self 'put 'strength total-calories)
    )
  )

  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place) (error "Can't go" direction))
            ((not (ask new-place 'may-enter? self)) 
             (error "Locded"))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self))))))





(define-class (thing name)
  (parent (basic-object))
  (instance-vars 
    (possessor 'no-one))
  (initialize 
    (ask self 'put 'thing? #t))
  (method (initialize value-for-self) (set! self value-for-self))
  (method (send-usual-to-parent) 
          (error "Can't use USUAL without a parent." 'thing))
  (method (possessor) possessor)
  (method (type) 'thing)
  (method (change-possessor new-possessor) 
          (set! possessor new-possessor))

  (method (may-take? receiver)
    (if (not (eq? (ask self 'possessor) 'no-one))
        (if (< (ask receiver 'strength) (ask (ask self 'possessor) 'strength)) 
            #f
            self)
        self
    )
  )
)

(define-class (ticket sn)
  (parent (thing 'ticket)))

(define-class (laptop name)
  (parent (thing name))
  (method (connect pass)
    (ask (ask (ask self 'possessor) 'place) 'connect self pass))
  (method (surf url)
    (ask (ask (ask self 'possessor) 'place) 'surf self url)))

; (define thing
;   (let ()
;     (lambda (class-message)
;       (cond
;        ((eq? class-message 'instantiate)
; 	(lambda (name)
; 	  (let ((self '()) (possessor 'no-one))
; 	    (define (dispatch message)
; 	      (cond
; 	       ((eq? message 'initialize)
;       		(lambda (value-for-self) (set! self value-for-self)))
;   	     ((eq? message 'send-usual-to-parent)
;   		    (error "Can't use USUAL without a parent." 'thing))
; 	       ((eq? message 'name) (lambda () name))
; 	       ((eq? message 'possessor) (lambda () possessor))
; 	       ((eq? message 'type) (lambda () 'thing))
; 	       ((eq? message 'change-possessor)
; 		      (lambda (new-possessor) (set! possessor new-possessor)))
; 	       (else (no-method 'thing))
;         )
;       )
; 	    dispatch)))
;        (else (error "Bad message to class" class-message))))))




(define-class (food name num)
  (parent (thing name))
  (initialize
    (ask self 'put 'edible? #t)
    (ask self 'put 'calories num))
)

(define-class (pasta)
  (parent (food 'pasta 150))
  (class-vars (name 'pasta))
)

(define-class (bagel)
  (parent (food 'bagel 33))
  (class-vars (name 'bagel))
)

(define-class (restaurant name foodkind price)
  (parent (place name))
  (method (menu)
    (list (ask foodkind 'name) price))
  (method (sell person foodname)
    (if (and (eq? foodname (ask foodkind 'name)) (not (police? person)))
        (if (ask person 'pay-money price)
            (instantiate foodkind)
            #f)
        (instantiate foodkind)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define *foods* '(pizza potstickers coffee))

(define (edible? obj)
  (ask obj 'edible?))



(define-class (thief initial-name initial-place)
  (parent (person initial-name initial-place))
  (instance-vars
   (behavior 'steal))
  (initialize (ask self 'put 'strength 100))
  (method (type) 'thief)

  (method (notice person)
    (if (and (eq? behavior 'run) 
             (not (null? (ask (usual 'place) 'exits))))
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

(define-class (police initial-name initial-place)
  (parent (person initial-name initial-place))
  (instance-vars (shout-at-thief "Crime Does Not Pay"))
  (initialize 
    (ask self 'put 'strength 1000)
    (ask self 'put 'police? #t))
  (method (notice person)
    (if (eq? (ask person 'type) 'thief)
        (begin (print shout-at-thief)
               (for-each (lambda (obj) (ask self 'take obj)) 
                         (ask person 'possessions))
               (ask person 'go-directly-to jail))

    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (ask obj 'person?))

(define (police? obj)
  (ask obj 'police?))

(define (thing? obj)
  (ask obj 'thing?))

(define (place? obj)
  (ask obj 'place?))


(define (name obj) (ask obj 'name))

(define (inventory obj)
    (if (person? obj)
        (map name (ask obj 'possessions))
        (map name (ask obj 'things))))

(define (whereis obj)
  (if (person? obj)
      (ask (ask obj 'place) 'name)
      (error "obj not person")
  )
)

(define (owner obj)
  (if (thing? obj)
      (if (eq? (ask obj 'possessor) 'no-one)
          'no-one
          (ask (ask obj 'possessor) 'name)
      )
      (error "obj not thing")
  )
)

(define (people-in-place obj)
  (map (lambda (x) (ask x 'name)) (ask obj 'people))
)

(define (check vehicle plc)
  (let ((all-things (ask plc 'things)))
    (if (null? all-things)
      #f
      (if (eq? vehicle (car all-things))
          #t
          (check vehicle (cdr all-things))
      )
    )
  )
)

(define serial-number 0)

(define (sn-generator)
  (set! serial-number (+ 1 serial-number))
  serial-number
)

(define (show-ticket person)
  (define (inner-rec things)
    (if (null? things)
        #f
        (if (eq? 'ticket (ask (car things) 'name))
            (car things)
            (inner-rec (cdr things))
        )
    )
  )
  (inner-rec (ask person 'possessions))
)