(define Dormitory (instantiate place 'Dormitory))
(define kirin (instantiate place 'kirin))

(define HuangQiang (instantiate person 'HuangQiang Dormitory))

(define potstickers (instantiate thing 'potstickers))



(can-go Dormitory 'down art-gallery)
(can-go art-gallery 'up Dormitory)
(can-go Soda 'north kirin)
(can-go kirin 'south Soda)

(ask kirin 'appear potstickers)

(ask HuangQiang 'go 'down)
(ask HuangQiang 'go 'down)
(ask HuangQiang 'go 'north)
(ask HuangQiang 'take potstickers)
(ask HuangQiang 'go 'south)
(ask HuangQiang 'go 'up)
(ask HuangQiang 'go 'west)
(ask HuangQiang 'lose potstickers)
(ask Brian 'take potstickers)
(ask HuangQiang 'go 'east)
(ask HuangQiang 'go 'down)
(ask HuangQiang 'go 'down)

(define 61A-Lab-Garage (instantiate garage '61A-Lab-Garage))
(define HQ-auto (instantiate thing 'HQ-auto))
(ask 61A-Lab 'appear HQ-auto)
(ask HuangQiang 'take HQ-auto)
(can-go 61A-Lab 'down 61A-Lab-Garage)
(can-go 61A-Lab-Garage 'up 61A-Lab)
(ask HuangQiang 'go 'down)

(define park-table (make-table))

(ask 61A-Lab-Garage 'park HQ-auto)
(ask 61A-Lab-Garage 'unpark (show-ticket HuangQiang))

(define library (instantiate hotspot 'library 1234))
(define somelaptop (instantiate laptop 'somelaptop))
(define laptoptester (instantiate person 'laptoptester library))

(ask library 'appear somelaptop)
(ask laptoptester 'take somelaptop)
(ask somelaptop 'connect 1234)


(define pesto-pasta (instantiate pasta))
(define new-bagel (instantiate bagel))

(define somerestaurant (instantiate restaurant 'somerestaurant pasta 7))
(define anotherrestaurant (instantiate restaurant 'anotherrestaurant bagel 11))

(ask somerestaurant 'menu)
(define foodbuyer (instantiate person 'foodbuyer somerestaurant))
(ask foodbuyer 'buy 'pasta)

(can-go somerestaurant 'down anotherrestaurant)
(ask foodbuyer 'go 'down)
(ask foodbuyer 'buy 'bagel)


(define notfood (instantiate thing 'notfood))
(ask anotherrestaurant 'appear notfood)
(ask foodbuyer 'take notfood)

(can-go anotherrestaurant 'down sproul-plaza)
(define somepolice (instantiate police 'grammarpolice Pimentel))
(define somepolice2 (instantiate police 'somepolice2 s-h))
(define somepolice3 (instantiate police 'somepolice3 Telegraph-Ave))
(define freepolice (instantiate police 'freepolice somerestaurant))

(define jail (instantiate place 'jail))