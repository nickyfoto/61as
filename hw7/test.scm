(define-class (singer)
    (method (introduce) '(I am aiming for MTV awards!))
    (method (sing) '(tralala lalala)))

(define-class (TA)
    (method (introduce) '(GO BEARS!))
    (method (teach) '(Let me help you with that box-and-pointer diagram)) )

(define-class (singer-TA)
    (parent (singer) (TA)) )

(define-class (TA-singer)
    (parent (TA) (singer)) )