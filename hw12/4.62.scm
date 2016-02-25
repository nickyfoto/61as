(assert! (rule (last-pair (?e) (?e))))
(assert! (rule (last-pair (?u . ?v) (?l))
               (last-pair ?v (?l))))

;if only one element in list, the the last-pair
;is the pair itself

;if the last-pair in a list is l
;and the last-pair of (cdr list) is also l
;then l is the last-pair of the list