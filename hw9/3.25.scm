#lang planet neil/sicp
; #lang racket
(define (insert! table value . key-list)
  (let ((current-key (car key-list))
        (remain-key (cdr key-list)))
      (let ((record (assoc current-key (cdr table))))
          (cond 
              ; 1) 有记录，且没有其他关键字
              ;    更新记录的值
              ((and record (null? remain-key))
                   (set-cdr! record value)
                   table)
              ; 2) 有记录，且还有其他关键字
              ;    说明这个记录实际上是一个子表
              ;    使用 insert! 递归地进行插入操作
              ((and record remain-key)
                  ; (insert! remain-key value record)
                  (insert! record value remain-key)
                  table)
              ; 3) 无记录，且有其他关键字
              ;    需要执行以下三步：
              ;    一、 创建子表
              ;    二、 对子表进行插入
              ;    三、 将子表加入到 table
              ;    这三个步骤可以用一句完成，wow！
              ((and (not record) (not (null? remain-key)))
                  ; (join-in-table (insert! remain-key value (make-table current-key)) table)
                  (join-in-table (insert! (list current-key) value remain-key) table)
                  table)
              ; 4) 无记录，且无其他关键字
              ;    创建记录并将它加入到 table
              ((and (not record) (null? remain-key))
                  (let ((new-record (cons current-key value)))
                      (join-in-table new-record table)
                      table))))))

(define (join-in-table new-record table)
    (set-cdr! table
              (cons new-record (cdr table))))

(define (lookup table . key-list)
  (let ((current-key (car key-list))
        (remain-key (cdr key-list)))
      (let ((record (assoc current-key (cdr table))))
          (if record
              (if (null? remain-key)
                  (cdr record)
                  (lookup record remain-key))
              #f))))

(define (make-table)
  (list '*table*))

(define t (make-table))

(insert! t 123 'a 'b 'c 'd 'e)
(insert! t 456 'a 'b 'c 'd 'e)


(lookup t 'a 'b 'c 'd 'e)











