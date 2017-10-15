;; Find the K`th element of a list:
(define (kth_element list_argument k)
  (cond
   ((<= k 0) "Invalid index!")
   (else kth list_argument k 1)))

(define (kth list_argument k index)
  (cond
   ((null? (cdr list_argument)) "none found")
   ((= index k) (car list_argument))
   ((< index k)
          (kth (cdr list_argument) k (+ index 1)))))
