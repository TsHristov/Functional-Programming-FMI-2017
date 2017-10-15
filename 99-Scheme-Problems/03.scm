;; Find the K`th element of a list:

(define (kth l k)
  (if (and (> k 0) (<= k (length l)) (list? l))
      (kth-element l k 1)))

(define (kth-element l k index)
  (if (= index k)
      (car l)
      (kth-element (cdr l) k (+ 1 index))))
