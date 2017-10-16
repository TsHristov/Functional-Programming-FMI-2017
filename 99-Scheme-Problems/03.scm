;; Find the K-th element of a list:

(define (kth-element list-argument k)
  (if (= k 1)
      (car list-argument)
      (kth-element (cdr list-argument) (- k 1))))
