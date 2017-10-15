;; Reverse a list:
(define (reverse-list l)
  (reverse-a-list l (- (length l) 1)))

(define (reverse-a-list l index)
  (if (> index 0)
      (cons (list-ref l index) (reverse-a-list l (- index 1)))
      (cons (list-ref l index) '())))
