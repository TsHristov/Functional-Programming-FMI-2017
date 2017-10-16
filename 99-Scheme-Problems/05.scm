;; Reverse a list:

(define (reverse-list list-argument)
  (define (reverse-a-list list-argument index)
    (if (> index 0)
	(cons (list-ref list-argument index) (reverse-a-list list-argument (- index 1)))
        (cons (list-ref list-argument index) '())))
  (reverse-a-list list-argument (- (length list-argument) 1)))

