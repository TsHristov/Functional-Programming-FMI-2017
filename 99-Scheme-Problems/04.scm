;; Find the number of elements in a list:

(define (elements-count list-argument)
  (define (count list-argument counter)
    (if (null? list-argument)
	counter
	(count (cdr list-argument) (+ 1 counter))))
  (if (not (list? list-argument))
      "Not a list!"
      (count list-argument 0)))

