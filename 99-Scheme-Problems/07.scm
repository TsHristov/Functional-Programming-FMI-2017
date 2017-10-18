;; Flatten a nested list structure
;; Example: (a (b (c d) e)) => (a b c d)

(define (flatten l)
  (if (not (null? l))
      (if (list? (car l))
	  (append (flatten (car l)) (flatten (cdr l)))
	  (cons (car l) (flatten (cdr l))))
  l))	  
