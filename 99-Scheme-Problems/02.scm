;; Find the last but one element of a list:

(define (last-but-one list-argument)
  (if (null? (cdr (cdr list-argument)))
      (car list-argument)
      (last-but-one (cdr list-argument))))
	     

