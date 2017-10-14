;; Find the last but one element of a list:
(define (last_but_one list_argument)
  (if (null? (cdr (cdr list_argument)))
      (car list_argument)
      (last_but_one (cdr list_argument))))
	     

