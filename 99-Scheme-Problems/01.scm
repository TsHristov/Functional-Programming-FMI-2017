;; Find the last element of a list:
(define (find_last list_argument)
  (if (null? (cdr list_argument))
      (car list_argument)
      (find_last (cdr list_argument))))
