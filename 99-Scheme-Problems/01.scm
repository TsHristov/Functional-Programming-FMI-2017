;; Find the last element of a list:

(define (find-last list-argument)
  (if (null? (cdr list-argument))
      (car list-argument)
      (find-last (cdr list-argument))))
