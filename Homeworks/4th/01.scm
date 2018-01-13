;; insert-before: Inserts an element before a sublist in a given list:
;; Example: (insert-before 'a '(1 2 3) '(1 1 2 3 4)) -> (1 a 1 2 3 4)
(define (insert-before element sublist list)
  (define prefix (take list (length sublist)))
  (if (equal? prefix sublist)
      (cons element list)
      (cons (car list) (insert-before element sublist (cdr list)))))
