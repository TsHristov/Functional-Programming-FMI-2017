;; Find the number of elements in a list:
(define (elements-count list-argument)
  (if (not (list? list-argument))
      "Not a list!"
      (count-helper list-argument 0)))

(define (count-helper list-argument counter)
  (if (null? list-argument)
      counter
      (count-helper (cdr list-argument) (+ 1 counter))))
