;; Find out whether a list is a palindrome:

(define (palindrome? list-argument)
  (if (list? list-argument)
      (equal? list-argument (reverse list-argument))
      "Not a list!"))
