;; Find out whether a list is a palindrome

(define (is-palindrome l)
  (if (list? l)
      (equal? l (reverse l))
      "Not a list!"))
