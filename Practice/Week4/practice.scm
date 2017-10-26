;; Task 5:
;; Function that returns the composition of f and g -> (f(g(x)))
(define (compose f g) (lambda (x) (f (g x))))

;; Task 6:
;; Function that returns a function that is applied 2 times:
(define (double f) (lambda (x) (f (f x))))


