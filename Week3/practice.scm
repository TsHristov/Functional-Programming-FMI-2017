;; Write a function that finds the n-th fibonacci number:
(define (fibonacci n)
  (define (fib a b n)
    (if (= n 0)
	a
	(fib b (+ a b) (- n 1))))
  (fib 0 1 n))

;; Write a function that performs fast power algorithm:
