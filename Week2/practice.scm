;; Accumulator function:
(define (accumulate initial accumulator start next end-condition? term)
  (define (linear-accumulator result current)
    (if (end-condition? current)
	result
	(linear-accumulator (accumulator result (term current)) (next current))))
  (linear-accumulator initial start))

;; Function that computes factorial (via accumulator):
(define (factorial n)
  (accumulate 1 * 1 (lambda (x) (+ x 1)) (lambda (x) (> x n)) (lambda (x) x))) 

;; Function that computes the sum of the numbers between 1 and 100 (via accumulate):
(define (range-sum)
  (accumulate 0 + 1 (lambda (x) (+ x 1)) (lambda (x) (> x 100)) (lambda (x) x)))

;; Function that checks if number is prime (via accumulate):
(define (divisor? x number) (= (remainder number x) 0))
(define (prime? number)
  (not (accumulate #t (lambda (x y) (and x y)) 2
	       (lambda (x) (+ x 1))
	       (lambda (x) (or (divisor? x number) (= x (ceiling (sqrt number)))))
	       (lambda (x) (divisor? x number)))))


