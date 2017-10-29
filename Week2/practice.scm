;; accumulate: Accumulator function:
(define (accumulate initial accumulator start next end-condition? term)
  (define (linear-accumulator result current)
    (if (end-condition? current)
	result
	(linear-accumulator (accumulator result (term current)) (next current))))
  (linear-accumulator initial start))

;; factorial: Computes factorial (via accumulate):
(define (factorial n)
  (accumulate 1 * 1 (lambda (x) (+ x 1)) (lambda (x) (> x n)) (lambda (x) x))) 

;; range-sum: Computes the sum of the numbers between 1 and 100 (via accumulate):
(define (range-sum)
  (accumulate 0 + 1 (lambda (x) (+ x 1)) (lambda (x) (> x 100)) (lambda (x) x)))

;; prime?: Checks if number is prime (via accumulate):
(define (prime? number)
  (define (divisor? x number)
    (and (> x 0) (= (remainder number x) 0)))
  (cond
   ((< number 2) #f)
   (else
    (let ((square-root (floor (sqrt number))))
      (accumulate #t (lambda (x y) (and x y)) square-root
		     (lambda (x) (- x 1))
	             (lambda (x) (< x 2))
	             (lambda (x) (not (divisor? x number))))))))

;; primes-count: Finds the count of prime numbers in a given interval [a, b] (via accumulate):
(define (primes-count a b)
  (accumulate 0 + a
	      (lambda (x) (+ x 1))
	      (lambda (x) (> x b))
	      (lambda (x) (if (prime? x) 1 0))))






