;; Accumulator function:
(define (accumulator initial accumulator start next end-condition? term)
  (define (linear-accumulator result current)
    (if (end-condition? current)
	result
	(linear-accumulator (accumulator result (term current)) (next current))))
  (linear-accumulator initial start))

;; Function that computes factorial (via accumulator):
(define (factorial n)
  (accumulator 1 * 1 (lambda (x) (+ x 1)) (lambda (x) (> x n)) (lambda (x) x))) 

;; Function that computes the sum of the numbers between 1 and 100 (via accumulator):
(define (range-sum)
  (accumulator 0 + 1 (lambda (x) (+ x 1)) (lambda (x) (> x 100)) (lambda (x) x)))




