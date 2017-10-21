;; Find factorial of n.
;; Recursive approach:

(define (factorial_recursive n)
  (if (= n 0)
      1
      (* n (factorial_recursive (- n 1)))))

;; Find factorial of n.
;; Iterative approach:

(define (factorial_iterative n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))

;; Write a function that finds the sum of digits of a number.
;; Recursive approach:

(define (digits_sum_recursive number)
  (if (< number 10)
      number
      (+ (remainder number 10)
	 (digits_sum_recursive (quotient number 10)))))

;; Write a function that finds the sum of digits of a number.
;; Iterative approach:

(define (digits_sum_iterative number)
  (digits_sum_helper number 0))

(define (digits_sum_helper number sum)
  (if (< number 10)
      (+ sum number)
      (digits_sum_helper (quotient number 10) (+ sum (remainder number 10)))))

;; Write a function that gives the binary representation of a non-negative decimal number:

(define (decimal-to-binary number)
  (if (not (= number 0))
      (cons (remainder number 2) (decimal-to-binary (quotient number 2)))
      (cons (remainder number 2) '())))

(define (binary number)
  (reverse (decimal-to-binary number)))
