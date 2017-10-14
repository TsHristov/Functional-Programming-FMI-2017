;; Find factorial of n.
;; Recursive approach:

(define (factorial_recursive n)
  (if (= n 1)
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

;; Find GCD of two numbers using the Euclidean algorithm:

(define (gcd x y)
  (cond
   ((= x 0) y)
   ((= y 0) x)
   (else (gcd y (remainder x y)))))

;; Write a function that computes power n of x

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

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

;; Write a function that finds out if a number is prime.

(define (prime? n) (prime n 2))

(define (prime n x)
  (cond
   ((and (<= x (/ n 2)) (= (remainder n x) 0)) #f)
   ((and (<= x (/ n 2)) (not (= (remainder n x) 0)))
	 (prime n (+ x 1)))
   (else #t)))
	   

;; Write a function that solves a quadratic equation ax^2 + bx + c = 0:

(define (descriminant a b c)
  (- (* b b) (* 4 (* a c))))

(define (one_real_root a b)
  (/ (- b) (* 2 a)))

(define (two_real_or_complex_roots a b c)
  (cons
   (/ (+ (- b) (sqrt (descriminant a b c))) (* 2 a))
   (/ (- (- b) (sqrt (descriminant a b c))) (* 2 a))))

(define (quadratic_solver a b c)
  (cond
   ((= a b c 0) 0) ;; When a = b = c = 0
   ((= (descriminant a b c) 0) (one_real_root a b)) ;; When b^2 - 4ac = 0
   (else (two_real_or_complex_roots a b c)))) ;; When b^2 - 4ac < 0 or b^2 - 4ac > 0
