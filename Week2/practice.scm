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

(define (solve a b c)
  (cond
   ((= a b c 0) "every x")
   ((and (= a b 0) (not (= c 0))) "no roots")
   ((and (= a 0) (not (= b c 0))) (/ (- c) b)))
  (let ((descriminant (- (* b b) (* 4 a c))))
    (cond
     ((< d 0) "no roots")
     ((= d 0) ( / (- b) (* 2 a)))
     (else
      (cons
       (/ (- (- b) (sqrt descriminant)) (* 2 a))
       (/ (+ (- b) (sqrt descriminant)) (* 2 a)))))))

;; Write a function that gives the binary representation of a non-negative decimal number:

(define (decimal-to-binary number)
  (if (not (= number 0))
      (cons (remainder number 2) (decimal-to-binary (quotient number 2)))
      (cons (remainder number 2) '())))

(define (binary number)
  (reverse (decimal-to-binary number)))
