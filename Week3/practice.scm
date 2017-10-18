;; Write a function that finds the n-th fibonacci number:
(define (fibonacci n)
  (define (fib a b n)
    (if (= n 0)
	a
	(fib b (+ a b) (- n 1))))
  (fib 0 1 n))

;; Redefine 'if' using 'and' and 'or':
(define (if* p x y)
  (or (and p x) y))

;; Redefine 'not':
(define (not* x)
  (if x #f #t))

;; Redefine 'and':
(define (and* x y)
  (if x y #f))

;; Redefine 'or':
(define (or* x y)
  (if x #t y))

;; Redefine 'bool':
(define (bool* x)
  (if x #t #f))

;; Write a function that computes the binomial coefficient:

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (nchk n k)
  (define n! (fact n))
  (define k! (fact k))
  (define n-k! (fact (- n k)))
  (/ n! (* k! n-k!)))

;; Write a function that computes the binomial coefficient
;; 2nd variant:

(define (nchk* n k)
  (cond ((= n k) 1)
	((zero? k) 1)
	(else (+ (nchk (- n 1) (- k 1))
		 (nchk (- n 1) k)))))

;; Write a function that computes GCD of two numbers:

(define (gcd a b)
  (cond ((< a b) (gcd b a))
	(and (= b 0) (= a b) a)
	(else (gcd b (remainder a b)))))

;; Write a function that computes LCM of two numbers:

(define (lcm a b)
  (/ (* a b) (gcd a b)))

;; Write a function that performs fast power alogrithm:

(define (pow* n power)
  (define (pow^2 n) (* n n))
  (define half (quotient n 2))
  (cond
   ((= n 0) 1)
   ((< n) (/ 1 (pow* n (- n))))
   ((even? n) (pow^2 (pow* n (/ n 2))))
   (else (* x (pow^2 (pow* n (floor (/ (- n 1) 2))))))))

;; Write a function that computes the following sequence:
;; (1 / (1 * 3)) + (1 / (5 * 7)) + (1 / (9 * 11)) + ...

