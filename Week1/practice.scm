;; Write a function that finds the n-th fibonacci number:
(define (fibonacci n)
  (define (fibonacci* a b n)
    (if (= n 0)
	a
	(fibonacci* b (+ a b) (- n 1))))
  (fibonacci* 0 1 n))

;; Write a function that checks if x has divisor in the interval [2, n]:
(define (has-divisor? n x)
  (define (divisor? d x)
    (and (> d 0) (= 0 (remainder x d))))
  (cond ((< n 2) #f)
	((divisor? n x) #t)
	(else (has-divisor? (- n 1) x))))

;; Write a function that checks if a number is prime:
(define (prime? x)
  (not (has-divisor? (floor (sqrt x)) x)))

;; Write a function that solves a quadratic equation ax^2 + bx + c = 0:

(define (solve a b c)
  (define (solve-linear)
    (cond ((and (= b 0) (= c 0)) "Inf!")
	  ((= b 0) "No!")
	  (else (/ (- c) b))))
  (define descriminant (- (* b b) (* 4 (* a c))))
  (if (= a 0)
      (solve-linear)
      (if (> descriminant 0)
	  (let ((descriminant-sqrt (sqrt descriminant)))
	    (cons (/ (+ (- b) descriminant-sqrt) (* a 2))
		  (/ (- (- b) descriminant-sqrt) (* a 2))))
	  (if (< descriminant 0)
	      "Not a real number"
	      (/ (- b) (* 2 a))))))

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

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (n-choose-k n k)
  (define n! (factorial n))
  (define k! (factorial k))
  (define n-k! (factorial (- n k)))
  (/ n! (* k! n-k!)))

;; Write a function that computes the binomial coefficient
;; 2nd variant:

(define (nchk* n k)
  (cond ((= n k) 1)
	((zero? k) 1)
	(else (+ (n-choose-k (- n 1) (- k 1))
		 (n-choose-k (- n 1) k)))))

;; Write a function that computes GCD of two numbers:

(define (gcd* a b)
  (cond ((< a b) (gcd* b a))
	(and (= b 0) (= a b) a)
	(else (gcd* b (remainder a b)))))

;; Write a function that computes LCM of two numbers:

(define (lcm* a b)
  (/ (* a b) (gcd* a b)))

;; Write a function that performs fast power alogrithm:

(define (pow* n power)
  (define (pow^2 n) (* n n))
  (define half (quotient n 2))
  (cond
   ((= n 0) 1)
   ((< n) (/ 1 (pow* n (- n))))
   ((even? n) (pow^2 (pow* n (/ n 2))))
   (else (* x (pow^2 (pow* n (floor (/ (- n 1) 2))))))))
