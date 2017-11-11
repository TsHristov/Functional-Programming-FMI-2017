;; accumulate: Recursive accumulate:
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; filter-accumulate: Recursive accumulate with filter:
(define (filter-accumulate op nv a b term next p?)
  (cond
   ((> a b) nv)
   ((p? a) (op (term a) (filter-accumulate op nv (next a) b term next)))
   (else (filter-accumulate op nv (next a) b term next))))

;; accumulate-i: Iterative accumulate:
(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

;; filter-accumulate-i: Iterative accumulate with filter:
(define (filter-accumulate-i op nv a b term next p?)
  (if (> a b) nv
      (cond
       ((p? a) (filter-accumulate-i op (op nv (term a)) (next a) b term next p?))
       (else (filter-accumulate-i op nv (next a) b term next p?)))))

;; !!n: Calculates the product of all numbers <= of n, with the same parity:
(define (!!n n)
  (define start (if (odd? n) 1 2))
  (accumulate * 1 start n (lambda (x) x) (lambda (x) (+ x 2))))

;; nchk: Calculates the binomial coefficient:
(define (id x) x)
(define (1+ x) (+ x 1))
(define (nchk n k)
  (/ (accumulate * 1 (+ n (- k) 1) n id 1+)
     (accumulate * 1 1             k id 1+)))

;; nchk*: Calculates the binomial coefficient using only one call to accumulate:
(define (nchk* n k)
  (accumulate * 1 1 k (lambda (i) (/ (+ n 1 (- i)) i)) 1+))

;; 2^: Calculates 2^n using accumulate and nchk:
(define (2^ n) 
  (accumulate + 0 0 n (lambda (i) (nchk n i)) 1+))

;; divisors-sum: Finds the sum of divisors of a given number:
(define (divisors-sum n)
  (define (divisor? x n) (= (remainder n x) 0))
  (accumulate + 0 1 n (lambda (x) (if (divisor? x n) x 0)) 1+))

;; divisors-sum*: ----||----
(define (divisors-sum* n)
  (define (op curr rest)
    (if (= (remainder n curr) 0)
	(+ curr rest)
	rest))
  (accumulate op 0 1 n id 1+))

;; divisors-sum**: ----||-----
(define (divisors-sum** n)
  (filter-accumulate + 0 1 n id 1+ (lambda (i) (= (remainder n i) 0))))

;; count: Count of numbers that hold true for the given predicate in the interval [a, b]:
(define (count p? a b)
  (filter-accumulate-i + 0 a b (lambda (x) 1) 1+ p?))
	      
;; all?: Checks whether the predicate is true for all number in the interval [a, b]:
(define (all p? a b)
  (accumulate (lambda (x y) (and x y)) #t a b p? 1+))

;; any?: Checks whether the predicate is true for any of the numbers in the interval [a, b]:
(define (any? p? a b)
  (accumulate (lambda (x y) (or x y)) #f a b p? 1+))

;; prime?: Check if a number is prime via accumulate:
(define (prime? n)
  (define (divisor? x n) (= (remainder n x) 0))
  (cond ((< n 2) #f)
   (else (accumulate-i (lambda (x y) (and x y)) #t 2 (floor (sqrt n)) (lambda (x) (not (divisor? x n))) 1+))))

;; flip: Returns function with swapped parameters:
(define (flip f) (lambda (x y) (f y x)))

;; complement: Function that gives the negation of a given predicate:
(define (complement p) (lambda (x) (not (p x))))


