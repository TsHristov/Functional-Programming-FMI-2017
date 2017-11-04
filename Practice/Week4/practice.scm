;; Task 1:
;; search: Bisection method to find a root of a continuous function f in
;;         the interval [a, b]:
(define (search f a b)
  (define precision 0.001)
  (let ((c (/ (+ a b) 2.0)))
    (if (or (= (f c) 0) (< (- c a) precision)) c
	(cond
	 ((< (* (f a) (f c)) 0) (search f a c))
	 ((< (* (f b) (f c)) 0) (search f c b))))))

;; Task 2:
;; square-root: Newton`s Iteration method for computing the square root of n:
(define (square-root n)
  (define precision 0.0001)
  (define (reccurence n seed)
    (let ((new-seed (* (+ seed (/ n seed)) 0.5)))
      (if (= (- new-seed (sqrt n)) precision) new-seed
	  (reccurence n (new-seed)))))
   (reccurence n 1))

;; Task 3:
;; Task 4:
;; derivative: Function that calculates the first derivative of a given function f:

;; Task 5:
;; compose: Function that returns the composition of f and g -> (f(g(x)))
(define (compose f g) (lambda (x) (f (g x))))

;; Task 6:
;; double: Function that returns a function that is applied 2 times:
(define (double f) (lambda (x) (f (f x))))

;; Task 7:
;; repeat: Function that return a function f that is applied n times:
(define (repeat f n)
  (lambda (x)
    (if (zero? n) x
	(f ((repeat f (- n 1)) x)))))

