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
;; continued-fraction: Calculates continued fraction up to the kth:
(define (continued-fraction n d k)
  (if (= k 0) 0
      (/ n (+ d (continued-fraction n d (- k 1))))))

;; Task 4:
;; first-derivative: Calculates the first derivative of a function f:
(define (first-derivative f x)
  (define precision 0.001)
  (/ (- (f (+ x precision)) (f x)) precision))

;; Task 5:
;; compose: Returns the composition of functions f and g:
(define (compose f g) (lambda (x) (f (g x))))

;; Task 6:
;; twice: Returns a function that is applied 2 times:
(define (twice f) (lambda (x) (f (f x))))

;; Task 7:
;; repeat: Returns a function that is applied n times:
(define (apply-n-times f n)
  (lambda (x)
    (if (zero? n) x
	(f ((apply-n-times f (- n 1)) x)))))
