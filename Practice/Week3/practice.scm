;; Task 1:
;; Function that computes the following sequence:
;; (1/(1 * 3)) + (1/(5 * 7)) + (1/(9 * 11)) + ...

(define (generic-sum-in-interval term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (generic-sum-in-interval term (next a) next b))))

(define (term a) (/ 1 (* a (+ a 2))))
(define (next a) (+ a 4))

(define (sequence-sum a b)
  (generic-sum-in-interval term a next b))			       

;; Task 2:

;; Task 3:
(define (accumulate combiner null-value term a next b))

;; Task 4:

;; Task 5:
;; Function that takes function f as an argument
;; and returns the function with reversed arguments:
(define (flip f)
  (lambda (x y) (f y x)))
