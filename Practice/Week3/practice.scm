;; Task 1:
;; Write a function that computes the following sequence:
;; (1/(1 * 3)) + (1/(5 * 7)) + (1/(9 * 11)) + ...

(define (generic-sum-in-interval term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (generic-sum-interval term (next a) next b))))

(define (member-calculation a) (/ 1 (* a (+ a 2))))
(define (next a) (+ a 4))

(define (sequence-sum a b)
  (generic-sum-interval term a next b))			       
