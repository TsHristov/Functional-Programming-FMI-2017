;; Task 1:
;; Write a function that computes the following sequence:
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
;; Write a function that computes the following sequence:
;; (2 * 4 * 4 * 6 * 6 * 8...) / (3 * 3 * 5 * 5 * 7 * 7...)


