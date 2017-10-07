;; Define a function that checks if
;; a number is even:

(define (even x)
  (=(remainder x 2) 0))

;; Define a function that finds the
;; sum of number in a given range [x,y]

(define (range_sum x y)
  (if (>= x y)
      x
      (+ x (range_sum (+ x 1) y)))) 
