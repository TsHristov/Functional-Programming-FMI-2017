;; Define a function that checks if
;; a number is even:

(define (even x)
  (= (remainder x 2) 0))

;; Define a function that accepts three arguments
;; and returns the sum of the squares of the larger two:

(define (square x) ( * x x))

(define (sum_squares x y)
  (+ (square x) (square y)))

(define (smaller x y z)
  (and (< x y) (< x z)))

(define (biggest_squares_sum x y z)
  (cond
   ((smaller x y z) (sum_squares y z))
   ((smaller y x z) (sum_squares x z))
   (else (sum_squares x y))))
     
;; Define a function that finds the
;; sum of number in a given range [x,y]:

(define (range_sum x y)
  (if (>= x y)
      x
      (+ x (range_sum (+ x 1) y)))) 
