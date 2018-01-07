;; add-vectors:
;; Example: (add-vectors '(1 2 3) '(3 4 5)) -> '(4 6 8)
(define (add-vectors x y) (apply map + (list x y)))

;; vector*scalar: Multiplies vector with a given scalar
;; Example: (vector*scalar '(1 2 3) 2) -> '(2 4 6)
(define (vector*scalar vector scalar) (map (lambda (x) (* x scalar)) vector))

;; choose-row: Chooses the first row from the matrix that has first element != 0
;; Example: (choose-row '((1 5 2) (2 3 8) (-2 0 4))) -> '(1 5 2)
(define (choose-row matrix)
  (car (filter (lambda (row) (not (equal? (car row) 0))) matrix)))

;; find-scalar: Finds a given scalar, such that when the vectors are added the first column becomes 0
;; Example: (find-scalar '(1 2 3) '(4 5 6)) -> -4
(define (find-scalar vector1 vector2) (- 0 (* (car vector1) (car vector2))))

;; row-reduce: Performs Gaussian Elimination over a given square matrix:
;; Example: (row-reduce '((1 5 2) (2 3 8) (-2 0 4))) -> '((1 5 2) (0 -7 4) (0 10 8))
(define (row-reduce matrix)
  (let ((non-zero (choose-row matrix)))
    (map (lambda (row) (if (equal? row non-zero) non-zero
			 (add-vectors (vector*scalar non-zero (find-scalar row non-zero)) row)))
	 matrix)))
  

