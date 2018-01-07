;; from-to: Generate list of numbers from a to b
(define (from-to a b)
  (if (>= a b) '()
      (cons a (from-to (+ a 1) b))))

;; transpose:
(define (transpose matrix) (apply map list matrix))

;; row-indices: Generate row indices range
;; Example: (row-indices '((1 2 3 4) (5 6 7 8) (9 0 1 2))) -> '(0 1 2)
(define (row-indices matrix) (from-to 0 (length matrix)))

;; column-indices: Generate column indices range
;; Example: (column-indices '((1 2 3 4) (5 6 7 8) (9 0 1 2))) -> '(0 1 2 3)
(define (column-indices matrix) (from-to 0 (length (transpose matrix))))

;; all-variants: Generate key-value pairs of indices of the possible ways to slice a given matrix
;; Example: (all-variants '((1 2 3 4) (5 6 7 8) (9 0 1 2))) ->
;; '((0 . 0) (0 . 1) (0 . 2) (0 . 3) (1 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 0) (2 . 1) (2 . 2) (2 . 3))
(define (all-variants matrix)
  (apply append (map (lambda (x)
		       (map (lambda (y) (cons x y)) (column-indices matrix)))
		     (row-indices matrix))))

;; remove-row: Remove a concrete row from a given matrix
;; Example: (remove-row 0 '((1 2 3 4) (5 6 7 8) (9 0 1 2))) -> '((5 6 7 8) (9 0 1 2))
(define (remove-row n matrix)
  (map car (filter (lambda (row) (not (= (cadr row) n))) (zip matrix (row-indices matrix)))))

;; remove-column: Remove a concrete column from a given matrix:
;; Example: (remove-column 0 '((1 2 3 4) (5 6 7 8) (9 0 1 2))) -> '((2 3 4) (6 7 8) (0 1 2))
(define (remove-column n matrix)
  (transpose (remove-row n (transpose matrix))))

;; cross-out: Generate all possible matrices, obtained via cross-out of a given row and given column:
(define (cross-out matrix)
  (map (lambda (variant) (remove-row (car variant) (remove-column (cdr variant) matrix))) (all-variants matrix)))
