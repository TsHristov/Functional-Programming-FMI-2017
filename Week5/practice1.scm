(load "../Week4/practice1.scm")

;; count-atoms: Counts the atoms in a nested list structure:
(define (count-atoms l)
  (cond
   ((null? l) 0)
   ((list? (car l)) (+ (count-atoms (car l)) (count-atoms (cdr l))))
   (else (+ 1 (count-atoms (cdr l))))))

;; member-deep?: Check if an element x is a member of a nested list structure,
;;               with boolean operations only:
(define (member-deep? x l)
  (and (not (null? l))
       (or (equal? x l)
	   (member-deep? x (car l))
	   (and (pair? l) (member-deep? x (cdr l))))))

;; get-at: Get element at position [row, col] from a given matrix:
(define (get-at matrix row col)
  (define (get l i)
    (if (= i 0)
	(car l)
	(get (cdr l) (- i 1))))
  (get (get matrix row) col))

;; transpose: Transpose a given matrix:

;; same-rows?: Checks if there are two same rows in a matrix
(define (same-rows? matrix)
  (if (null? matrix) #f
      (and (or  (member (car matrix) (cdr matrix))
	        (same-rows? (cdr matrix))) #t)))
