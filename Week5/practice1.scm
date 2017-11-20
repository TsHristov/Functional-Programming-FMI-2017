(load "../Week4/practice1.scm")

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldr op (op nv (car l)) (cdr l))))

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
	   (and (pair? l)
		(or (member-deep? x (car l))
		    (member-deep? x (cdr l)))))))


;; get: Get element at a given position from a list:
(define (get l n)
  (if (= n 0) (car l)
      (get (cdr l) (- n 1))))

;; get-at: Get element at position [row, col] from a given matrix:
(define (get-at matrix row col)
  (get (get matrix row) col))

;; transpose: Transpose a given matrix:
(define (transpose matrix) (apply map list matrix))

;; same-rows?: Checks if there are two same rows in a matrix
(define (same-rows? matrix)
  (> (length (filter matrix (lambda (x) (member x (cdr matrix))))) 1))

;; sum: Returns sum of elements in a list via foldr:
(define (sum l) (apply + l))

;; max: Finds the maximum of two numbers:
(define (max x y) (if (> x y) x y))

;; max-el: Finds the maximum element of a list via foldr:
(define (max-el l) (foldr max (car l) l))

;; max-row: Find the row in a column with the largest sum
(define (max-row matrix)
  (let ((max-sum (max-el (map (lambda (x) (sum x)) matrix))))
    (filter (map (lambda (x) (cons x (list (sum x)))) matrix)
	    (lambda (x) (and (= (cadr x) max-sum) x)))))
