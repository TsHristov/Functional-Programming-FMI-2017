(load "../../Common/common_functions.scm")

;; map**: Map via foldr:
(define (map** f l)
  (fold-right (lambda (x nv) (cons (f x) nv)) '() l))

;; reverse*: Reverse via foldr:
(define (reverse* l)
  (fold-right (lambda (x y) (append y (list x))) '() l))

;; reverse**: Reverse via foldl:
(define (reverse** l)
  (fold-left (lambda (x y) (cons y x)) '() l))

;; matrix-sum: Finds the sum of all elements in a matrix:
(define (sum l) (apply + l))
(define (matrix-sum matrix)
  (sum (map (lambda (x) (sum x)) matrix)))

;; matrix-min: Finds the minimum element of a matrix:
(define (matrix-min matrix)
  (define (min x y) (if (< x y) x y))
  (define (min-el l) (fold-right min (car l) l))
  (min-el (map min-el matrix)))

;; get-nth: Takes the nth element of a list:
(define (get-nth n l)
  ;; take: Take the first n elements of a list:
  (define (take n l)
    (if (or (= n 0) (null? l)) '()
	(cons (car l) (take (- n 1) l))))
  ;; drop: Drop the first n elements of a list:
  (define (drop n l)
    (if (or (= n 0) (null? l)) l
	(drop (- n 1) (cdr l))))
  (take 1 (drop (- n 1) l)))

;; transpose: Transpose a given matrix:
(define (transpose matrix) (apply map list matrix))

;; nth-column: Gets the nth column of a given matrix:
(define (nth-column nth matrix)
  (get-nth nth (transpose matrix)))


;; range: Generates a range list [a,b]:
(define (range a b)
  (accumulate-right cons '() a b (lambda (x) x) (lambda (x) (+ x 1))))

;; flatten: Flattens a nested list structure:
(define (flatten l) (deep-fold '() list append l))

;; get-main-diagonal: Gets the main diagonal of a matrix:
(define (get-main-diagonal matrix)
  (let ((indices (range 1 (length (car matrix)))))
    (flatten (map (lambda (row) (get-nth (car row) (cdr row)))
		  (apply map list indices matrix)))))

;; get-anti-diagonal: Gets the anti-diagonal of a matrix:
(define (get-anti-diagonal matrix)
  (let ((indices (reverse (range 1 (length (car matrix))))))
    (flatten (map (lambda (row) (get-nth (car row) (cdr row)))
		  (apply map list indices matrix)))))

