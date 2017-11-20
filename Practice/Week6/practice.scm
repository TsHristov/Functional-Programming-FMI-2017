(load "../../Common/common_functions.scm")

;; map*: Map via foldr:
(define (map* f l)
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

;; matrix-min: Find the minimum element of a matrix:
(define (matrix-min matrix)
  (define (min x y) (if (< x y) x y))
  (define (min-el l) (fold-right min (car l) l))
  (min-el (map min-el matrix)))
