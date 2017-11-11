(load "../Week3/practice.scm")

;; accumulate: List accumulator
(define (accumulate l function initial-value)
  (if (null? l) initial-value
      (accumulate (cdr l) function (function initial-value (car l)))))

;; min-el: Find the minimum element of a list:

(define (min x y) (if (< x y) x y))
(define (min-el l)
  (cond
   ((null? l) #f)
   ((null? (cdr l)) (car l))
   (else (min (car l) (min-el (cdr l))))))

;; min-el*: Find the minimum element of a list using accumulator:
(define (min-el* l)
  (if (null? l) #f
      (accumulate l (lambda (x y) (if (< x y) x y)) (car l))))

;; min-el**: Find the minimum element of a list via foldr:
(define (foldr* op nv l)
  (if (null? l) nv
      (op (car l) (foldr* op nv (cdr l)))))

(define (min-el** l) (foldr* min (car l) l))


;; map*: Maps a function over each element of a list:
(define (map* f l)
  (if (null? l) '()
      (cons (f (car l)) (map* f (cdr l)))))

;; filter*: Filters a given list according to a predicate:
(define (filter* l p?)
  (if (null? l) l
      (if (p? (car l))
	  (cons (car l) (filter* (cdr l) p?))
	  (filter* (cdr l) p?))))

;; zip*: Applies f to each two elements of two lists with equal length:
(define (zip* l1 l2 f)
  (if (null? l1) l1
      (cons (f (car l1) (car l2)) (zip* (cdr l1) (cdr l2) f))))

;; zip**: Zip with accumulate and lambda:
(define (zip** l1 l2 f)
  (accumulate (cons l1 l2) '()
       (lambda (result l-pair) (cons result (f (car l-pair) (cadr l-pair))))
       (accumulate (cons (cdar l1) (cddr 2)))))

;; remove-first: Removes the first occurence of an element x in l:
(define (remove-first x l)
  (if (null? l) l
      (if (= (car l) x) (cdr l)
	  (cons (car l) (remove-first x (cdr l))))))

;; remove-first*: Remove first occurence of an element x with filter:


;; remove-all: Remove all occurences of an element x in the list:
(define (remove-all x l)
  (filter* l (lambda (y) (not (= x y)))))

;; find: Find an element x in the list l:
(define (find x l)
  (accumulate l (lambda (initial-value list-element) (or initial-value (= list-element x))) #f))

;; remove-min: Remove the minimum element in the list l:
(define (remove-min l)
  (remove-first (min-el** l) l))

;; reverse*: Reverse a list using accumulator:
(define (reverse* l)
    (accumulate l (lambda (a b) (cons b a)) '()))

;; reverse**: Reverse a list using foldl:
(define (foldl* op nv l)
  (if (null? l) nv
      (foldl* op (op nv (car l)) (cdr l))))

(define (reverse** l)
  (foldl* (lambda (a b) (cons b a)) '() l))

;; selection-sort: Selection sort implementation:
(define (selection-sort l)
  (if (null? l) l
      (cons (min-el l) (selection-sort (remove-min l)))))

;; q-sort: Quick sort implementation:
(define (q-sort l)
  (if (null? l) l
      (append (q-sort (filter* l (lambda (x) (< x (car l)))))
	      (list (car l))
	      (q-sort (filter* (cdr l) (lambda (x) (not (< x (car l)))))))))

;; merge-sort: Merge sort implementation:
;; FIX IT!
(define (merge l1 l2)
  (cond
   ((null? l1) l2)
   ((null? l2) l1)
   ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
   (else (cons (car l2) (merge l1 (cdr l2))))))

(define (merge-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (merge-sort (merge (take-n (/ (length l) 2) l) (drop-n (/ (length l) 2) l)))))
