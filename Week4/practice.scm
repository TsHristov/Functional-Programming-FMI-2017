(load "../Week3/practice.scm")

;; list-accumulate: List accumulator
(define (list-accumulate l function initial-value)
  (if (null? l) initial-value
      (list-accumulate (cdr l) function (function initial-value (car l)))))

;; min-el: Find the minimum element of a list:
(define (min x y) (if (< x y) x y))
(define (min-el l)
  (cond
   ((null? l) #f)
   ((null? (cdr l)) (car l))
   (else (min (car l) (min-el (cdr l))))))

;; min-el-acc: Find the minimum element of a list using accumulator:
(define (min-el-acc l)
  (if (null? l) #f
      (list-accumulate l (lambda (x y) (if (< x y) x y)) (car l))))

;; map*: Maps a function to each element of a list:
(define (map* f l)
  (if (null? l) '()
      (cons (f (car l)) (map* f (cdr l)))))

;; filter*: Filters a given list according to a predicate:
(define (filter* l pred?)
  (if (null? l) l
      (if (pred? (car l))
	  (cons (car l) (filter*-m (cdr l) pred?))
	  (filter* (cdr l) pred?))))

;; zip*: Applies f to each two elements of two lists with equal length:
(define (zip* l1 l2 f)
  (if (null? l1) l1
      (cons (f (car l1) (car l2)) (zip* (cdr l1) (cdr l2)))))

;; map-zip: Map using zip:
(define (map-zip l1 l2 f)
  (if (null? l1) l1
      (map-zip (zip* l1 l2 cons) (lambda (x) (f (car x) (cdr x))))))

;; zip**: Zip with accumulate and lambda:
(define (zip** l1 l2 f)
  (list-accumulate (cons l1 l2) '()
       (lambda (result l-pair) (cons result (f (car l-pair) (cadr l-pair))))
       (list-accumulate (cons (cdar l1) (cddr 2)))))

;; remove-first: Removes the first occurence of an element x in l:
(define (remove-first l x)
  (if (null? l) l
      (if (= (car l) x) (cdr l)
	  (cons (car l) (remove-first (cdr l) x)))))

;; remove-first-f: Remove first occurence of an element x with filter:
(define (remove-first-f l x)
  (filter l (lambda (a)
	      ((letrec ((searching (and searching (= x a))))
	       (not searching))))))

;; remove-all: Remove all occurences of an element x in the list:
(define (remove-all l x)
  (filter l (lambda (y) (not (= x y)))))

;; find: Find an element x in the list l:
(define ((find l x))
  (list-accumulate l (lambda (val elem) (or val (= elem x))) #f))

;; remove-min: Remove the minimum element in the list l:
(define (remove-min l)
  (remove-first (min-el l) l))

;; reverse*: Reverse a list using accumulator:
(define (reverse* l)
  (list-accumulate l (lambda (a b) (cons b a)) '()))

;; selection-sort: Selection sort implementation:
(define (selection-sort l)
  (if (null? l) l
      (cons (min-el l) (selection-sort (remove-min l)))))

;; q-sort: Quick sort implementation:
(define (q-sort l)
  (if (null? l) l
      (append q-sort (fitler l (lambda (x) (< x (car l))))
	      (list (car l))
	      (q-sort (filter (cdr l) (lambda (x) (not (< x (car l)))))))))

;; merge-sort: Merge sort implementation:
(define (merge l1 l2)
  (cond
   ((null? l1) l2)
   ((null? l2) l1)
   ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
   (else (cons (car l2) (merge l1 (cdr l2))))))

(define (merge-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (merge (merge-sort (take-n l (/ (len l) 2)) (drop-n l (/ (len l) 2))))))
