(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (filter* l p?)
  (cond
   ((null? l) l)
   ((p? (car l)) (cons (car l) (filter* (cdr l) p?)))
   (else (filter* (cdr l) p?))))

;; take: Take the first n elements of a given list:
(define (take n lst)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;; drop: Drop the first n elements of a given list:
(define (drop n lst)
  (if (= n 0) lst
      (drop (- n 1) (cdr lst))))

;; nth: Takes the n-th element of a given list:
(define (nth n l)
  (take 1 (drop (- n 1) l)))

;; range: Returns list of the numbers in the interval [from; to]:
(define (range from to)
  (accumulate cons '() from to (lambda (x) x) (lambda (x) (+ x 1))))

;; digit-list: Returns the digits of a given number:
(define (digit-list n)
  (define (digits n)
   (cond
     ((= n 0) '())
     (else (cons (remainder n 10) (digits (quotient n 10))))))
  (reverse* (digits n)))

;; zip: Zips two lists:
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

;; zip-with: Zips two lists with applied function:
(define (zip-with f l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (f (car l1) (car l2)) (zip-with f (cdr l1) (cdr l2)))))

;; sorted: Checks if the array is in ascending order:
(define (sorted l)
  (if (or (null? l) (null? (cdr l))) #t
      (and (<= (car l) (cadr l)) (sorted (cdr l)))))

;; uniques: Returns the unique elements of a list:
(define (uniques l)
  (cond
   ((null? l) '())
   ((not (member (car l) (cdr l))) (cons (car l) (uniques (cdr l))))
   (else (uniques (cdr l)))))

;; extract-ints: Returns list of ints from a given list with arbitrary objects:
(define (extract-ints l)
  (filter* l (lambda (x) (integer? x))))

;; insert-at: Inserts a value in at a given position in the list:
(define (insert-at n value l)
  (append (take n l) (list value) (drop n l)))

;; insert-in-sorted: Inserts a value in it`s proper position in sorted array
(define (insert-in-sorted val l)
  (append (filter* l (lambda (x) (< x val)))
	  (list val)
	  (filter* l (lambda (x) (>= x val)))))
