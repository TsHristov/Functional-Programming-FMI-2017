(load "../Common/common_functions.scm")

;; begins-with?:
(define (begins-with? lst1 lst2)
  (cond
   ((null? lst1) #t)
   ((null? lst2) #f)
   (else (and (equal? (car lst1) (car lst2))
	      (begins-with? (cdr lst1) (cdr lst2))))))

;; sublist?:
(define (sublist? lst1 lst2)
  (or (begins-with? lst1 lst2)
      (begins-with? lst1 (cdr lst2))))

;; make-set:
(define (remove value l)
  (filter (lambda (x) (not (equal? x value))) l))

(define (make-set lst)
  (if (null? lst) '()
      (cons (car lst) (make-set (remove (car lst) (cdr lst))))))

;; count: Counts occurences of a given value in a list:
(define (count value lst)
  (apply + (map (lambda (x) (if (= x value) 1 0)) lst)))

;; histogram: Gives the counts of each element in a list:
(define (histogram lst)
  (map (lambda (x) (cons x (count x lst))) (make-set lst)))
