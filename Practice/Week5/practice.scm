;; member?: Check if element is a member of a list:
;; Example:
;;     (member? 2 (list 1 3 4)) -> #f
;;     (member? 2 (list 3 3 2)) -> #t
(define (member? x l)
  (if (null? l) #f
      (or (= x (car l)) (member? x (cdr l)))))

;; length*: Find the length of a list:
;; Example:
;;     (length* (list 1 2 3)) -> 3
;;     (length* (list)) -> 0
(define (length* l)
  (if (null? l) 0
      (+ 1 (length* (cdr l)))))

;; append*: Append two lists:
;; Example:
;;     (append* (list 2 3) (list 3 4)) -> (2 3 3 4)
(define (append* l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append* (cdr l1) l2))))

;; reverse*: Reverse a list:
;; Example:
;;     (reverse* (list 1 2 3)) -> (3 2 1)
(define (reverse* l)
  (if (null? l) l
      (append (reverse* (cdr l)) (list (car l)))))

;; union*: Find the union of two lists:
;; Example:
;;     (union* (list 1 2 3) (list 2 2 2 4)) -> (1 2 3 4)
;;     (union* '() (list 2 3 4)) -> (2 3 4)
(define (union* l1 l2)
  (define appended (append l1 l2))
  (define (unify appended res)
    (cond
     ((null? appended) res)
     ((not (member? (car appended) res))
      (unify (cdr appended) (append res (list (car appended)))))
     (else (unify (cdr appended) res))))
  (unify appended '()))

;; intersection*: Find the intersection of two lists:
;; Example:
;;     (intersection* (list 1 2 3) (list 2 4 5)) -> (2)
;;     (intersection* (list 1 2 3) (list 4 5 6)) -> ()
(define (intersection* l1 l2)
  (define (intersect l1 l2 res)
    (if (null? l1) res
      (if (and (not (member? (car l1) res)) (member? (car l1) l2))
	  (intersect (cdr l1) l2 (cons (car l1) res))
	  (intersect (cdr l1) l2 res))))
  (intersect l1 l2 '()))

;; slice*: Slice a given list from index i to index j:
;; Example:
;;     (slice* (list 1 2 3 4) 0 3) -> (1 2 3 4)
;;     (slice* (list 1 2 3) 1 1) -> (2)
(define (slice* l i j)
  (if (> i j) '())
  (define (slice l n)
    (cond
     ((null? l) l)
     ((< n i) (slice (cdr l) (+ n 1)))
     ((<= n j) (cons (car l) (slice (cdr l) (+ n 1))))
     ((> n j) '())))
  (slice l 0))

;; any?: Returns #t if the predicate p? applies to any of the elements of the list l:
;; Example:
;;     (any? even? (list 1 2 3)) -> #t
;;     (any? odd? (list 2 4)) -> #f
(define (accumulate l f initial)
  (if (null? l) initial
      (accumulate (cdr l) f (f initial (car l)))))

(define (any? p? l)
  (accumulate l (lambda (initial x) (or (p? x) initial)) #f))

;; all?: Returns #t if the predicate p? applies to all of the elements of the list l:
;; Example:
;;     (all? integer? (list 1 2 3)) -> #t
;;     (all? even? (list 1 2 3)) -> #f
(define (all? p? l)
  (accumulate l (lambda (initial x) (and (p? x) initial)) #t))

;; member-deep?: Finds an element x in a nested list structure:
;; Example:
;;     (member-deep? 2 '((1 (3)) ((1 (1 (1 (2))))))) -> #t
(define (atom? l) (and (not (null? l)) (not (pair? l))))
(define (member-deep? x l)
  (cond
   ((null? l) #f)
   ((atom? l) (equal? x l))
   (else (or (member-deep? x (car l))
	     (member-deep? x (cdr l))))))

;; flatten: Flattens a nested lists:
;; Example:
;;     (flatten '((1 (2) (3)) (((((4))))))) -> (1 2 3 4)
(define (flatten l)
  (cond
   ((null? l) '())
   ((atom? l) (list l))
   (else (append (flatten (car l)) (flatten (cdr l))))))

;; cartesion-product: Returns the cartesian-product of two lists:
;; Example:
;;     (cartesian-product (list 1 2 3) (list 3 4)) -> ((1 3) (1 4) (2 3) (2 4) (3 3) (3 4))
(define (cartesian-product l1 l2)
  (define l2-copy l2)
  (define (helper l1 l2-copy)
    (cond
     ((null? l1) '())
     ((null? l2-copy) (helper (cdr l1) l2))
     (else (append (list (list (car l1) (car l2-copy)))
		   (helper l1 (cdr l2-copy))))))
  (helper l1 l2-copy))

