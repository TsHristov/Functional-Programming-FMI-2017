;; member*: Check if element is a member of a list:
(define (member* x l)
  (if (null? l) #f
      (or (= x (car l)) (member* x (cdr l)))))

;; length*: Find the length of a list:
(define (length* l)
  (if (null? l) 0
      (+ 1 (length* (cdr l)))))

;; append*: Append two lists:
(define (append* l1 l2)
  (if (null? l2) l1
      (cons (car l1) (car l2) (append* l1 (cdr l2)))))

;; reverse*: Reverse a list:
(define (reverse* l)
  (if (null? l) l
      (append (reverse* (cdr l)) (list (car l)))))

;; union*: Find the union of two lists:
(define (union* l1 l2)
  (define appended (append l1 l2))
  (define (unify appended res)
    (cond
     ((null? appended) res)
     ((not (member* (car appended) res))
      (unify (cdr appended) (append res (list (car appended)))))
     (else (unify (cdr appended) res))))
  (unify appended '()))cons

;; intersection*: Find the intersection of two lists:
(define (intersection* l1 l2)
  (define (helper l1 l2 res)
    (if (null? l1) res
      (if (and (not (member* (car l1) res)) (member* (car l1) l2))
	  (helper (cdr l1) l2 (cons (car l1) res))
	  (helper (cdr l1) l2 res))))
  (helper l1 l2 '()))

;; slice*: Slice a given list from index i to index j:
(define (slice* l i j)
  (if (> i j) '())
  (define (slice l n)
    (cond
     ((null? l) l)
     ((< n i) (slice (cdr l) (+ n 1)))
     ((<= n j) (cons (car l) (slice (cdr l) (+ n 1))))
     ((> n j) '())))
  (slice l 0))

;; any?:
;; all?:
;; member-deep?: Finds en element x in a nested lists
;; flatten: Flattens a nested lists:
;; cartesion-product: Example: (1 2 3) (3 4) -> ((1 3) (1 4) (2 3) (2 4) ... )

