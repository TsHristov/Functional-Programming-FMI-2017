;; member*:
(define (member* x l)
  (if (null? l) #f
      (or (= x (car l)) (member* x (cdr l)))))

;; length*:
(define (length* l)
  (if (null? l) 0
      (+ 1 (length* (cdr l)))))

;; append*:
(define (append* l1 l2)
  (if (null? l2) l1
      (cons (car l1) (car l2) (append* l1 (cdr l2)))))

;; reverse*:
(define (reverse* l)
  (if (null? l) l
      (append (reverse* (cdr l)) (list (car l)))))

;; union*:
;; intersection*:
(define (intersection* l1 l2)
  (define (helper l1 l2 res)
    (if (null? l1) res
      (if (and (not (member* (car l1) res)) (member* (car l1) l2))
	  (helper (cdr l1) l2 (cons (car l1) res))
	  (helper (cdr l1) l2 res))))
  (helper l1 l2 '()))

;; slice* list index1 index2:
;; any? list:
;; all? list:
;; member-deep?: Finds en element x in a nested lists
;; flatten:
;; cartesion-product: Example: (1 2 3) (3 4) -> ((1 3) (1 4) (2 3) (2 4) ... )

