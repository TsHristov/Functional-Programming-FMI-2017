;; take:
(define (take n l)
  (if (or (= n 0) (null? l)) '()
      (cons (car l) (take (- n 1) (cdr l)))))

;; drop:
(define (drop n l)
  (if (= n 0) l
      (drop (- n 1) (cdr l))))

;; uniques:
(define (uniques lst)
  (cond
   ((null? lst) '())
   ((not (member (car lst) (cdr lst))) (cons (car lst) (uniques (cdr lst))))
   (else (uniques (cdr lst)))))

;; get-nth:
(define (get-nth n l) (car (drop (- n 1) l)))

;; accumulate-right:
(define (accumulate-right op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate-right op nv (next a) b term next))))

;; filter-accumulate-right:
(define (filter-accumulate-right op nv a b term next p?)
  (cond
   ((> a b) nv)
   ((p? a) (op (term a) (filter-accumulate-right op nv (next a) b term next p?)))
   (else (filter-accumulate-right op nv (next a) b term next p?))))

;; accumulate-left:
(define (accumulate-left op nv a b term next)
  (if (> a b) nv
      (accumulate-left op (op nv (term a)) (next a) b term next)))

;; filter-accumulate-left:
(define (filter-accumulate-left op nv a b term next p?)
  (cond
   ((> a b) nv)
   ((p? a) (filter-accumulate-left op (op nv (term a)) (next a) b term next p?))
   (else (filter-accumulate-left op nv (next a) b term next p?))))

;; fold-right:
(define (fold-right op nv l)
  (if (null? l) nv
      (op (car l) (fold-right op nv (cdr l)))))

;; fold-left:
(define (fold-left op nv l)
  (if (null? l) nv
      (fold-left op (op nv (car l)) (cdr l))))

;; any?:
(define (any? p? l)
  (fold-right (lambda (x y) (or (p? x) y)) #f l))

;; map:
(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map* f (cdr l)))))

;; filter:
(define (filter p? l)
  (cond
   ((null? l) l)
   ((p? (car l)) (cons (car l) (filter p? (cdr l))))
   (else (filter p? (cdr l)))))

;; deep-fold:
(define (deep-fold nv term op l)
  (define (atom? l) (and (not (pair? l)) (not (null? l))))
  (cond
   ((null? l) nv)
   ((atom? l) (term l))
   (else (op (deep-fold nv term op (car l))
	     (deep-fold nv term op (cdr l))))))

;; flatten:
(define (flatten l) (deep-fold '() list append l))

;; search:
(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

;; any?:
(define any? search)

;; all?:
(define (all? p l)
  (not (search (lambda (x) (not (p x)))) l))
