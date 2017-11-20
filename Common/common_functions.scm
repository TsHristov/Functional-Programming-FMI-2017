;; accumulate-right:
(define (accumulate-right op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate-right op nv (next a) b term next))))

;; accumulate-left:
(define (accumulate-left op nv a b term next)
  (if (> a b) nv
      (accumulate-left op (op nv (term a)) (next a) b term next)))

;; fold-right:
(define (fold-right op nv l)
  (if (null? l) nv
      (op (car l) (fold-right op nv (cdr l)))))

;; fold-left:
(define (fold-left op nv l)
  (if (null? l) nv
      (fold-left op (op nv (car l)) (cdr l))))

;; map:
(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))

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
