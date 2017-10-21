;; Eliminate consecutive duplicates of list elements:
;; Example:
;; '(a a a a b c c a a d e e e e) -> '(a b c a d e)

(define (compress* l)
  (define (compress** l result)
    (if (not (null? l))
	(if (and (pair? result) (equal? (car l) (last result)))
	  (compress** (cdr l) result)
	  (compress** (cdr l) (append result (list (car l)))))
	(append result l)))
  (compress** l '()))

