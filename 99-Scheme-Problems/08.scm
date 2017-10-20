;; Eliminate consecutive duplicates of list elements:
;; Example:
;; '(a a a a b c c a a d e e e e) -> '(a b c d e)

(define (compress* l)
  (define (compress** l res)
    (if (not (null? l))
	(if (member (car l) res)
	  (compress** (cdr l) res)
	  (compress** (cdr l) (append res (list (car l)))))
	(append res '())))
  (compress** l '()))

