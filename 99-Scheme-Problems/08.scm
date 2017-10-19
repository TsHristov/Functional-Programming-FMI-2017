;; Eliminate consecutive duplicates of list elements:
;; Example:
;; '(a a a a b c c a a d e e e e) -> '(a b c d e)

(define (compress* l)
  (define (compress2* l res)
    (if (not (null? l))
	(case (car l)
	  ((res) (compress2* (cdr l) res))
	  (else  (compress2* (cdr l) (append res (car l)))))
	res))
  (compress2* l '()))
