(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (leaf? tree) (and (not (empty-tree? (root-tree tree)))
			  (empty-tree? (left-tree tree))
			  (empty-tree? (right-tree tree))))

;; reduce-tree:
(define (reduce-tree proc null-value tree)
  (cond
   ((empty-tree? tree) null-value)
   ((leaf? tree) (root-tree tree))
   (else
    (proc (reduce-tree proc null-value (left-tree tree))
	  (reduce-tree proc null-value (right-tree tree))))))

;; sum-leaves:
(define (sum-leaves tree) (reduce-tree + 0 tree))
