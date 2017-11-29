(load "../Common/common_functions.scm")

(define sample-tree '(1 (2 () ()) (3 (4 () ()) (5 () ()))))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define (tree? t)
  (or (empty-tree? t)
      (and (list? t)
	   (= (length t) 3)
	   (tree? (left-tree t))
	   (tree? (right-tree t)))))

;; tree-sum:
(define (tree-sum tree)
  (if (empty-tree? tree) 0
      (+ (root-tree tree)
	 (tree-sum (left-tree tree))
	 (tree-sum (right-tree tree)))))

;; tree-max:
(define (tree-max tree)
  (if (empty-tree? tree)
      (max (root-tree tree)
	   (tree-max (left-tree tree))
	   (tree-max (right-tree tree)))))

;; tree-level:
(define (tree-level k t)
  (cond
   ((empty-tree? t) '())
   ((= k 0) (list (root-tree t)))
   (else (append (tree-level (- k 1) (left-tree t))
		 (tree-level (- k 1) (right-tree t))))))
