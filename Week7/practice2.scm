(load "../Common/common_functions.scm")

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define (binary-tree? t)
  (or (empty-tree? t)
      (and (list? t)
	   (= (length t) 3)
	   (binary-tree? (left-tree t))
	   (binary-tree? (right-tree t)))))

;; tree-sum: Find the sum of all nodes in a binary tree:
(define (tree-sum tree)
  (if (empty-tree? tree) 0
      (+ (root-tree tree)
	 (tree-sum (left-tree tree))
	 (tree-sum (right-tree tree)))))

;; tree-max: Finds the max node of a binary tree:
(define (tree-max tree)
  (if (empty-tree? tree)
      (max (root-tree tree)
	   (tree-max (left-tree tree))
	   (tree-max (right-tree tree)))))
  
;; is-BST?: Checks if a tree is a Binary Search Tree:
(define (is-BST? tree)
  (or (empty-tree? tree)
      (and
       (or (empty-tree? (left-tree tree))
	   (all? (lambda (node) (< node (root-tree tree))) (flatten (left-tree tree))))
       (or (empty-tree? (right-tree tree))
	   (all? (lambda (node) (> node (root-tree tree))) (flatten (right-tree tree))))
       (is-BST? (left-tree tree))
       (is-BST? (right-tree tree)))))

;; BST-insert: Insert a given value in a Binary Search Tree:
(define (BST-insert value tree)
  (cond
   ((empty-tree? tree) (make-leaf value))
   ((< value (root-tree tree))
    (make-tree (root-tree tree) (BST-insert value (left-tree tree)) (right-tree tree)))
   ((> value (root-tree tree))
    (make-tree (root-tree tree) (left-tree tree) (BST-insert value (right-tree tree))))
   (else tree)))
