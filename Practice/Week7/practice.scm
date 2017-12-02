(load "../../Common/common_functions.scm")

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define (children node) (cdr node))
(define (is-leaf? node) (null? (children node)))

(define (binary-tree? t)
  (or (empty-tree? t)
      (and (list? t)
	   (= (length t) 3)
	   (binary-tree? (left-tree t))
	   (binary-tree? (right-tree t)))))

;; inorder: Traverses a Binary Tree inorder:
(define (inorder tree)
  (if (empty-tree? tree) empty-tree
      (append (inorder (left-tree tree))
	      (list (root-tree tree))
	      (inorder (right-tree tree)))))

;; preorder: Traverses a Binary Tree preorder:
(define (preorder tree)
  (if (empty-tree? tree) empty-tree
      (append (list (root-tree tree))
	      (preorder (left-tree tree))
	      (preorder (right-tree tree)))))

;; postorder: Traverses a Binary Tree postorder:
(define (postorder tree)
  (if (empty-tree? tree) empty-tree
      (append (postorder (left-tree tree))
	      (postorder (right-tree tree))
	      (list (root-tree tree)))))

;; levels-nodes: Returns all nodes at a given level in the tree:
(define (level-nodes level tree)
  (if (empty-tree? tree) empty-tree
      (if (= level 1) (list (root-tree tree))
	  (append (level-nodes (- level 1) (left-tree tree))
		  (level-nodes (- level 1) (right-tree tree))))))

;; map-tree: Maps a given function to all nodes of a binary tree:
(define (map-tree f tree)
  (if (empty-tree? tree) empty-tree
      (make-tree (f (root-tree tree))
		 (map-tree f (left-tree tree))
		 (map-tree f (right-tree tree)))))

;; symmetric?: Checks if a Binary Tree is symmetric:
(define (symmetric? tree)
  (define (symmetric-subtrees? left right)
    (or (and (empty-tree? left) (empty-tree? right))
	(and (equal? (root-tree left) (root-tree right))
	     (symmetric-subtrees? (left-tree left) (right-tree right))
	     (symmetric-subtrees? (right-tree left) (left-tree right)))))
  (or (empty-tree? tree) (symmetric-subtrees? (left-tree tree) (right-tree tree))))

;; BST-construct: Construct a Binary Search Tree from a sorted list:
(define (middle l)
  (let ((length/2 (quotient (length l) 2)))
    (car (get-nth (+ length/2 1) l))))

(define (BST-construct l)
  (if (null? l) l
      (make-tree (middle l)
		 (BST-construct (filter (lambda (node) (< node (middle l))) l))
		 (BST-construct (filter (lambda (node) (> node (middle l))) l)))))
