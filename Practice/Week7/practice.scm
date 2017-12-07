(load "../../Common/common_functions.scm")

(define root car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define (leaf? node) (and (empty-tree? (left-tree node))
			     (empty-tree? (right-tree node))))
(define (children node)
  (if (leaf? node) '()
      (cdr node)))

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
	      (list (root tree))
	      (inorder (right-tree tree)))))

;; preorder: Traverses a Binary Tree preorder:
(define (preorder tree)
  (if (empty-tree? tree) empty-tree
      (append (list (root tree))
	      (preorder (left-tree tree))
	      (preorder (right-tree tree)))))

;; postorder: Traverses a Binary Tree postorder:
(define (postorder tree)
  (if (empty-tree? tree) empty-tree
      (append (postorder (left-tree tree))
	      (postorder (right-tree tree))
	      (list (root tree)))))

;; levels-nodes: Returns all nodes at a given level in the tree:
(define (level-nodes level tree)
  (if (empty-tree? tree) empty-tree
      (if (= level 1) (list (root tree))
	  (append (level-nodes (- level 1) (left-tree tree))
		  (level-nodes (- level 1) (right-tree tree))))))

;; map-tree: Maps a given function to all nodes of a binary tree:
(define (map-tree f tree)
  (if (empty-tree? tree) empty-tree
      (make-tree (f (root tree))
		 (map-tree f (left-tree tree))
		 (map-tree f (right-tree tree)))))

;; path-exists?: Checks if a given path exists in a Binary Tree:
(define (path-exists? path tree)
  (or (and (null? path) (leaf? tree))
      (and (pair? path)
	   (not (empty-tree? tree))
           (equal? (car path) (root tree))
	   (or (path-exists? (cdr path) (left-tree tree))
	       (path-exists? (cdr path) (right-tree tree))))))

;; all-paths: Find all paths in a given Binary Tree:
(define (all-paths tree)
  (if (empty-tree? tree) tree
      (list (append (list (root tree)) (all-paths (left-tree tree)))
	    (append (list (root tree)) (all-paths (right-tree tree))))))

;; symmetric?: Checks if a Binary Tree is symmetric:
(define (symmetric? tree)
  (define (symmetric-subtrees? left right)
    (or (and (empty-tree? left) (empty-tree? right))
	(and (equal? (root left) (root right))
	     (symmetric-subtrees? (left-tree left) (right-tree right))
	     (symmetric-subtrees? (right-tree left) (left-tree right)))))
  (or (empty-tree? tree) (symmetric-subtrees? (left-tree tree) (right-tree tree))))

;; BST-construct: Construct a Binary Search Tree from a sorted list:
(define (BST-construct lst)
  (let ((half (quotient (length lst) 2)))
    (if (null? lst) lst
	(make-tree (list-ref lst half)
		   (BST-construct (take half lst))
		   (BST-construct (drop (+ half 1) lst))))))
