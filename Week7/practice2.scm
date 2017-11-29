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

;; tree-level: Returns list of all nodes at a given level in a binary tree:
(define (tree-level k t)
  (cond
   ((empty-tree? t) '())
   ((= k 0) (list (root-tree t)))
   (else (append (tree-level (- k 1) (left-tree t))
		 (tree-level (- k 1) (right-tree t))))))

;; tree-map: Maps a given function to all nodes of a binary tree:
(define (tree-map f tree)
  (if (empty-tree? tree) '()
      (make-tree (f (root-tree tree))
		 (tree-map f (left-tree tree))
		 (tree-map f (right-tree tree)))))

;; inorder: Traverses a binary tree inorder:
(define (inorder tree)
  (if (empty-tree? tree) '()
      (append (inorder (left-tree tree))
	      (list (root-tree tree))
	      (inorder (right-tree tree)))))

;; is-BST?: Checks if a tree is a Binary Search Tree:
(define (is-BST? tree)
  (or (empty-tree? tree)
      (and
       (or (empty-tree? (left-tree tree))
  	   (> (root-tree tree) (root-tree (left-tree tree))))
       (or (empty-tree? (right-tree tree))
  	   (< (root-tree tree) (root-tree (right-tree tree))))
       (is-BST? (left-tree tree))
       (is-BST? (right-tree tree)))))
