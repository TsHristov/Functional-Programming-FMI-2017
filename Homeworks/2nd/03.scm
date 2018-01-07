;; Utility functions for working with Binary Trees:
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define (leaf? tree) (and (not (empty-tree? (root-tree tree)))
			  (empty-tree? (left-tree tree))
			  (empty-tree? (right-tree tree))))

;; operator?:
(define (operator? x) (not (not (member x '(+ - * / expt)))))

;; constant?:
(define (constant? x) (number? x))

;; variable?:
(define (variable? x) (and (not (operator? x)) (not (constant? x))))

;; expr->tree: Constructs a Binary Expression Tree from a given expression:
;; Example: (expr->tree '(+ x x)) -> '(+ (x () ()) (x () ()))
;;          (expr->tree '(+ (expt x 2) (* x 3))) -> '(+ (expt (x () ()) (2 () ())) (* (x () ()) (3 () ()))) 
(define (expr->tree expression)
  (if (not (list? expression))
      (if (constant? expression)
	  (make-leaf expression)
	  (make-leaf (symbol expression)))
      (let ((operator (car expression))
	    (left-operand (cadr expression))
	    (right-operand (caddr expression)))
	    (make-tree (symbol operator) (expr->tree left-operand) (expr->tree right-operand)))))

;; evaluate-expression: Evaluate an expression given as list of symbols
;; Example: (evaluate-expression '(* 3 3)) -> 9
;;          (evaluate-expression '(* (expt 2 3) (/ 5 3))) -> 40/3
(define (evaluate-expression expression) (eval expression user-initial-environment))

;; evaluate-constant:
;; Example: (evaluate-constant 5) -> 5
(define (evaluate-constant expression) (and (constant? expression) expression))

;; evaluate-variable:
;; Example: (evaluate-variable 'x 5) -> 5
(define (evaluate-variable expression value)
  (and (variable? expression) ((evaluate-expression (list 'lambda (list 'x) expression)) value)))

;; tree-eval: Evaluate a given expression binary tree with a concrete value
;; Example: (tree-eval (expr->tree '(* (expt x 3) (/ 5 x))) 2) -> 20
(define (tree-eval expression-tree value)
  (if (leaf? expression-tree)
      (or (evaluate-constant (root-tree expression-tree))
	  (evaluate-variable (root-tree expression-tree) value))
      (evaluate-expression (list (root-tree expression-tree)
				 (tree-eval (left-tree expression-tree) value)
				 (tree-eval (right-tree expression-tree) value)))))

;; derive-constant: c -> 0
(define (derive-constant constant) (and (constant? (car constant)) (make-leaf 0)))

;; derive-variable: x -> 1
(define (derive-variable variable) (and (variable? (car variable)) (make-leaf 1)))

;; sum-rule: f + g -> f' + g'
(define (sum-rule left right)
  (make-tree '+ (tree-derive left) (tree-derive right)))

;; difference-rule: f - g -> f' - g'
(define (difference-rule left right)
  (make-tree '- (tree-derive left) (tree-derive right)))

;; product-rule: fg -> fg' + f'g
(define (product-rule left right)
  (make-tree '+
	     (make-tree '* left (tree-derive right))
	     (make-tree '* (tree-derive left) right)))

;; quotient-rule: f/g -> (f'g - g'f) / g^2
(define (quotient-rule left right)
  (make-tree '/
	     (make-tree '-
			(make-tree '* (tree-derive left) right)
			(make-tree '* (tree-derive right) left))
	     (make-tree '* right right)))

;; power-rule: x^n -> nx^(n-1)
(define (power-rule left right)
  (let ((power (car right)))
    (if (variable? right)
	(make-tree '* right (make-tree 'expt left (make-tree '- (make-leaf power) (make-leaf 1))))
	(cond
	 ((= power 0) (make-leaf 1))
	 ((= power 1) left)
	 (else
	  (make-tree '* right (make-tree 'expt left (make-leaf (- power 1)))))))))

;; tree-derive: Derive a given expression tree
;; Example: (tree-derive (expr->tree '(+ (* x x) (+ 7 x)))) -> '(+ (+ (* (x () ()) (1 () ())) (* (1 () ()) (x () ()))) (+;; (0 () ()) (1 () ())))
(define (tree-derive tree)
  (if (leaf? tree)
      (or (derive-constant tree)
	  (derive-variable tree))
      (let ((operator (root-tree tree))
	    (left-operand (left-tree tree))
	    (right-operand (right-tree tree)))
	(cond
	 ((equal? operator '+) (sum-rule left-operand right-operand))
	 ((equal? operator '*) (product-rule left-operand right-operand))
	 ((equal? operator '-) (difference-rule left-operand right-operand))
	 ((equal? operator '/) (quotient-rule left-operand right-operand))
	 ((equal? operator 'expt) (power-rule left-operand right-operand))))))
