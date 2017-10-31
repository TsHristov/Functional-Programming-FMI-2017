;; reverse*: Return reversed version of a list:
(define (reverse* l)
  (define (rev-ln l res)
    (if (null? l)
	res
	(rev-ln (cdr l) (cons (car l) res))))
  (rev-ln l '()))

;; copy: Returns an identical list to l:
(define (copy l)
  (if (null? l)
      '()
      (cons (car l) (copy (cdr l)))))

;; len: Find the length of a list:
(define (len l)
  (if (null? l)
      0
      (+ (len (cdr l)) 1)))

;; gen-list: Generate a list from an interval [a, b]:
(define (gen-list a b)
  (define (gen-list-ln a b acc)
    (if (> a b) acc
	(gen-list (+ a 1) b (cons a acc))))
  (reverse (gen-list a b '())))

;; take-n: Takes the first n elements of a list:
(define (take-n n l)
  (if (or (= n 0) (null? l))
      '()
      (cons (car l) (take-n (- n 1) (cdr l)))))

;; drop-n: Skips the first n elements of a list:
(define (drop-n n l)
  (if (or (= n 0) (null? l))
      l
      (drop-n (- n 1) (cdr l))))

;; get-nth: Get the nth element of a list:
(define (get-nth n l)
  (if (= n 0)
      (car l)
      (get-nth (- n 1) (cdr l))))

;; append1: Append an element x to a list:
(define (append1 l x)
  (reverse (cons x (reverse l))))

;; append2: Append an element x to a list:
(define (append2 x l)
  (if (null? l)
      (list x)
      (cons (car l) (append2 x (cdr l)))))

;; append-lists: Appends two lists (O(n)):
(define (append-lists x y)
  (if (null? x)
      y
      (cons (car x) (app (cdr x) y))))

;; member*: Checks if an element x is a member of a list l:
(define (member* x l)
  (if (null? l)
      l
      (or (equal? x (car l))
	  (member x (cdr l)))))

;; remove1: Remove an element at a given position n:
(define (remove1 n l)
  (if (= n 0)
      (cdr l)
      (cons (car l) (remove (- n 1) (cdr l)))))

;; remove2: Remove an element at a given position n (using drop and take):
(define (remove2 n l)
  (app (take (- n 1) l) (drop n l)))

;; insert-at: Insert an element at a given position:
(define (insert-at n l x)
  (app (take l (- n 1))
       (cons x (drop-n (- n 1) l))))



