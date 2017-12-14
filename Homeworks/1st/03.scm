;; Compare three root-finding algorithms:
;;     - Newton`s Method
;;     - Bisection Method
;;     - Secant Method
;; ---------------------------------------------------------------------------

(define (bisection-method f a b eps)
  (define (search a b iterations)
    (let ((c (/ (+ a b) 2.0)))
      (if (or (= (f c) 0) (< (- c a) eps)) (list (list 'iterations: iterations)
						 (list 'root: c))
	  (cond
	   ((< (* (f a) (f c)) 0) (search a c (+ 1 iterations)))
	   ((< (* (f b) (f c)) 0) (search c b (+ 1 iterations)))))))
  (search a b 0))

;; ---------------------------------------------------------------------------

(define (secant-method f a b eps)
  (define xi-1 a)
  (define xi b)
  (define (xi+1 xi-1 xi)
    (- xi (/ (* (f xi) (- xi xi-1)) (- (f xi) (f xi-1)))))
  
  (define (search xi-1 xi iterations)
    (if (or (= (f xi) 0) (< (- xi xi-1) eps)) (list (list 'iterations: iterations)
						 (list 'root: xi))
	(search xi (xi+1 xi-1 xi) (+ 1 iterations))))
  (search xi-1 xi 0))

;; ---------------------------------------------------------------------------

(define (newton-method f a b eps)
  (define dx 0.001)
  (define (derive f)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
  (define x0 a)
  (define (xi+1 xi)
    (- xi (/ (f xi) ((derive f) xi))))
  (define (search xi-1 xi iterations)
    (if (or (= (f xi) 0) (< (- xi xi-1) eps)) (list (list 'iterations: iterations)
						 (list 'root: xi))
	(search xi (xi+1 xi) (+ 1 iterations))))
  (search x0 (xi+1 x0) 0))

;; ---------------------------------------------------------------------------

(define (compare-methods f a b eps)
  (list (cons    'Newton-Method: (newton-method f a b eps))
	(cons 'Bisection-Method: (bisection-method f a b eps))
	(cons    'Secant-Method: (secant-method f a b eps))))
  
    
