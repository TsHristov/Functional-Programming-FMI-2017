(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

;; map*: Map via foldr:
(define (map* f l)
  (foldr (lambda (x nv) (cons (f x) nv)) '() l))

;; reverse*: Reverse via foldr:
(define (reverse* l)
  (foldr (lambda (x y) (append y (list x)) '() l)))

;; reverse**: Reverse via foldl:
(define (reverse** l)
  (foldl (lambda (x y) (cons y x)) '() l))
