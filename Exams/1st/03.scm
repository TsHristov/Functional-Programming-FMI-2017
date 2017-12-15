(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

(define (all? p l)
  (not (search (lambda (x) (not (p x))) l)))

(define (cartesian-product l)
  (apply append (map (lambda (x) (map (lambda (y) (cons x y)) l)) l)))

(define (remove-same lst)
  (filter (lambda (x) (not (equal? (car x) (cdr x)))) lst))

(define (is-em? lst op f)
  (and (all? (lambda (x) (member (f x) lst)) lst)
       (all? (lambda (x) (equal? (op (f (car x)) (f (cdr x))) (f (op (car x) (cdr x)))))
	     (remove-same (cartesian-product lst)))))
