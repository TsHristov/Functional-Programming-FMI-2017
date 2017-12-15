(define (transpose matrix) (apply map list matrix))
(define (get-columns matrix) (transpose matrix))

(define (sum lst) (apply + lst))

(define (removed element lst)
  (filter (lambda (x) (not (equal? x element))) lst))

(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

(define (check-property column)
  (search (lambda (x) (equal? x (sum (removed x column)))) column))

(define (count-matching matrix)
  (sum (map (lambda (x) 1) (filter (lambda (column) (check-property column)) (get-columns matrix)))))
  
