(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; nth: Takes the n-th element of a given list:
(define (nth n lst)
  (cond
   ((null? lst) '())
   ((= n 0) (car lst))
   (else (nth (- n 1) (cdr lst)))))

;; range: Returns list of the numbers in the interval [from; to]:
(define (range from to)
  (accumulate cons '() from to (lambda (x) x) (lambda (x) (+ x 1))))

;; digit-list: Returns the digits of a given number:
(define (digit-list n)
  (define (digits n)
   (cond
     ((= n 0) '())
     (else (cons (remainder n 10) (digits (quotient n 10))))))
  (reverse* (digits n)))

;; take: Take the first n elements of a given list:
(define (take n lst)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;; drop: Drop the first n elements of a given list:
(define (drop n lst)
  (if (= n 0) lst
      (drop (- n 1) (cdr lst))))
	      
;; sorted: Checks if the array is in ascending order:
(define (sorted l)
  (if (or (null? l) (null? (cdr l))) #t
      (and (<= (car l) (cadr l)) (sorted (cdr l)))))
