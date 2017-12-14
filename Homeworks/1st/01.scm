(define (zip function arguments)
  (map (lambda (argument) (list (+ (function argument) 0.0) argument)) arguments))

(define (extremum function arguments type)
  (if (null? arguments) 'none
      (let* ((zipped (zip function arguments))
	     (values (map car zipped)))
	(cadr (assoc (fold-right type (car values) values) zipped)))))

(define (argmax function arguments) (extremum function arguments max))
(define (argmin function arguments) (extremum function arguments min))

