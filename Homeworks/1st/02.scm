(define (max-digit n)
  (if (= n 0) 0
      (max (remainder n 10) (max-digit (quotient n 10)))))

(define (max-digit-count n)
  (define max.digit (max-digit n))
  (define (count-digit n count)
    (cond
     ((= n 0) count)
     ((= (remainder n 10) max.digit)
      (count-digit (quotient n 10) (+ count 1)))
     (else (count-digit (quotient n 10) count))))
  (count-digit n 0))

(define (remove-max-digit n)
  (define max.digit (max-digit n))
  (define count (max-digit-count n))
  (define (remove n new step count)
    (let ((last (remainder n 10))
	  (rest (quotient n 10)))
      (cond
       ((= n 0) new)
       ((and (= last max.digit) (= count 1))
	(remove rest new step (- count 1)))
       ((and (= last max.digit) (not (= count 1)))
	(remove rest (+ new (* last step)) (* step 10) (- count 1)))
       (else (remove rest (+ new (* last step)) (* step 10) count)))))
  (remove n 0 1 count))

(define (reduce n)
  (if (= (quotient n 10) 0) n
      (reduce (* (max-digit n) (remove-max-digit n)))))
