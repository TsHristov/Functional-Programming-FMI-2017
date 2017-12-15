(define (count-digits n)
  (define (helper n count)
    (cond
     ((= n 0) count)
     (else (helper (quotient n 10) (+ count 1)))))
  (helper n 0))

(define (middle-digit n)
  (define digits-count (count-digits n))
  (define middle (quotient digits-count 2))
  (define (helper n count)
    (cond
     ((= (remainder digits-count 2) 0) (- 1))
     ((= count (- digits-count middle 1)) (remainder n 10))
     (else (helper (quotient n 10) (+ count 1)))))
  (helper n 0))

