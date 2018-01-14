(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

;; roller-coaster-stream: Endless stream of type '(n n-1 n-2 .. 3 2 1 2 3 .. n-2 n-1 n n-1 n-2 ..)
;; Example: (take 10 (roller-coaster-stream 3)) -> (3 2 1 2 3 2 1 2 3 2)
(define (roller-coaster-stream n)
  ;; down?: decrement the sequence? - #t
  ;;        increment the sequence? - #f
  (define (generate-stream current down?)
    (cond
     ((= current n) (cons-stream current (generate-stream (- current 1) #t)))
     ((= current 1) (cons-stream current (generate-stream (+ current 1) #f)))
     (else
      (if (and down?)
	  (cons-stream current (generate-stream (- current 1) #t))
	  (cons-stream current (generate-stream (+ current 1) #f))))))
  (generate-stream n #t))
