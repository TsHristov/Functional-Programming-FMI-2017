(define test-graph '((A (B . 20) (G . 90) (D . 80))
		     (B (F . 10))
		     (C (D . 10) (H . 20) (F . 50))
		     (D (C . 10) (G . 20))
		     (E (B . 50) (G . 30))
		     (F (C . 10) (D . 40))
		     (H)
		     (G (A . 20))))

;; vertices: Return the verices of a given graph:
(define (vertices graph)
  (map car graph))

;; successors: Return the successors of a given vertex in a graph:
(define (successors vertex graph)
  (if (not (assoc vertex graph)) '()
      (cdr (assoc vertex graph))))

;; has-edge?: Check if path exists between vertex u and vertex v (directed graph):
(define (has-edge? u v graph)
  (and (member v (successors u graph)) #t))


;; Dijkstra: Dijksta algorithm implementation:
(define (Dijkstra start-vertex graph)
  (define MAX_DISTANCE 9999999999)
  (define (get-distance vertex distances)
    (cdr (filter (lambda (x) (equal? (car x) vertex)) distances)))
  (let* ((start (list start-vertex))
	 (unvisited (filter (lambda (v) (not (equal? v start))) (vertices graph)))
	 (distances (map (lambda (node) (if (not (equal? node start))
					    (cons node MAX_DISTANCE)
					    (cons node 0)))
					    univisted)))
    (define (helper visited unvisited distances)
      (let* ((current (car visited)))
	    (current-distance (get-distance current distances))
	    (descendants (successors current graph))
	(map (lambda (x) (if (member (car x) descendants)
			     (if (< (+ current-distance (cdr x)) (cdr x))
				 (cons (cons (car x) (+ current-distance (cdr x)))
				       (filter (lambda (x) (not (equal? (car x) (car x)))) distances))
				 distances)))
			     distances)))
    (helper start unvisited distances)))
  
