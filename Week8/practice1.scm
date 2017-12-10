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
  (define distance cdr)
  (define vertex car)
  ;; --------------- Utility functions: -------------------
  ;; ------------------------------------------------------
  ;; get-distance: Get distance associated with a given vertex in the graph:
  (define (get-distance vertex distances)
    (distance (filter (lambda (x) (equal? (vertex x) vertex)) distances)))
  ;; -------------------------------------------------------
  ;; initialize-distances: Initialize all distances at the start of the algorithm.
  ;;     Distance 0 for start vertex.
  ;;     Distance Infinity for all other vertices.
  (define (initialize-distances start vertices)
    (map (lambda (node) (if (not (equal? node start))
			    (cons node MAX_DISTANCE)
			    (cons node 0)))
	 vertices))
  ;; --------------------------------------------------------
  ;; select-min-distance: Choose the vertex with minimal distance
  (define (select-min-distance distances)
    (define min-distance (apply min (map cdr distances)))
    ;; If there are many, choose first:
    (caar (filter (lambda (x) (equal? (distance x) min-distance)) distances)))
  ;; --------------------------------------------------------
  (define (get-unvisited-vertex vertex unvisited)
    (filter (lambda (x) (equal? x vertex)) unvisited))
  ;; --------------------------------------------------------
  ;; update-distances: Update all neighbours distances.
  (define (update-distances distances descendants current-distance)
    (map (lambda (x) (if (member (car x) descendants)
			 (if (< (+ current-distance (cdr x)) (cdr x))
			     (cons (cons (car x) (+ current-distance (cdr x)))
				   (filter (lambda (x) (not (equal? (car x) (car x)))) distances))
			     distances)
			 x))
	 distances))
  ;; ------------------ Dijkstra's Algorithm: -------------------------
  (let* ((start (list start-vertex))
	 (unvisited (vertices graph))
	 (distances (initialize-distances start unvisited)))
    (define (run visited unvisited distances)
      (let* ((current (get-unvisited-vertex (select-min distances)))
	     (current-distance (get-distance current distances))
	     (descendants (successors current graph)))
	(if (null? current) visited
	    (run (append visited current)
		 (filter (lambda (x) (not (equal? x current))) unvisited)
		 (update distances descendants current-distance)))))
    (run '() unvisited distances)))
  
