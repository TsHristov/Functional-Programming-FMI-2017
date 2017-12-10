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
  ;; ----------------------------- Utility functions: ----------------------------------
  ;; -----------------------------------------------------------------------------------
  ;; get-distance: Gets the distance associated with a given vertex in the graph:
  (define (get-distance vertex distances)
    (distance (car (filter (lambda (x) (equal? (vertex x) vertex)) distances))))
  ;; -----------------------------------------------------------------------------------
  ;; initialize-distances: Initialize all distances at the start of the algorithm.
  ;;     -Distance 0 for start vertex.
  ;;     -Distance MAX_DISTANCE for all other vertices.
  (define (initialize-distances start vertices)
    (map (lambda (vertex) (if (not (equal? vertex start))
			    (cons vertex MAX_DISTANCE)
			    (cons vertex 0)))
	 vertices))
  ;; -----------------------------------------------------------------------------------
  ;; min-distance-vertex: Choose the unvisited vertex with minimal distance.
  (define (min-distance-vertex distances unvisited)
    ;; Choose only from vertices that are unvisited:
    (define (filter-distances distances unvisited)
      (filter (lambda (x) (member (vertex x) unvisited)) distances))
    ;; Find the minimal distance:
    (define min-distance
      (if (null? unvisited) '()
	  (apply min (map cdr (filter-distances distances unvisited)))))
    ;; Choose the unvisited vertex with the minimal distance:
    (let ((min-vertex (filter (lambda (x) (and (member (vertex x) unvisited)
					       (equal? (distance x) min-distance))) distances)))
      (if (null? min-vertex) '()
	  (car min-vertex))))
  ;; --------------------------------------------------------
  ;; update-distances: Update all neighbours distances.
  (define (update-distances distances descendants current-distance)
    (define (descendant-distance descendant) (distance (assoc descendant descendants)))
    (map (lambda (x) (if (assoc (vertex x) descendants)
			 (if (< (+ current-distance (descendant-distance (vertex x)) (distance x)))
			     (cons (vertex x) (+ current-distance (descendant-distance (vertex x))))
			     (cons (vertex x) (distance x)))
			 x))
	 distances))
  ;; ------------------ Dijkstra's Algorithm: -------------------------
  (define start (list start-vertex))
  (define unvisited (vertices graph))
  (define distances (initialize-distances start-vertex unvisited))
  (define (run visited unvisited distances)
    (let ((current (min-distance-vertex distances unvisited)))
      (if (null? current) visited
	  (let ((descendants (successors (vertex current) graph)))
	       (run (append visited (list current))
		    (filter (lambda (x) (not (equal? x (vertex current)))) unvisited)
		    (update-distances distances descendants (distance current)))))))
  (run '() unvisited distances))
  
