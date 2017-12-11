;; weighted-graph: Graph representation via list of descendants (directed, weighted):
(define weighted-graph '((A (B . 20) (G . 90) (D . 80))
			 (B (F . 10))
			 (C (D . 10) (H . 20) (F . 50))
			 (D (C . 10) (G . 20))
			 (E (B . 50) (G . 30))
			 (F (C . 10) (D . 40))
			 (H)
			 (G (A . 20))))

;; sample-graph: Graph representation via list of descendants (directed):
(define graph '((a b c d) (b e f) (c a d) (d b c g) (e) (f b e) (g a)))

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

;; add-vertex: Add new vertex to the graph:
(define (add-vertex vertex graph)
  (if (assoc vertex graph) graph
      (cons (list vertex) graph)))

;; add-edge: Add new edge between two vertices (directed graph):
(define (add-edge u v graph)
  (cond
   ((has-edge? u v graph) graph)
   ((not (member v (vertices graph)))
    (map (lambda (x) (if (equal? u (car x))
			 (append x (list v))
			 x))
	 (add-vertex v graph)))
   ((member v (vertices graph))
    (map (lambda (x) (if (equal? u (car x))
			 (append x (list v))
			 x))
    graph))))

;; path-exists?: Check if the given path of vertices exists in the graph:
(define (path-exists? path graph)
  (if (not (null? (cdr path)))
      (and (has-edge? (car path) (cadr path) graph)
	   (path-exists? (cdr path) graph))
      (and (member (car path) (vertices graph)) #t)))

;; predecessors: Find all vertices in the graph which are predecessors of the given:
(define (predecessors vertex graph)
  (vertices (filter (lambda (x) (member vertex (successors (car x) graph))) graph)))

;; extend-path: Return all possible path extensions with one edge of a given path of vertices:
(define (extend-path path graph)
  (map (lambda (x) (append path (list x)))
       (filter (lambda (x) (not (member x path))) (successors (car (reverse path)) graph))))

;; edge-list: Return the edges of the graph:
(define (edge-list graph)
  (apply append (map (lambda (vertex)
		       (map (lambda (successor) (cons vertex successor)) (successors vertex graph)))
		     (vertices graph))))
  
;; delete-edge: Delete an edge between two vertices in the graph:
(define (delete-edge predecessor successor graph)
  (map (lambda (x) (if (equal? (car x) predecessor)
		       (filter (lambda (y) (not (equal? y successor))) x)
		       x)) graph))

;; graph-traverse: General graph traversal:
(define (graph-traverse start-vertex graph structure-append)
  (define rest cdr)
  (define get-first car)
  (define (traverse data-structure visited)
    (if (null? data-structure) visited
	(let ((descendants (filter (lambda (vertex) (and (not (member vertex visited))
							 (not (member vertex data-structure))))
				   (successors (get-first data-structure) graph)))
	      (current-vertex (list (get-first data-structure))))
	  (if (not (null? descendants))
	      (traverse (structure-append descendants data-structure) (append visited current-vertex))
	      (traverse (rest data-structure) (append visited current-vertex))))))
  (traverse (list start-vertex) '()))

;; BFS: Perform BFS on the given graph:
(define (queue data storage) (append (cdr storage) data))
(define (BFS start graph) (traverse start graph queue))

;; DFS: Perform DFS on the given graph:
(define (stack data storage) (append data (cdr storage)))
(define (DFS start graph) (traverse start graph stack))

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
    (define (filter-distances distances)
      (filter (lambda (x) (member (vertex x) unvisited)) distances))
    ;; Find the minimal distance:
    (define min-distance
      (if (null? unvisited) '()
	  (apply min (map cdr (filter-distances distances)))))
    ;; Choose the unvisited vertex with the minimal distance:
    (let ((min-vertex (filter (lambda (x) (and (member (vertex x) unvisited)
					       (equal? (distance x) min-distance))) distances)))
      (if (null? min-vertex) '()
	  (if (equal? (distance (vertex min-vertex)) MAX_DISTANCE) '()
	      (car min-vertex)))))
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
  

;; TO-DO:
;; find-path: Finds a path between two vertices in the graph:
;; invert: Invert the directions in a graph:
;; all-paths: Find all simple paths in a given graph:
;; cyclic? : Check if the given graph is cyclic:
;; path-exists?: Checks if a path exists between two vertices in the graph:
;; connected?: Checks if the graph is connected:
;; tree?: Check is the graph is a tree.

