;; vertices: Return the verices of a given graph:
(define (vertices graph)
  (map car graph))

;; successors: Return the successors of a given vertex in a graph:
(define (successors vertex graph)
  (cdr (assoc vertex graph)))

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
   (else (map (lambda (x) (if (equal? u (car x))
			      (append x (list v))
			      x)))
	 graph)))

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
