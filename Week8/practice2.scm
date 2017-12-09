;; sample-graph: Graph representation via list of descendants (directed):
(define sample-graph '((a b c d) (b e f) (c a d) (d b c g) (e) (f b e) (g a)))

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

;; BFS: Perform BFS on the given graph:
(define (BFS start-vertex graph)
  (define (traverse queue visited)
    (if (null? queue) visited
	(let ((descendants (filter (lambda (vertex) (and (not (member vertex visited))
							 (not (member vertex queue))))
				   (successors (car queue) graph)))
	      (current-vertex (list (car queue))))
	  (if (not (null? descendants))
	      (traverse (append (cdr queue) descendants) (append visited current-vertex))
	      (traverse (cdr queue) (append visited current-vertex))))))
  (traverse (list start-vertex) '()))

;; DFS: Perform DFS on the given graph:
(define (DFS start-vertex graph)
  (define (traverse stack visited)
    (if (null? stack) visited
	(let ((descendants (filter (lambda (vertex) (and (not (member vertex visited))
							 (not (member vertex stack))))
				   (successors (car stack) graph)))
	      (current-vertex (list (car stack))))
	  (if (not (null? descendants))
	      (traverse (append descendants (cdr stack)) (append visited current-vertex))
	      (traverse (cdr stack) (append visited current-vertex))))))
  (traverse (list start-vertex) '()))
