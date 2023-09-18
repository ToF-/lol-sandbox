(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

; create an edge between nodes a and b if they're not equal
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

; create a list of edges between random nodes
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

; give the list of all nodes connected to a node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                    edge-list))

; get all the nodes that are directly or indirectly connected to a node
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
      (traverse node))
    visited))

; get all distinct groups of connected nodes
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
                          (let* ((connected (get-connected (car nodes) edge-list))
                                 (unconnected (set-difference nodes connected)))
                            (push connected islands)
                            (when unconnected
                              (find-island unconnected)))))
      (find-island nodes))
    islands))

; create edges between first nodes of all islands
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

