;;;; section4

;;;; 4-1
(defun quarter-turn (a)
  (let* ((n (car (array-dimensions a)))
	 (q (make-array (list n n))))
    (do ((y 0 (+ y 1)))
	((eql y n))
      (do ((x 0 (+ x 1)))
	  ((eql x n))
	(setf (aref q x (- n y 1)) (aref a y x))))
    q))

;;; 4-2
(defun copy-list-by-reduce (lst)
  (reduce
   #'(lambda (x y)
       (append x (list y)))
   lst
   :initial-value nil))

(defun reverse-by-reduce (lst)
  (reduce 
   #'(lambda (x y)
       (append (list y) x))
   lst
   :initial-value nil))

;;;; 4-3

;;; (1)
(defstruct tree3
  elt (n1 nil) (n2 nil) (n3 nil))

(defun dup-tree3 (n)
  (if (or (null n) (not (tree3-p n)))
      nil
      (make-tree3
       :elt (tree3-elt n)
       :n1 (dup-tree3 (tree3-n1 n))
       :n2 (dup-tree3 (tree3-n2 n))
       :n3 (dup-tree3 (tree3-n3 n)))))
       
;;; (2)
(defun member-tree3 (obj n)
  (if (or (null n) (not (tree3-p n)))
      nil
      (or (eql (tree3-elt n) obj)
	  (member-tree3 obj (tree3-n1 n))
	  (member-tree3 obj (tree3-n2 n))
	  (member-tree3 obj (tree3-n3 n)))))

;;;; 4-4
(defun bst-sort (bst)
  (let ((l (node-l bst)) (r (node-r bst)) (elt (node-elt bst)))
    (if l
	(bst-sort l))
    (format t " ~A " elt)
    (if r
	(bst-sort r))))
    
;;;; 4-5
;; See http://www.paulgraham.com/ancomliser.html

;;;; 4-6

;;; (a) assoc to hash
;; ((k .v) ...) to hash
(defun assoc2hash (a)
  (let ((h (make-hash-table)))
    (mapcar
     #'(lambda (item)
	 (let ((k (car item)) (v (cdr item)))
	   (setf (gethash k h) v)))
     a)
    h))

;;; (b) hash to assoc
;; hash to ((k .v) ...)
(defun hash2assoc (h)
  (let ((a nil))
    (maphash
     #'(lambda (k v)
	 (setf a (append (list(cons k v)) a)))
     h)
    a))
	     
	     
    
