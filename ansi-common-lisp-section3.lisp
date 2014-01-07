;;; section 3

;;;; 3-1
; a)  (a b (c d))
;
;  +-------+   +-------+   +-------+
;  |   | --+-->|   | --+-->|   |nil|
;  +-|-----+   +-|-----+   +-|-----+
;    |           |           |
;    V           V           V
;    a           b         +-------+   +-------+
;                          |   | --+-->|   |nil|
;                          +-|-----+   +-|-----+
;                            |           |
;                            V           V
;                            c           d
;
; b) (a (b (c (d))))
;
;  +-------+   +-------+
;  |   | --+-->|   |nil|
;  +-|-----+   +-|-----+
;    V           |      
;    a           V      
;              +-------+   +-------+
;              |   | --+-->|   |nil|
;              +-|-----+   +-|-----+
;                V           |
;                b         +-------+   +-------+
;                          |   | --+-->|   |nil|
;                          +-|-----+   +-|-----+
;                            V           |
;                            c           V
;                                      +-------+
;                                      |   |nil|
;                                      +-|-----+
;                                        V
;                                        d
;
; c) (((a b) c) d)
;  +-------+   +-------+
;  |   | --+-->|   |nil|
;  +-|-----+   +-|-----+
;    |           V
;    |           d
;    V
;  +-------+   +-------+
;  |   | --+-->|   |nil|
;  +-|-----+   +-|-----+
;    |           V
;    |           c
;    V
;  +-------+   +-------+
;  |   | --+-->|   |nil|
;  +-|-----+   +-|-----+
;    V           V
;    a           b
;
; d) (a (b . c) . d)
;  +-------+   +-------+
;  |   | --+-->|   |   |
;  +-|-----+   +-|---|-+
;    |           |   V
;    V           |   d
;    a           V
;              +-------+
;              |   |   |
;              +-|---|-+
;                V   V
;                b   c

;;;; 3-2

;; x and y must be list
(defun new-union (x y)
  (if (null y)
      x
      (let ((p (car y)) (q (cdr y)))
	(if (not (member p x))
	    (new-union (append x (list p)) q)
	    (new-union x q)))))

;;;; 3-3

(defun assoc-inc (item alist)
  (let ((p (assoc item alist)))
    (if p
	(incf (cdr p))
	(setq alist (acons item 1 alist)))
    alist))

(defun my-occur (x)
  (if (null x)
      nil
      (let ((item (car x)) (alist (my-occur (cdr x))))
	(assoc-inc item alist))))

(defun my-occurrences (x)
  (sort (my-occur x) #'(lambda (a b) (> (cdr a) (cdr b)))))

;;;; 3-4
;; member が eql で比較をし、eql は、異ったリスト (cons) を別のものと
;; 見做すため。
;; (member '(a) '((a) (b)) :test #'equal) は、真 ('((a) (b))) を返す。
;; (let ((x '(a))) (member x (list x '(b)))) も同じく真を返す。

;;; 3-5
;; (a) recursive version
(defun pos-recursive2 (x n)
  (if (null x)
      nil
      (cons (+ n (car x)) (pos-recursive2 (cdr x) (+ 1 n)))))

(defun pos-recursive (x)
  (pos-recursive2 x 0))

;; (b) loop version
(defun pos-loop (x)
  (let ((result))
    (do ((lst x (cdr lst))
	 (n 0 (+ 1 n)))
	((null lst) result)
      (push (+ (car lst) n) result))
    (reverse result)))

;; (b) loop version 2
(defun pos-loop-2 (x)
  (do ((lst x (cdr lst))
       (n 0 (+ 1 n)))
      ((null lst) x)
    (incf (car lst) n)))

;; (c) mapcar version
(defun pos-mapcar (lst)
  (let ((n 0))
    (mapcar
     #'(lambda (x)
	 (let ((v (+ x n)))
	   (incf n)
	   v))
     lst)))

;;;; 3-6
(defun cons2 (x y)
  (cons y x))

(defun list2s (args)
  (if (atom args)
      args
      (cons (list2s (cdr args)) (list2s (car args)))))

(defun list2 (&rest args)
  (list2s args))

(defun length2 (lst)
  (if (null lst)
      0
      (+ 1 (length2 (car lst)))))

(defun member2 (p lst)
  (if (null lst)
      nil
      (if (eql p (cdr lst))
	  (car lst)
	  (member2 p (car lst)))))

;;;; 3-7
(defun compress (x)
  (if (null (cdr x))
      x
      (let ((p (car x))
	    (rest (compress (cdr x))))
	;; compress returns nil, (a ...) or ((a . n) ...) form.
	(let ((top (car rest)) (tail (cdr rest)))
	  ;; (a ...) case
	  (if (atom top)
	      (if (eql p top)
		  (cons (cons top 2) tail)
		  (cons p rest))
	      ;; ((a . n) ...) case
	      (let ((v (car top))
		    (c (cdr top)))
		(if (eql p v)
		    (cons (cons p (+ 1 c)) tail)
		    (cons p rest))))))))

;;;; 3-8
(defun showdots (lst)
  (if (atom lst)
      (format t "~A" lst)
      (progn
	(format t "(")
	(showdots (car lst))
	(format t " . ")
	(showdots (cdr lst))
	(format t ")"))))

;;;; 3-9

;;; Search shortest path
;;; Graph format
;;;  ((node connected1, connected2 ...) ...)

(defun connected (from path)
  (cdr (assoc from path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun search-route (path net end)
  (if (null path)
      nil
      (let ((top (car path)) (rest (cdr path)) (cand))
	(dolist (q (connected (car top) net))
	  (let ((newp (cons q top)))
	    (if (eql q end)
		(return-from search-route newp)
		(push newp cand))))
	(search-route (nconc rest cand) net end))))
		
(defun shortest-path (start end net)
  (let ((pathes (list (list start))))
    (reverse (search-route pathes net end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun search-route-loop (path net end)
  (let ((result nil))
    (loop
       (dolist (p path)
	 (dolist (q (connected (car p) net))
	   (let ((newp (cons q p)))
	     (if (eql q end)
		 (return-from search-route-loop newp)
		 (push newp result)))))
       (if (null result)
	   ;; no more canidates
	   (return-from search-route-loop nil)
	   ;; result as next target and clear result
	   (setq path result result nil)))))

(defun shortest-path-loop (start end net)
  (let ((pathes (list (list start))))
    (reverse (search-route-loop pathes net end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Longest path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circularp (p path)
  (member p path))

(defun search-route-longest (path net end longest)
  (if (null path)
      longest
      (let ((top (car path)) (rest (cdr path)) (cand))
	(dolist (q (connected (car top) net))
	  (if (not (circularp q top))
	      (let ((newp (cons q top)))
		(if (eql q end)
		    (setq longest newp)
		    (push newp cand)))))
	(search-route-longest (nconc rest cand) net end longest))))
		
(defun longest-path (start end net)
  (let ((pathes (list (list start))))
    (reverse (search-route-longest pathes net end nil))))
