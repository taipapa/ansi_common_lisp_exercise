;;; 2-2
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c ())))

;;; 2-3
(defun get-fourth (x)
  (car (cdr (cdr (cdr x)))))

;;; 2-4
(defun mymax (x y)
  (if (> x y)
      x
      y))

;;; 2-5

;; enigma は、x の中に nil が含まれるかどうかを返す。

; (enigma '(a b c)) -> nil
; (enigma '(a nil c)) -> T

;; mystery は、y の中で x の最初に出現する位置を返す (0 オリジン)。
;; 発見されなかった場合は nil を返す。

;; (mystery 'c '(a b c d e f))
;; 2

;;; 2-6

;; (a)
(car (car (cdr '(a (b c) d))))

;; (b)
(or 13 (/ 1 0))

;; (c)
(apply #'list 1 nil)

;;; 2-7

(defun have-list (x)
  (if (atom x)
      nil
      (or (listp (car x))
           (have-list (cdr x)))))

;;; 2-8

;; (a)
(defun print-dots-recursive (x)
  (if (> x 0)
      (progn
         (format t ".")
         (print-dots-recursive (- x 1)))))

(defun print-dots-loop (x)
  (do ((i 0 (+ i 1)))
      ((>= i x) nil)
    (format t ".")))

;; (b)

;; Those versions do not handle a in sublist, such as '(a b (a b) c)
(defun count-sym-a-recursive (x)
  (if (null x)
      0
      (+ (if (eql (car x) 'a) 1 0)
	 (count-sym-a-recursive (cdr x)))))

(defun count-sym-a-loop (x)
  (let ((cnt 0))
    (do ((tmp x (cdr tmp)))
        ((null tmp) cnt)
        (if (eql (car tmp) 'a)
            (incf cnt)))))

;; This version can handle 'a and '(a . a)
(defun count-sym-a-recursive-2 (x)
  (cond ((atom x) (if (eql x 'a) 1 0))
	(t  (+ (if (eql (car x) 'a) 1 0)
	       (count-sym-a-recursive-2 (cdr x))))))

;; This version can handle sublist 
(defun count-sym-a-recursive-3 (x)
  (cond ((eql x 'a) 1)
	((atom x) 0)
	(t (+ (count-sym-a-recursive-3 (car x))
	      (count-sym-a-recursive-3 (cdr x))))))

;;; 2-9

;; (a)
(defun summit (lst)
  (let ((n (remove nil lst)))
    (apply #'+ n)))

;; (b)
(defun summit-2 (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
          (summit-2 (cdr lst))
          (+ x (summit-2 (cdr lst)))))))
