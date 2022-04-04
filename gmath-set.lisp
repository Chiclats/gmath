					;集合部分

(defun re-set (list)
  (let* ((fst (pop list))
	 ans-l)
    (if (equal list nil)
	(list fst)
	(if (equal (find fst (setf ans-l (re-set list))) nil)
	    (push fst ans-l)
	    ans-l))))

(defun setp (set)
  (= (length set) (length (re-set set))))

(defun union-set (set1 &rest rest)
  (let ((ans set1))
    (dolist (s rest ans)
      (setf ans (re-set (concatenate 'list ans s))))))

(defun intersection-set (set1 &rest rest)
  (let ((di-in-s (lambda (set1 set2)
		   (let ((sum (concatenate 'list set1 set2))
			 (unn (union-set set1 set2)))
		     (dolist (p unn sum)
		       (setq sum (remove p sum :count 1)))))))
    (if (equal rest nil)
	set1
	(funcall di-in-s set1 (apply #'intersection-set rest)))))

(defun complement-set (set1 &rest rest)
  (let ((ans set1))
    (dolist (s rest ans)
      (dolist (p s)
	(setq ans (remove p ans :count 1))))))

