					;线代部分

(defgeneric scl_* (scalar generalized_vector)
  (:documentation "This GEN is used to do scalar-multiply on a generalized vector"))

(defun vector_- (v1 &rest rest)
  (let ((ans v1))
    (dolist (v rest ans)
      (setq ans (map 'list #'- ans v)))))

(defun vector_+ (v1 &rest rest)
  (let ((ans v1))
    (dolist (v rest ans)
      (setq ans (map 'list #'+ ans v)))))

(defun vector_* (scalar vector)
  (let ((ans (concatenate 'list vector)))
    (dotimes (i (length vector) ans)
      (setf (nth i ans) (* scalar (nth i ans))))))

(defmethod scl_* (scalar (vector list))
  (vector_* scalar vector))

(defun inner_* (v1 v2 &key (metric-matrix t m-mtx-supplied-p))
  (if (equal m-mtx-supplied-p nil)
      (apply #'+ (map 'list #'* v1 v2))
      (first (content (mtx_* (v->m v1) metric-matrix (turn (v->m v2)))))))

(defclass matrix ()
  ((content
    :initarg :content
    :initform '((0))
    :accessor content)))
(defmethod explain ((object matrix) &key (io-stream t))
  (format io-stream "~d * ~d :~%" (length (content object)) (length (first (content object))))
  (dolist (v (content object))
    (dolist (s v)
      (format io-stream "~a~,8t" s))
    (format io-stream "~%"))
  object)

(defun mtx_* (m1 &rest rest)
  (if (equal rest nil)
      m1
      (let* ((m2 (apply #'mtx_* rest))
	     r-v
	     ans
	     (r (length (content m1)))
	     (c (length (first (content m2)))))
	(dotimes (ir r)
	  (dotimes (ic c)
	    (push (inner_* (r-vec (- r ir 1) m1) (c-vec m2 (- c ic 1))) r-v))
	  (push r-v ans)
	  (setq r-v nil))
	(make-instance 'matrix :content ans))))

(defun mtx_+ (m1 &rest rest)
  (if (equal rest nil)
      m1
      (let ((m2 (apply #'mtx_+ rest)))
	(make-instance 'matrix :content (map 'list #'vector_+ (content m1) (content m2))))))

(defun mtx_- (m1 &rest rest)
  (if (equal rest nil)
      m1
      (let ((m2 (apply #'mtx_- rest)))
	(make-instance 'matrix :content (map 'list #'vector_- (content m1) (content m2))))))

(defmethod scl_* (scl (mtx matrix))
  (make-instance 'matrix :content (map 'list (lambda (v) (scl_* scl v)) (content mtx))))

(defun elmt (matrix r c)
  (nth c (nth r (content matrix))))

(defun r-vec (r matrix)
  (nth r (content matrix)))

(defun c-vec (matrix c)
  (let (ans)
    (dotimes (i (length (content matrix)) ans)
      (setq ans (concatenate 'list ans `(,(elmt matrix i c)))))))

(defun turn (matrix)
  (let ((c (length (first (content matrix))))
	ans)
    (make-instance 'matrix :content (dotimes (i c ans)
				      (push (c-vec matrix (- c i 1)) ans)))))

(defun v->m (vec &key (form '1*n))
  (if (equal form '1*n)
      (make-instance 'matrix :content `(,vec))
      (let (ans)
	(make-instance 'matrix :content (dotimes (i (length vec) ans)
					  (push `(,(nth (- (length vec) i 1) vec)) ans))))))

(defun hermite (matrix)
  (make-instance 'matrix
		 :content (map 'list (lambda (v) (map 'list #'conjugate v)) (content (turn matrix)))))

(defun mtx_trace (matrix)
  (if (/= (length (content matrix)) (length (first (content matrix))))
      (error "The matrix supplied is not a square matrix!"))
  (let ((ans 0))
    (dotimes (i (length (content matrix)) ans)
      (incf ans (elmt matrix i i)))))

(defun triu-r (matrix)
  (let* (l_mtx
	 (r (length (content matrix)))
	 (c (length (first (content matrix))))
	 (0vp (lambda (v)
		(apply #'= 0 v)))
	 (c-v (lambda (l-mtx c st_r)
		(let (ans)
		  (dotimes (i (- (length l-mtx) st_r) ans)
		    (setq ans (concatenate 'list ans `(,(nth c (nth (+ i st_r) l-mtx))))))))))
    (setq l_mtx (concatenate 'list (content matrix)));;
    (do ((ir 0)
	 (ic 0))
	((or (= ir r) (= ic c))
	 (make-instance 'matrix :content l_mtx))
      ;(format t "~%r c: ~d ~d~%" ir ic);
      (if (funcall 0vp (funcall c-v l_mtx ic ir))
	  (incf ic)
	  (progn
	    (let ((rc (+ ir (position-if (lambda (x) (not (= x 0))) (funcall c-v l_mtx ic ir)))));
	      (tempf (nth ir l_mtx)
		     (nth rc l_mtx)))
	      ;(print rc));
	    ;(print l_mtx);
	    (setf (nth ir l_mtx) (scl_* (/ 1 (nth ic (nth ir l_mtx))) (nth ir l_mtx)))
	    ;(format t "~%A:~A~%" l_mtx)  ;
	    (dotimes (i r)
	      (if (/= i ir)
		  (setf (nth i l_mtx)
			(vector_- (nth i l_mtx) (scl_* (nth ic (nth i l_mtx)) (nth ir l_mtx))))))
	    ;(format t "~%B:~A~%" l_mtx);
	    (incf ir)
	    (incf ic))))))
    
(defun rank (mtx_or_v1 &rest v_rest)
  (if (equal (type-of mtx_or_v1) 'cons)
      (rank (make-instance 'matrix :content (push mtx_or_v1 v_rest)))
      (position-if (lambda (v) (apply #'= 0 v)) (content (triu-r mtx_or_v1)))))

(defun unitary-matrix (n)
  (diag (let (ans)
	  (dotimes (i n ans) (push 1 ans)))))

(defun inv (mtx)
  (let* ((t-m (turn mtx))
	 (r (length (content mtx)))
	 (c (length (first (content mtx))))
	 (p (if (= r c)
		t
		(error "The matrix supplied is not a square matrix!")))
	 (q (if (= (det mtx) 0)
		(error "The matrix supplied is a singular matrix!")
		t))
	 mtemp ans-l)
    (setf (content t-m) (concatenate 'list (content t-m) (unitary-matrix r)))
    (setf mtemp (triu-r (turn t-m)))
    (dotimes (i r (turn (make-instance 'matrix :content ans-l)))
      (push (c-vec mtemp (+ r r (- i) -1)) ans-l))))

(defun det (mtx)
  (let* ((l-mtx (content (scl_* 1 mtx)))
	 (ans 0)
	 (p (if (= (length l-mtx) (length (first l-mtx)))
		t
		(error "The matrix supplied is not a square matrix!")))
	 (c-v (lambda (l-mtx c st_r)
		(let (ans)
		  (dotimes (i (- (length l-mtx) st_r) ans)
		    (setq ans (concatenate 'list ans `(,(nth c (nth (+ i st_r) l-mtx))))))))))
    (if (= 1 (length l-mtx))
	(elmt mtx 0 0)
	(dotimes (i (length l-mtx) ans)
	  (incf ans (* (elmt mtx 0 i)
		       (if (evenp i) 1 -1)
		       (det (make-instance 'matrix
					   :content (let (anss)
						      (dotimes (j (length (content mtx)) anss)
							(if (/= (- (length l-mtx) j 1) i)
							    (push (funcall c-v l-mtx (- (length l-mtx) j 1) 1) anss))))))))))))

(defun smtx (elmt-list)
  (let ((c (position '* elmt-list))
	(lst (remove '* elmt-list))
	r-v
	ans)
    (dotimes (i (length lst) (make-instance 'matrix :content ans))
      (setq r-v (concatenate 'list r-v `(,(nth i lst))))
      (if (= 0 (mod (1+ i) c))
	  (progn 
	    (setq ans (concatenate 'list ans `(,r-v)))
	    (setq r-v nil))))))

(defun full-rank-p (matrix)
  (not (= (det matrix) 0)))

(defun setel (mtx r c new-value)
  (if (or (>= r (length (content mtx))) (>= c (length (first (content mtx)))))
      (error "In SETEL: The position supplied is not in the scale of the matrix.")
      (let* ((row (nth r (content mtx))))
	(setf (nth c row) new-value)
	(setf (nth r (content mtx)) row)
	mtx)))

(defun gmath-linearAl-zero-mtx-zero-list (r c)
  (let (ans)
    (dotimes (i (* c r) ans)
      (push 0 ans)
      (if (= i (- (* c r) c 1))
	  (push '* ans)))))

(defun zeros-mtx (r &optional (c 1 c-s-p))
  (if (not c-s-p) (setq c r))
  (let* ((m-l (gmath-linearal-zero-mtx-zero-list r c))
	 (fans (smtx m-l)))
    fans))
