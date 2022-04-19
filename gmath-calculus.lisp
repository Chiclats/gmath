(defun factorial (x)
  (if (and (integerp x) (>= x 0))
      (if (= x 0)
	  1
	  (let ((ans 1))
	    (dotimes (i x ans)
	      (setq ans (* ans (1+ i))))))
      (error "FACTORIAL: not a positive integer supplied")))

(defun double-factorial (x)
  (if (and (integerp x) (>= x 0))
      (if (evenp x)
	  (* (expt 2 (/ x 2)) (factorial (/ x 2)))
	  (let ((ans 1))
	    (dotimes (i (/ (1+ x) 2) ans)
	      (setq ans (* ans (1+ (* 2 i)))))))
      (error "DOUBLE-FACTORIAL: not a positive integer supplied")))

(defun gmath-calculus-nest (degree coefficient-list x &optional (bases-list nil b-s-p))
  (if (not b-s-p)
      (setf bases-list (make-array degree :initial-element 0)))
  (let ((y (nth degree coefficient-list)))
    (dotimes (i degree y)
      (setf y (+ (* y (- x (elt bases-list (- degree i 1)))) (nth (- degree i 1) coefficient-list))))))

(defun gmath-calculus-newtondd-generate-mtx (n x-list fx-list)
  (if (or (/= n (length x-list)) (/= n (length fx-list)))
      (error "GMATH-CALCULUS-NEWTONDD-GENERATE-MTX: not correct length of x-list and fx-list")
      (let ((a (make-list (* n (1- n)) :initial-element 0)))
	(setq a (concatenate 'list fx-list a))
	(push '* a)
	(setq a (concatenate 'list x-list a))
	(smtx a))))

(defun newtondd (x-list fx-list)
  (let ((n (length x-list)))
    (if (/= n (length fx-list))
	(error "NEWTONDD: not same length of x-list and fx-list")
	(let ((mtx (gmath-calculus-newtondd-generate-mtx n x-list fx-list))
	      (result-list nil))
	  ;(explain mtx)
	  (dotimes (i (1- n) mtx)
	    (dotimes (j (- n i 1))
	      (setel mtx (+ i 2) j (/ (- (elmt mtx (1+ i) j) (elmt mtx (1+ i) (1+ j)))
				      (- (elmt mtx 0 j) (elmt mtx 0 (+ j i 1))))))
	    ;(explain mtx)
	    )
	  (setq result-list (c-vec mtx 0))
	  (pop result-list)
	  result-list))))

;(defun gmath-calculus-var? (x)
;  (and (symbolp x)
;       (eql (char (symbol-name x) 0) #\?)))

(defun gmath-calculus-differential-main (sentence var)
  (cond ((typep sentence 'list)
	 (let ((fir (car sentence))
	       (sec (second sentence))
	       (re (cddr sentence)))
	   (cond ((typep fir 'list) (gmath-calculus-differential-main fir var))
		 ((= (length sentence) 1)
		  (if (equal fir var) 1 0))
		 ((or (equal fir '+) (equal fir '-))
		  (if (equal re nil)
		      (gmath-calculus-differential-main sec var)
		      `(,fir ,(gmath-calculus-differential-main sec var) ,(gmath-calculus-differential-main (push '+ re) var))))
		 ((equal fir '*)
		  (if (equal re nil)
		      (gmath-calculus-differential-main sec var)
		      `(+ (* ,(gmath-calculus-differential-main sec var) ,(push '* re))
			  (* ,sec ,(gmath-calculus-differential-main re var)))))
		 ((equal fir '/)
		  `(/ (- (* ,(gmath-calculus-differential-main sec var) ,(push '* re))
			 (* ,sec ,(gmath-calculus-differential-main re var)))
		      (expt ,re 2)))
		 ((equal fir '=)
		  (let (ans)
		    (concatenate 'list '(=)
				 (dotimes (i (1- (length sentence)) ans)
				   (push (gmath-calculus-differential-main (nth (- (length sentence) i 1) sentence) var) ans)))))
		 ((equal fir 'exp)
		  `(* ,sentence ,(gmath-calculus-differential-main sec var)))
		 ((equal fir 'expt)
		  `(+ (* ,re (expt ,sec (- ,re 1)) ,(gmath-calculus-differential-main sec var))
		      (* (log ,sec) ,sentence ,(gmath-calculus-differential-main re var))))
		 ((equal fir 'log)
		  (if (equal re nil)
		      `(* (/ 1 ,sec) ,(gmath-calculus-differential-main sec var))
		      (gmath-calculus-differential-main `(/ (log ,sec) (log ,re)) var)))
		 ((equal fir 'sin)
		  `(* (cos ,sec) ,(gmath-calculus-differential-main sec var)))
		 ((equal fir 'cos)
		  `(* -1 (sin ,sec) ,(gmath-calculus-differential-main sec var)))
		 ((equal fir 'tan)
		  `(* (/ 1 (expt (cos ,sec) 2)) ,(gmath-calculus-differential-main sec var)))
		 ((equal fir 'sinh)
		  `(* (cosh ,sec) ,(gmath-calculus-differential-main sec var)))
		 ((equal fir 'cosh)
		  `(* (sinh ,sec) ,(gmath-calculus-differential-main sec var)))
		 ((equal fir 'tanh)
		  `(* (- 1 (expt ,sentence 2)) ,(gmath-calculus-differential-main sec var)))
		 (t 0))))
	  ((equal sentence var) 1)
	  (t 0)))

(defun differential-at (sentence var value &optional (time 1))
  (eval `(let ((,var ,value))
	   ,(differential sentence var time))))

(defun gmath-calculus-differential-modify (sentence)
  (cond ((not (consp sentence)) sentence)
	((= (length sentence) 1) (gmath-calculus-differential-modify (car sentence)))
	(t (let* ((sym (pop sentence))
		  (len (length sentence))
		  (ans (list sym)))
	     (case sym
	       ('+
		(dotimes (i len ans)
		  (let ((item (gmath-calculus-differential-modify (nth i sentence))))
		    (if (not (equal item 0))
			(if (consp item)
			    (if (equal (first item) '+)
				(progn
				  (pop item)
				  (setq ans (concatenate 'list ans item)))
				(setq ans (concatenate 'list ans (list item))))
			    (setq ans (concatenate 'list ans (list item)))))))
		(if (= (length ans) 2)
		    (second ans)
		    (if (= (length ans) 1)
			0
			ans)))
	       ('-
		(dotimes (i len ans)
		  (let ((item (gmath-calculus-differential-modify (nth i sentence))))
		    (if (not (equal item 0))
			(if (consp item)
			    (if (equal (first item) '+)
				(progn
				  (pop item)
				  (setq ans (concatenate 'list ans item)))
				(setq ans (concatenate 'list ans (list item))))
			    (setq ans (concatenate 'list ans (list item)))))))
		(if (= (length ans) 2)
		    (second ans)
		    (if (= (length ans) 1)
			0
			ans)))
	       ('*
		(block tag
		  (dotimes (i len ans)
		    (let ((item (gmath-calculus-differential-modify (nth i sentence))))
		      (if (not (equal item 0))
			  (if (not (equal item 1))
			      (if (consp item)
				  (if (equal (first item) '*)
				      (progn
					(pop item)
					(setq ans (concatenate 'list ans item)))
				      (setq ans (concatenate 'list ans (list item))))
				  (setq ans (concatenate 'list ans (list item)))))
			  (return-from tag 0))))
		  (if (= (length ans) 2)
		      (second ans)
		      (if (= (length ans) 1)
			  1
			  ans))))
	       ('/
		(if (= len 1)
		    (gmath-calculus-differential-modify (car sentence))
		    (let ((sec (gmath-calculus-differential-modify (pop sentence)))
			  (re (gmath-calculus-differential-modify (push '* sentence))))
		      (if (not (consp re))
			  `(/ ,sec ,re)
			  (if (equal (first re) '*)
			      (concatenate 'list (list '/ sec) (cdr re))
			      (list '/ sec re))))))
	       ('expt
		(let ((sec (gmath-calculus-differential-modify (first sentence)))
		      (thi (gmath-calculus-differential-modify (second sentence))))
		  (case thi
		    (0 1)
		    (1 sec)
		    (otherwise `(expt ,sec ,thi)))))
	       (otherwise
		(dotimes (i len ans)
		  (setq ans (concatenate 'list ans (list (gmath-calculus-differential-modify (nth i sentence))))))))))))	

(defun differential (sentence var &optional (time 1))
  (if (= time 0)
      sentence
      (gmath-calculus-differential-modify (gmath-calculus-differential-main (differential sentence var (1- time)) var))))

(defun numerical-integrate (fx x-beg x-end step-num &optional (method 'simpson))
  (let ((step (/ (- x-end x-beg) step-num))
	(func (case method
		('simpson (lambda (fx x0 x1)
			    (let ((xm (/ (+ x0 x1) 2))
				  (h (- x1 x0)))
			      (* h (+ (funcall fx x0) (funcall fx x1) (* 4 (funcall fx xm))) 1/6))))
		('mid (lambda (fx x0 x1)
			(let ((xm (/ (+ x0 x1) 2))
			      (h (- x1 x0)))
			  (* h (funcall fx xm)))))
		('trapezoidal (lambda (fx x0 x1)
				(let ((h (- x1 x0)))
				  (* h (+ (funcall fx x0) (funcall fx x1)) 1/2))))
		(otherwise (if (typep method 'function)
			       method
			       (error "NUMERICAL-INTEGRATE: method 提供错误!")))))
	(ans 0))
    (dotimes (i step-num ans)
      (let* ((x0 (+ x-beg (* step i)))
	     (x1 (+ x0 step)))
	(incf ans (funcall func fx x0 x1))))))
