					;几何部分

(defgeneric area (figure)
  (:documentation "This generic function can get the area of 2-dimentional figure."))

(defgeneric leng (figure)
  (:documentation "This generic function can get the length of 1-dimentional figure."))
(defmethod leng ((figure list))
  (let* ((v1 (first figure))
	 (v2 (second figure))
	 (x1 (first v1))
	 (x2 (first v2))
	 (y1 (second v1))
	 (y2 (second v2))
	 (z1 (if (= 3 (length v1)) (third v1) 0))
	 (z2 (if (= 3 (length v2)) (third v2) 0)))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2) (expt (- z1 z2) 2))))) 

(defgeneric correct-figure (figure)
  (:documentation "This generic function can correct the slot of figure."))

(defclass figure ()
  ((dimention
    :initarg :dimention
    :initform 2
    :accessor dimention)
   (color
    :initarg :color
    :initform 'white
    :accessor color)
   (equations
    :initarg :equations
    :initform nil-equations
    :accessor equations)))
(defmethod explain ((figure figure) &key (io-stream t))
  (format io-stream "Dimention: ~d" (dimention figure))
  (print (color figure) io-stream)
  (format io-stream "~%")
  (explain (equations figure))
  figure)

(defclass circle (figure)
  ((center
    :initarg :center
    :initform (error "Must supply initial value: center of circle!")
    :accessor center)
   (radius
    :initarg :radius
    :initform 1
    :accessor radius)))
(defmethod area ((object circle))
  (ne '(pi * (radius object) expt 2)))
(defmethod correct-figure ((object circle))
  (setf (dimention object) 2)
  (setf (equations object) (gather-eqs (make-instance 'equation :varlist '(x y)
								:content `(+ (expt (- x ,(first (center object))) 2)
									     (expt (- y ,(second (center object))) 2)
									     (- ,(expt (radius object) 2))))))
  object)
(defmethod explain ((figure circle) &key (io-stream t))
  (format io-stream "Center: ~A~%" (center figure))
  (format io-stream "Radius: ~d~%" (radius figure))
  (call-next-method))
(defmethod leng ((figure circle))
  (* 2 pi (radius figure)))
(defun circle (x y r)
  (correct-figure (make-instance 'circle :center (list x y) :radius r)))

(defclass poligon (figure)
  ((side
    :initarg :side
    :initform 3
    :accessor side)
   (vertices
    :initarg :vertices
    :initform (error "Must supply initial value: vertices list")
    :accessor vertices)))
(defmethod explain ((figure poligon) &key (io-stream t))
  (format io-stream "Amount of sides: ~d~%" (side figure))
  (format io-stream "list of vertices: ~A~%" (vertices figure))
  (call-next-method))
(defmethod area ((figure poligon))
  (let* ((ans 0)
	 (p-list (vertices figure))
	 (p1 (first p-list))
	 (tri-s (lambda (v1 v2)
		  (/ (- (* (first v1) (second v2))
			(* (second v1) (first v2)))
		     2))))
    (dotimes (i (- (length p-list) 2) (abs ans))
      (let ((v1 (vector_- (nth (1+ i) p-list) p1))
	    (v2 (vector_- (nth (+ i 2) p-list) (nth (1+ i) p-list))))
	(incf ans (funcall tri-s v1 v2))))))
(defmethod leng ((figure poligon))
  (let ((ans (leng (list (first (vertices figure)) (nth (1- (side figure)) (vertices figure))))))
    (dotimes (i (1- (side figure)) ans)
      (incf ans (leng (list (nth i (vertices figure)) (nth (1+ i) (vertices figure))))))))
(defun triangle (x1 y1 x2 y2 x3 y3)
  (make-instance 'poligon :vertices (list (list x1 y1) (list x2 y2) (list x3 y3))))

(defclass segment (figure)
  ((vertices
    :initarg :vertices
    :initform (error "Must supply initial value: vertices list of segment")
    :accessor vertices)))
(defmethod leng ((object segment))
  (let* ((v1 (first (vertices object)))
	 (v2 (second (vertices object)))
	 (v (vector_- v1 v2)))
    (sqrt (+ (expt (first v) 2) (expt (second v) 2)))))
(defmethod explain ((object segment) &key (io-stream t))
  (format io-stream "List of vertices: ~A~%" (vertices object))
  (call-next-method))
(defmethod correct-figure ((figure segment))
  (setf (dimention figure) 1)
  (let* ((v1 (first (vertices figure)))
	 (v2 (second (vertices figure)))
	 (x1 (first v1))
	 (x2 (first v2))
	 (y1 (second v1))
	 (y2 (second v2)))
    (if (= x1 x2)
	(setf (equations figure) (gather-eqs (make-instance 'equation :content `(- x ,x1) :varlist '(x))))
	(let ((k (/ (- y1 y2) (- x1 x2)))
	      (b (- y1 (/ (- y1 y2) (- x1 x2)))))
	  (setf (equations figure) (gather-eqs (make-instance 'equation :content `(+ (* ,k x) ,b (- y))
									:varlist '(x y)))))))
  figure)

(defun new-figure (&rest class-and-init)
  (correct-figure (apply #'make-instance (pop class-and-init) class-and-init)))






