					;关系部分

(defclass equation ()
  ((varlist
    :initarg :varlist
    :initform (error "Must supply initial value: varlist")
    :accessor varlist)
   (content
    :initarg :content
    :initform (error "Must supply initial value: content of equation")
    :accessor content)))
(defmethod explain ((object equation) &key (io-stream t))
  (format io-stream "~A ,~%" (content object))
  (if (= 1 (length (varlist object)))
      (format io-stream "  where there is 1 variable: ~A~%" (varlist object))
      (format io-stream "  where there is ~d variables: ~A~%" (length (varlist object)) (varlist object)))
  object)

(defclass equations ()
  ((varlist
    :initarg :varlist
    :initform (error "Must supply initial value: varlist")
    :accessor varlist)
   (subeqs
    :initarg :subeqs
    :initform (error "Must supply initial value: subequations")
    :accessor subeqs)))
(defmethod explain ((equations equations) &key (io-stream t))
  (dotimes (i (length (subeqs equations)))
    (explain (nth i (subeqs equations)) :io-stream io-stream))
  (format io-stream "Above there is ~d variables: ~A~%" (length (varlist equations)) (varlist equations))
  equations)
(defun gather-eqs (&rest eqs)
  (let ((ans-eqs (make-instance 'equations :varlist nil :subeqs nil)))
    (dolist (p eqs ans-eqs)
      (push p (subeqs ans-eqs))
      (setf (varlist ans-eqs) (concatenate 'list (varlist p) (varlist ans-eqs)))
      (setf (varlist ans-eqs) (re-set (varlist ans-eqs))))))
(defconstant nil-equations (make-instance 'equations :subeqs nil :varlist nil))
