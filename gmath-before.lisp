(defgeneric explain (object &key)
  (:documentation "This generic function act as print to print out a readable text about the object."))

(defgeneric setel (object new-value place)
  (:documentation "This generic function act as setf to set the value of a element in a matrix or a table."))

(defun value-at (sentence var value)
  (eval `(let ((,var ,value))
	   ,sentence)))
