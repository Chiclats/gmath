(defgeneric explain (object &key)
  (:documentation "This generic function act as print to print out a readable text about the object."))


(defun value-at (sentence var value)
  (eval `(let ((,var ,value))
	   ,sentence)))
