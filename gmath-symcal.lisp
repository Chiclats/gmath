(setq *read-default-float-format* 'double-float)

(defun ftor (flt &optional enob)
  (if (typep flt 'list)
      (if (= (length flt) 2)
	  (+ (first flt) (/ (second flt) (1- (expt 10 (1+ (floor (log (second flt) 10)))))))
	  (+ (first flt)
	     (/ (second flt) (expt 10 (1+ (floor (log (second flt) 10)))))
	     (/ (third flt) (expt 10 (1+ (floor (log (second flt) 10)))) (1- (expt 10 (1+ (floor (log (third flt) 10))))))))))
