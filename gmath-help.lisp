(defun help (symbol-or-function)
  (case symbol-or-function
    ('*gmath-linearal-matrix-explain-ioformat*
     (print "全局变量，默认值为 \"~a~,8t\"")
     (print "用于控制矩阵表达时数据形式与数据间距")
     (print "源 gmath-linearAl")
     (print "函数定位 (explain MATRIX)"))

    ('differential
     (print "differential (sentence var &optional (time 1))")
     (print "对表达式 sentence 中变量 var 求符号微分, time 表示求微分的阶数")
     (print "源 gmath-calculus")
     (print "内置函数 gmath-calculus-differential-modify gmath-calculus-differential-main")
     t)

    ('differential-at
     (print "differential-at (sentence var value &optional (time 1))")
     (print "对表达式 sentence 中变量 var 求符号微分,并在 var=value 处求值, time 表示求微分的阶数")
     (print "源 gmath-calculus")
     (print "内置函数 differential")
     t)
    
    ('numerical-integrate
     (print "numerical-integrate (fx x-beg x-end step-num &optional (method 'simpson))")
     (print "对单变量匿名函数 fx 的从 x-beg 到 x-end 的数值积分,其中 step-num 为将区间分割的次数，要求为正整数.")
     (print "method 为数值积分方法,默认为 'simpson ,即用辛普森方法求积分.也可改为 'trapezoidal 梯形法则, 'mid 中点法则, 'bool 布尔法则.")
     (print "method 也可输入一个匿名函数作为参数 (lambda (fx x0 x1) formula) ,利用 formula 近似 fx 在 x0 到 x1 上的积分.")
     (print "源 gmath-calculus")
     t)

    ('value-at
     (print "value-at (sentence var value)")
     (print "求表达式 sentence 中变量 var=value 时表达式的值")
     (print "源 gmath-before")
     t)

    (otherwise (error "HELP: 未找到该名称的符号或函数!"))))

(defvar *gmath-help-list*)

(defun gmath-help-load-help-text ()
  (let ((f_i (open "d:/emacs-27.2-x86_64/programs/gmath/gmath-help-text" :if-does-not-exist :create)))
    (setq *gmath-help-list* (read f_i))))

(defun add-help (symbol doc)
  (push (cons symbol doc) *gmath-help-list*)
  (cons symbol doc))

(defun overwrite-help (symbol doc)
  (if (position symbol *gmath-help-list* :key 'car)
      (error "OVERWRITE-HELP: 未找到该符号"))
  (setf (nth (position symbol *gmath-help-list* :key 'car) *gmath-help-list*) (cons symbol doc)))

(defun help (symbol)
  (if (not (position symbol *gmath-help-list* :key 'car))
      nil
      (progn
	(print symbol)
	(print (cdr (nth (position symbol *gmath-help-list* :key 'car) *gmath-help-list*)))
	t)))
  
(gmath-help-load-help-text)
