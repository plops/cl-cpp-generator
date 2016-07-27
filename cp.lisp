(defpackage :g
  (:use :cl))
(in-package :g)

(setf (readtable-case *readtable*) :invert)





#+nil
(trace emit-cpp)

(defun emit-cpp (&key code (str nil))
  (if code
      (case (car code)
	(:params (loop for e in (cadr code) collect
		     (destructuring-bind (name &key type) e
		       (format str "~a ~a" type name))))
	(function (destructuring-bind (name params ret &rest rest) (cdr code)
		    (format str "~a ~a(~{~a~^,~}){~a}"
			    ret name
			    (emit-cpp :code `(:params ,params))
			    (emit-cpp :code rest))))
	(with-namespace (destructuring-bind (ns &optional block) (cdr code)
			  (format str "namespace ~a { ~a };~%"
				  ns (emit-cpp :code block))))
	(with-compilation-unit (format str "~a~%"
				(emit-cpp :code (cadr code)))))
      ""))


(progn
  (with-open-file (s "/home/martin/stage/cl-cpp-generator/o.cpp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		(with-namespace N
		  (function g ((a :type char)
			       (b :type int*)) int)))))
  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/stage/cl-cpp-generator/o.cpp")))
