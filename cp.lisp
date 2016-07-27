(ql:quickload :optima)

(defpackage :g (:use :cl :optima))
(in-package :g)

(setf (readtable-case *readtable*) :invert)

(defparameter *out* *standard-output*)

(defmacro w (&rest rest)
  `(format *out* "~{~a ~}" ',rest))

(defun namespace (ns &rest rest)
  (w namespace ns { (w rest) }))

(let ((N))
 (namespace N
	    ))



(emit-cpp 
 (with-namespace N
   (function g ((a :type char)) int
	     (return (+ 1 a)))
   (let ((i :type int)))))


(progn
  (with-open-file (s "o.cpp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		(with-namespace N
		  (function g ((a :type char)
			       (b :type int*)) int)))))
  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/o.cpp")))




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
				(emit-cpp :code (cdr code)))))
      ""))
