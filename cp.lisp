(defpackage :g
  (:use :cl))
(in-package :g)

(setf (readtable-case *readtable*) :invert)


;; OOP A%3A C++-Grammatik (mit Links).html


#+nil
(trace emit-cpp)

(defun lisp->c (name)
  (substitute #\_ #\-
   (substitute #\: #\% (format nil "~a" name))))

(defun emit-cpp (&key code (str nil))
  (if code
      (case (car code)
	(:params (loop for e in (cadr code) collect
		     (destructuring-bind (name &key type) e
		       (format str "~a ~a" type name))))
	(include (format str "#include ~a" (cadr code)))
	(function (destructuring-bind (name params ret &rest rest) (cdr code)
		    (format str "~a ~a(~{~a~^,~}){~a}"
			    ret name
			    (emit-cpp :code `(:params ,params))
			    (emit-cpp :code rest))))
	(functiond (destructuring-bind (name params ret) (cdr code)
		    (format str "~a ~a(~{~a~^,~});"
			    ret name
			    (emit-cpp :code `(:params ,params)))))
	(access-specifier (format str "~a:~%" (cadr code))
			  ;; public, private or protected
			  )
	(class (destructuring-bind (class-key identifier base-clause &rest member-specification) code
		 (with-output-to-string (s)
		   (format s "~a ~a " class-key (lisp->c identifier))
		   (when base-clause
		     (format s " : ~{~{~a ~}~^,~}" (loop for e in base-clause
						    collect (mapcar #'lisp->c e))))
		   (format s "{~{~a~%~}~%}~%" (loop for e in member-specification collect
						 (emit-cpp :code e))))))
	(with-namespace (destructuring-bind (ns &rest block) (cdr code)
			  (format str "namespace ~a {~% ~{ ~a~%~} };~%"
				  ns (loop for e in block collect 
					  (emit-cpp :code e)))))
	(with-compilation-unit (format str "~{~a~%~}"
				(loop for e in (cdr code) collect 
				     (emit-cpp :code e)))))
      ""))

#+Nil
(untrace format)
(progn
  (with-open-file (s "/home/martin/stage/cl-cpp-generator/o.cpp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		;(include <stdio.h>)
		(with-namespace N
		  #+nil (class gug%%senso ()
		   (access-specifier public)
		   (functiond f ((a :type int)) int)
		   (functiond h ((a :type int)) int)
		   (access-specifier private)
		   (functiond f2 ((a :type int)) int)
		   (functiond h2 ((a :type int)) int))
		  (class sensor ((public p%%pipeline)
		  		 (virtual public qqw%%q)))
		  (class lag%%sensor2 ((private p%%pipeline2)))
		  (functiond g ((a :type char)
		  	       (b :type int*)) int)
		  ))))
  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/stage/cl-cpp-generator/o.cpp"))
  )
