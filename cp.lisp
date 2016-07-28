(defpackage :g
  (:use :cl))
(in-package :g)

(setf (readtable-case *readtable*) :invert)


;; OOP A%3A C++-Grammatik (mit Links).html
#|
include arg
 arg either keyword like <stdio.h> or a string

function name params* ret expr1 expr2 ... 
name .. function name
parameters .. 0 or more but always a list
ret .. return value

class identifier base-clause
identifier .. class name like dfa%%flash
base-clause .. (()) or ((public virtual buh%%fcsdf)) or ((public virtual buh%%fcsdf) (private B::C))

|#

#+nil
(trace emit-cpp)

(defun lisp->c (name)
  (substitute #\_ #\-
	      (substitute #\: #\% (format nil "~a" name))))

(defun ensure-list (x)
  "Make sure x is in a list."
  (if (listp x)
      x
      (list x)))

(defun emit-function-header (str name params ret)
  (format str "~{~a~^ ~} ~a(~{~a~^,~})"
	  (mapcar #'lisp->c (ensure-list ret))
	  name
	  (emit-cpp :code `(:params ,params))))

(defun emit-cpp (&key code (str nil))
  (if code
      (if (listp code)
       (case (car code)
	 (:params (loop for e in (cadr code) collect
		       (destructuring-bind (name &key type) e
			 (format str "~a ~a" type name))))
	 (include (format str "#include ~s" (cadr code)))
	 (function (destructuring-bind (name params ret &rest rest) (cdr code)
		     (concatenate 'string
				  (emit-function-header str name params ret)
				  (format str "{~a~%}~%"  (emit-cpp :code rest)))))
	 (functiond (destructuring-bind (name params ret) (cdr code)
		      (concatenate 'string
				   (emit-function-header str name params ret)
				   ";")))
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
			   (format str "namespace ~a {~%~{~a~%~} };~%"
				   ns (loop for e in block collect 
					   (emit-cpp :code e)))))
	 (with-compilation-unit (format str "~{~a~%~}"
				 (loop for e in (cdr code) collect 
				      (emit-cpp :code e))))
	 (decl (destructuring-bind (bindings) (cdr code)
		 (with-output-to-string (s)
		   (loop for e  in bindings do
			(destructuring-bind (name &key (type 'auto) init) e
			  (format s "~{~a~^ ~} ~a"
				  (mapcar #'lisp->c (ensure-list type))
				  (lisp->c name))
			  (if init
			      (format s " = ~a" (emit-cpp :code init)))
			  (format s ";~%"))))))
	 #+nil (let (destructuring-bind (bindings &rest block) (cdr code)
		      (format str "~{~a~%~}~%"
			      (loop for e in block collect 
				   (emit-cpp :code e)))))
	 )
       (cond ((numberp code)
	      (cond ((integerp code) (format str "~a" code))
		    ((floatp code)
		     (typecase code
		       (single-float (substitute #\f #\e (format str "~,10e" code)))
		       (double-float (substitute #\e #\d (format str "~,18e" code)))))
		    ((complexp code)
		     (typecase (realpart code)
		       (single-float
			(substitute #\f #\e
				       (format str "~,10e+~,10ei"
					       (realpart code) (imagpart code))))
		       (double-float
			(substitute #\e #\d
				       (format nil "~,18e+~,18ei"
					       (realpart code) (imagpart code))))))))))
      ""))



#+Nil
(untrace format)
(progn
  (with-open-file (s "/home/martin/stage/cl-cpp-generator/o.cpp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		(include <stdio.h>)
		(include "bla.h")
		(with-namespace N
		   (class gug%%senso ()
		   (access-specifier public)
		   (functiond f ((a :type int)) int)
		   (functiond h ((a :type int)) int)
		   (access-specifier private)
		   (functiond f2 ((a :type int)) int)
		   (functiond h2 ((a :type int)) int))
		  (class sensor ((public p%%pipeline)
		  		 (virtual public qqw%%q)
				 (virtual public qqw%%q)))
		  (class lag%%sensor2 ((private p%%pipeline2)))
		  (decl ((i :type int :init 0)
			 (f :type float :init 3.2s-7)
			 (d :type double :init 7.2d-31)
			 (z :type (complex float) :init #.(complex 2s0 1s0))
			 (w :type (complex double) :init #.(complex 2d0 1d0))))
		  (function g ((a :type char)
		  	       (b :type int*)) (complex double%%blub))
		  ))))
 ;(sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/stage/cl-cpp-generator/o.cpp"))
  )
