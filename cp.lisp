(defpackage :g
  (:use :cl))
(in-package :g)

(setf (readtable-case *readtable*) :invert)


;; OOP A%3A C++-Grammatik (mit Links).html
(defparameter *special-symbol*
  '(! &=  ++    ->      /=      <<=     >>    |\||
    !=  |(|     +=      ->*     |:|     <=      >>=     |\|=|
    %   |)|     |,|     |.|     |::|    =       ?       ||||
    %=  *       -       |.*|    |;|     ==      ^       }
    &   *=      --      |...|   <       >       ^=      ~
    &&  +       -=      /       <<      >=      {))

(defparameter *unary-operator-symbol*
  '(+ - ~ !))

(defparameter *binary-operator-symbol*
  '(* / % + - ^ & |\|| << >>))

(defparameter *logical-operator-symbol*
  '(== != < > <= >= && ||||))

(defparameter *computed-assignment-operator-symbol*
  '(*= /= %= += -= ^= &= |\|=| <<= >>=))

(defparameter *class-key*
  '(class struct union))

#+nil
(trace emit-cpp)

(defun emit-function-header (str name params ret)
  (format str "~a ~a(~{~a~^,~})"
	  ret
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
	 (block (with-output-to-string (s)
		  (format s "{~%")
		  (loop for e in (cdr code) do
		       (format s "  ~a;~%"  (emit-cpp :code e)))
		  (format s "~%}~%")))
	 (function (destructuring-bind (name params ret &rest rest) (cdr code)
		     (concatenate 'string
				  (emit-function-header str name params ret)
				  (emit-cpp :code `(block ,@rest)))))
	 (functiond (destructuring-bind (name params ret) (cdr code)
		      (concatenate 'string
				   (emit-function-header str name params ret)
				   ";")))
	 (access-specifier (format str "~a:~%" (cadr code))
			   ;; public, private or protected
			   )
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
			  (format s "~a ~a"
				  type name)
			  (if init
			      (format s " = ~a" (emit-cpp :code init)))
			  (format s ";~%"))))))
	 (setf (with-output-to-string (s)
		 (let ((args (cdr code)))
		  (loop for i below (- (length args) 2) by 2 do
		       (format s "~a = ~a;"
			       (emit-cpp :code (elt args i))
			       (emit-cpp :code (elt args (1+ i)))))
		  (format s "~a = ~a"
			       (emit-cpp :code (elt args (- (length args) 2)))
			       (emit-cpp :code (elt args (- (length args) 1)))))
		 ))
	 #+nil (let (destructuring-bind (bindings &rest block) (cdr code)
		      (format str "~{~a~%~}~%"
			      (loop for e in block collect 
				   (emit-cpp :code e)))))
	 (t (cond ((member (car code) *binary-operator-symbol*)
		   ;; handle binary operators
		   (with-output-to-string (s)
		     (format s "(")
		     (loop for e in (cdr code)
			and i below (1- (length (cdr code))) do
			  (format s "~a ~a " (emit-cpp :code e) (car code)))
		     (format s "~a)" (emit-cpp :code (car (last (cdr code)))))))
		  ((member (car code) *class-key*)
		   ;; handle class, struct or union definitions
		   (destructuring-bind (class-key identifier base-clause &rest member-specification) code
		  (with-output-to-string (s)
		    (format s "~a ~a " class-key identifier)
		    (when base-clause
		      (format s " :~{ ~a~^,~}" base-clause))
		    (format s "{~{~a~%~}~%}~%" (loop for e in member-specification collect
						    (emit-cpp :code e))))))
		  ((member (car code) *computed-assignment-operator-symbol*)
		   ;; handle computed assignment
		   (format str "~a ~a ~a"
			   (emit-cpp :code (second code))
			   (car code) ;; assignment operator
			   (emit-cpp :code (third code))))
		  (t (format nil "not processable: ~a" code)))))
       (cond
	 ((symbolp code)
	  (format str "~a" code))
	 ((numberp code) ;; print constants
	      (cond ((integerp code) (format str "~a" code))
		    ((floatp code)
		     (typecase code
		       (single-float (substitute #\f #\e (format str "(~,10e)" code)))
		       (double-float (substitute #\e #\d (format str "(~,18e)" code)))))
		    ((complexp code)
		     (typecase (realpart code)
		       (single-float
			(substitute #\f #\e
				       (format str "((~,10e) + (~,10ei))"
					       (realpart code) (imagpart code))))
		       (double-float
			(substitute #\e #\d
				       (format nil "((~,18e) + (~,18ei))"
					       (realpart code) (imagpart code))))))))))
      ""))



#+Nil
(untrace format)
(progn
  (with-open-file (s "/home/martin/stage/cl-cpp-generator/o.cpp"
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		(include <stdio.h>)
		(include "bla.h")
		(with-namespace N
		   (class "gug::senso" ()
		   (access-specifier public)
		   (functiond f ((a :type int)) int)
		   (functiond h ((a :type int)) int)
		   (access-specifier private)
		   (functiond f2 ((a :type int)) int)
		   (functiond h2 ((a :type int)) int))
		  (class sensor ("public p::pipeline"
				 "virtual public qqw::q"
				 "virtual public qq::q"))
		  (union "lag::sensor2" ("private p::pipeline2"))
		  (decl ((i :type int :init 0)
			 (f :type float :init 3.2s-7)
			 (d :type double :init 7.2d-31)
			 (z :type "complex float" :init #.(complex 2s0 1s0))
			 (w :type "complex double" :init #.(complex 2d0 1d0))))
		  (function g ((a :type char)
		  	       (b :type int*)) "complex double::blub"
		   (block
		    (setf  q (+ 1 2 3)
			   l (+ 1 2 3))
		     (+= m (* 2 3 4)))
		   (setf b (* (/ 3 (+ 32 3)) 2 3 (+ 2 (/ 13 (+ 2 39))))))
		  ))))
 (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/stage/cl-cpp-generator/o.cpp"))
  )
