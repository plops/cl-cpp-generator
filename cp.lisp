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

(defun emit-cpp (&key code (str nil))
  (if code
      (if (listp code)
       (case (car code)
	 (:params (loop for e in (cadr code) collect
		       (destructuring-bind (name &key type) e
			 (format str "~a ~a" type name))))
	 (include (format str "#include ~s" (cadr code)))
	 (compound-statement (with-output-to-string (s)
		  (format s "{~%")
		  (loop for e in (cdr code) do
		       (format s "  ~a~%"  (emit-cpp :code (append '(statement) e))))
		  (format s "}~%")))
	 (function (destructuring-bind ((name params &optional ret &key ctor specifier) &rest function-body) (cdr code)
		     (let ((header (concatenate 'string
						(when ret (format nil "~a " ret))
						(format nil "~a(~{~a~^,~})"
							name
							(emit-cpp :code `(:params ,params)))
						(when specifier
						  (format nil " ~a" specifier))
						(when ctor
						  (format nil ":~{~a~^,~}~%"
							  (loop for (e f) in ctor collect
							       (format nil " ~a( ~a )" e f)))))))
		       (if function-body
			   (concatenate 'string
					header
					(emit-cpp :code `(compound-statement ,@function-body)))
			   (concatenate 'string header ";")))))
	 (access-specifier (format str "~a:~%" (cadr code))
			   ;; public, private or protected
			   )
	 (with-namespace (destructuring-bind (ns &rest compound-statement) (cdr code)
			   (format str "namespace ~a {~%~{~a~%~} };~%"
				   ns (loop for e in compound-statement collect 
					   (emit-cpp :code e)))))
	 (with-compilation-unit (format str "~{~a~%~}"
				 (loop for e in (cdr code) collect 
				      (emit-cpp :code e))))
	 (decl (destructuring-bind (bindings) (cdr code)
		 (with-output-to-string (s)
		   (loop for e  in bindings do
			(destructuring-bind (name &key (type 'auto) init) e
			  (format s "~a ~a"
				  type (emit-cpp :code name))
			  (if init
			      (format s " = ~a" (emit-cpp :code init)))
			  (format s ";~%"))))))
	 (let (destructuring-bind (bindings &rest rest) (cdr code)
		(emit-cpp :code `(compound-statement
				     (decl ,bindings)
				   ,@rest))))
	 (for (destructuring-bind ((for-init-statement &optional condition-opt update-expression-opt) &rest statement-list)
		  (cdr code)
		(format str "for(~a; ~a; ~a) ~a"
			(if for-init-statement
			    (destructuring-bind (name init &key (type 'auto)) for-init-statement
			      (format nil "~a ~a = ~a" type name (emit-cpp :code init)))
			    "")
			(if condition-opt
			    (emit-cpp :code condition-opt)
			    "")
			(if update-expression-opt
			    (emit-cpp :code update-expression-opt)
			    "")
			(emit-cpp :code `(compound-statement ,@statement-list)))))
	 (if (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
	       (with-output-to-string (s)
		 (format s "if (~a) ~a"
			 (emit-cpp :code condition)
			 (emit-cpp :code `(compound-statement ,true-statement)))
		 (when false-statement
		  (format s "else ~a"
			  (emit-cpp :code `(compound-statement ,false-statement)))))))
	 (setf (with-output-to-string (s)
		 ;; handle multiple assignments
		 (let ((args (cdr code)))
		   (loop for i below (length args) by 2 do
			(format s "~a~%"
				(emit-cpp :code `(statement = ,(elt args i) ,(elt args (1+ i)))))))))
	 (return (format str "return ~a"
			 (emit-cpp :code (second code))))
	 (funcall (destructuring-bind (name &rest rest) (cdr code)
		      (format str "~a(~{~a~^,~})"
			      (emit-cpp :code name)
			      (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))

	 (statement ;; add semicolon
	  (cond ((member (second code) (append *binary-operator-symbol*
					       *computed-assignment-operator-symbol*
					       *logical-operator-symbol*
					       '(= return funcall)))
		 ;; add semicolon to expressions
		 (format str "~a;" (emit-cpp :code (cdr code))))
		((member (second code) '(if for compound-statement decl setf))
		 ;; if for, .. don't need semicolon
		 (emit-cpp :code (cdr code)))
		(t (format nil "not processable statement: ~a" code))))
	 
	 (t (cond ((member (car code) *binary-operator-symbol*)
		   ;; handle binary operators
		   ;; no semicolon
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
		    (format s "{~%~{~a~%~}};~%" (loop for e in member-specification collect
						    (emit-cpp :code e))))))
		  ((member (car code) (append '(=) *computed-assignment-operator-symbol*))
		   ;; handle assignment and computed assignment, i.e. =, +=, /=, ...
		   (destructuring-bind (op lvalue rvalue) code
		    (format str "~a ~a ~a"
			    (emit-cpp :code lvalue)
			    op
			    (emit-cpp :code rvalue))))
		  ((member (car code)  *logical-operator-symbol*)
		   ;; handle logical operators, i.e. ==, &&, ...
		   (destructuring-bind (op left right) code
		    (format str "~a ~a ~a"
			    (emit-cpp :code left)
			    op
			    (emit-cpp :code right))))
		  (t (format nil "not processable: ~a" code)))))
       (cond
	 ((or (symbolp code)
	      (stringp code)) ;; print variable
	  (substitute #\_ #\- (format nil "~a" code)))
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

#+nil
(with-open-file (s "/home/martin/stage/cl-cpp-generator/o.cpp"
		   :direction :output :if-exists :supersede :if-does-not-exist :create)
  (emit-cpp :str s :code
	      '(with-compilation-unit
		(for ((i a :type int) (< i n) (+= i 1))
		 (+= b q))
		)))

(defun compile-cpp (fn code)
  (let ((source-fn (concatenate 'string fn ".cpp"))
	(bin-fn (concatenate 'string fn ".bin")))
   (with-open-file (s source-fn
		      :direction :output :if-exists :supersede :if-does-not-exist :create)
     (emit-cpp :str s :code code))
   (sb-ext:run-program "/usr/bin/clang-format" `("-i" ,source-fn))
   (sleep .1)
   (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/g++" `("-fno-exceptions" "-nostdlib" "-fno-unwind-tables" "-fno-rtti" "-march=native" "-o" ,bin-fn  "-Os" ,source-fn)
			:output s :error :output))
;   (sleep .1)
;   (sb-ext:run-program "/usr/bin/objdump" `("-DS" ,bin-fn))
   ))





#+nil
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
		    (function (f ((a :type int)) int))
		    (function (h ((a :type int)) int))
		   (access-specifier private)
		    (function ("senso" ((a :type int))))
		    (function (h2 ((a :type int)) int)))
		  (class sensor ("public p::pipeline"
				 "virtual public qqw::q"
				 "virtual public qq::q"))
		  
		  (decl ((i :type int :init 0)
			 (f :type float :init 3.2s-7)
			 (d :type double :init 7.2d-31)
			 (z :type "complex float" :init #.(complex 2s0 1s0))
			 (w :type "complex double" :init #.(complex 2d0 1d0))))
		  (union "lag::sensor2" ("private p::pipeline2"))
		  (let ((i :type int :init 0)
			(f :type float :init 3.2s-7)
			(d :type double :init 7.2d-31)
			(z :type "complex float" :init #.(complex 2s0 1s0))
			(w :type "complex double" :init #.(complex 2d0 1d0)))
		    (setf i (+ f d)))
		  (function (g ((a :type char)
				(b :type int*)) "complex double::blub")
		   (compound-statement
		    (setf  q (+ 1 2 3)
			   l (+ 1 2 3))
		     (compound-statement
			 (if (== a b)
			     (+ a b)
			     (- a b))
		       (if (< b q)
			   (*= b q))))
		   (setf b (* (/ 3 (+ 32 3)) 2 3 (+ 2 (/ 13 (+ 2 39)))))
		   (for ((i a :type int) (< i n) (+= i 1))
			(+= b q))
		   (for (() (< i n) (+= i 1))
			(+= b q)))
		  (function (bla ((a :type char)
				  (b :type int*)) ()
				  ((a 3)
				   (sendToSensorCb sendToSensorCb_)))
		   (+= a b)
		   )
		  ))))
;  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/stage/cl-cpp-generator/o.cpp"))
  )


