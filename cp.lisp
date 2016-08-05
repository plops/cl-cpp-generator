(defpackage :cl-cpp-generator
  (:use :cl))
(in-package :cl-cpp-generator)

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
	 (tagbody (with-output-to-string (s)
		  (format s "{~%")
		  (loop for e in (cdr code) do
		       (if (symbolp e)
			   (format s " ~a:~%" e)
			   (format s "  ~a~%"  (emit-cpp :code (append '(statement) e)))))
		  (format s "}~%")))
	 (go (destructuring-bind (name) (cdr code)
	       (format str "goto ~a" name)))
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
	 (with-compilation-unit (format str "~{~a~^~%~}"
				 (loop for e in (cdr code) collect 
				      (emit-cpp :code e))))
	 (enum (destructuring-bind (name &rest rest) (cdr code)
		 ;; enum bla (normal 1) power-on (error (+ 1 2))
		 (with-output-to-string (s)
		   (format s "enum ~a {~{ ~a~^,~}};~%"
			   (if name name "")
			   (loop for e in rest collect
				(if (listp e)
				    (format nil "~a = ~a" (first e) (emit-cpp :code (second e)))
				    (format nil "~a" e)))))))
	 (decl (destructuring-bind (bindings) (cdr code)
		 (with-output-to-string (s)
		   (loop for e  in bindings do
			(destructuring-bind (name &key (type 'auto) init ctor) e
			  (format s "~a ~a"
				  type (emit-cpp :code name))
			  (if init
			      (format s " = ~a" (emit-cpp :code init))
			      (if ctor
				  (format s "( ~a )" (emit-cpp :code ctor))
				  ))
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
		 (format s "if ( ~a ) ~a"
			 (emit-cpp :code condition)
			 (emit-cpp :code `(compound-statement ,true-statement)))
		 (when false-statement
		  (format s "else ~a"
			  (emit-cpp :code `(compound-statement ,false-statement)))))))
	 (break (format str "break"))
	 (case (destructuring-bind (expr &rest cases) (cdr code)
		 (with-output-to-string (s)
		   (format s "switch ( ~a ) {~%" (emit-cpp :code expr))
		   (loop for e in cases do
			(destructuring-bind (const-expr &rest statements) e
			  (if (eq const-expr t)
			      (format s "default : ~a" (emit-cpp :code `(compound-statement ,@statements (break))))
			      (format s "case ~a : ~a"
				      (emit-cpp :code const-expr)
				      (emit-cpp :code `(compound-statement ,@statements (break))))))
			(format s ""))
		   (format s "}~%"))))
	 (setf (destructuring-bind (&rest args) (cdr code)
		(with-output-to-string (s)
		  ;; handle multiple assignments
		  (loop for i below (length args) by 2 do
		       (format s "~a"
			       (emit-cpp :code `(statement = ,(elt args i) ,(elt args (1+ i))))))
		  (if (< 2 (length args))
		      (format s "~%")))))
	 (aref (destructuring-bind (name &optional n) (cdr code)
		 (if n
		     (format str "~a[~a]" name (emit-cpp :code n))
		     (format str "~a[]" name))))
	 (return (format str "return ~a"
			 (emit-cpp :code (second code))))
	 (funcall (destructuring-bind (name &rest rest) (cdr code)
		      (format str "~a(~{~a~^,~})"
			      (emit-cpp :code name)
			      (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (extern-c (destructuring-bind (&rest rest) (cdr code)
		     (format str "extern \"C\" {~%~{~a~^~%~}} // extern \"C\"~%"
				 (loop for e in rest collect 
				      (emit-cpp :code e)))))
	 (raw (destructuring-bind (string) (cdr code)
		(format str "~a" string)))
	 (hex (destructuring-bind (number) (cdr code)
		(format str "0x~x" number)))
	 (string (destructuring-bind (string) (cdr code)
		   (format str "\"~a\"" string)))
	 (string+len (destructuring-bind (string) (cdr code)
		       (format str "\"~a\", ~a" string (length string))))
	 (list (destructuring-bind (&rest rest) (cdr code)
		 (format str "{~{~a~^,~}}" rest)))
	 (lisp (eval (cadr code)))
	 (statement ;; add semicolon
	  (cond ((member (second code) (append *binary-operator-symbol*
					       *computed-assignment-operator-symbol*
					       *logical-operator-symbol*
					       '(= return funcall raw go break)))
		 ;; add semicolon to expressions
		 (format str "~a;" (emit-cpp :code (cdr code))))
		((member (second code) '(if for compound-statement tagbody decl setf lisp case let))
		 ;; if for, .. don't need semicolon
		 (emit-cpp :code (cdr code)))
		(t (format nil "not processable statement: ~a" code))))
	 
	 (t (cond ((and (= 2 (length code)) (member (car code)  '(- ~ !)))
		      ;; handle unary operators, i.e. - ~ !, this code
		      ;; needs to be placed before binary - operator!
		  (destructuring-bind (op operand) code
		    (format nil "(~a (~a))"
			    op
			    (emit-cmd :code operand))))
		  ((member (car code) *binary-operator-symbol*)
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
		    (format str "(~a ~a ~a)"
			    (emit-cpp :code left)
			    op
			    (emit-cpp :code right))))
		  (t (format nil "not processable: ~a" code)))))
       (cond
	 ((or (symbolp code)
	      (stringp code)) ;; print variable
	  (format nil "~a" code)
	  ;(substitute #\_ #\- (format nil "~a" code))
	  )
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

(defun compile-cpp (fn code &key options)
  (let ((source-fn (concatenate 'string fn ".cpp"))
	(bin-fn (concatenate 'string fn ".bin")))
   (with-open-file (s source-fn
		      :direction :output :if-exists :supersede :if-does-not-exist :create)
     (emit-cpp :str s :code code))
   (sb-ext:run-program "/usr/bin/clang-format" `("-i" ,source-fn))
   (sleep .1)
   (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/g++" (append options `("-fno-exceptions" "-nostdlib" "-fno-unwind-tables" "-fno-rtti" "-march=native" "-o" ,bin-fn  "-Os" ,source-fn))
			:output s :error :output))
;   (sleep .1)
;   (sb-ext:run-program "/usr/bin/objdump" `("-DS" ,bin-fn))
   ))


;; interrupt routine doesn't have to save registers if no other functions are called
;; integer divide, modulus, multiply, bitwise and, or, xor and compare are not implemented in hardware
;; --ram-model loader doesn't need .cinit sectionx


(progn
  (with-open-file (s "/home/martin/stage/cl-cpp-generator/o.cpp"
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		(include <stdio.h>)
		(include "bla.h")
		(enum fsm (powerOff 1) (normal) (error (+ normal 3))) 
		(extern-c
		 (decl (((aref sdata 12) :type "extern Uint16")
			((aref rdata (+ 2 3)) :type "extern Uint16")
			((aref tdata) :type "extern Uint16" :init (list 3 1 4 1 5)))))
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
				  :ctor
				  ((a 3)
				   (sendToSensorCb sendToSensorCb_)))
		   (tagbody
		    start
		      (if (== a b)
			  (go start)
			  (go next))
		    next
		      (case q
			(3 (+= q 3))
			(4 (+= q 4)
			   (/= p 2))
			(t (/= p 1))))
		   )
		  ))))
  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/home/martin/stage/cl-cpp-generator/o.cpp"))
  )


