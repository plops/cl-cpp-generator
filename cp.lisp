(defpackage :cl-cpp-generator
  (:use :cl)
  (:export
   #:include
   #:compound-statement
   #:statements
   #:tagbody
   #:go
   #:defmacro
   #:macroexpand
   #:function
   #:access-specifier
   #:with-namespace
   #:with-compilation-unit
   #:enum
   #:decl
   #:break
   #:extern-c
   #:raw
   #:cast
   #:ns
   #:slot->value
   #:ref
   #:deref
   #:hex
   #:comma-list
   #:lisp
   #:statement))

(defpackage :cl-cpp-generator-macros
  (:use :cl :cl-cpp-generator))
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

(defun print-sufficient-digits-f32 (f)
  "print a single floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'single-float))
	 (s (format nil "~E" ff)))
   (assert (= 0s0 (- ff
		     (read-from-string s))))
   (substitute #\e #\f s)))

(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'double-float))
	 (s (format nil "~E" ff)))
   (assert (= 0s0 (- ff
		     (read-from-string s))))
   (substitute #\e #\d s)))

(defun get-all-macros (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (fboundp s)
	(when (macro-function s)
	 (if package
	     (when (eql (symbol-package s) package)
	      
	       (push s lst))
	     (push s lst)))))
    lst))

#+nil
(get-all-macros 'cl-cpp-generator)


(defparameter *env-functions* nil)
(defparameter *env-macros* nil)
(defun emit-cpp (&key code (str nil) (clear-env nil))
  (when clear-env
    (setf *env-functions* nil
	  *env-macros* nil))
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
	 (statements (with-output-to-string (s)
		       (loop for e in (cdr code) do
			    (format s "  ~a~%"  (emit-cpp :code (append '(statement) e))))
		       ))
	 (tagbody (with-output-to-string (s)
		  (format s "{~%")
		  (loop for e in (cdr code) do
		       (if (symbolp e)
			   (format s " ~a:~%" e)
			   (format s "  ~a~%"  (emit-cpp :code (append '(statement) e)))))
		  (format s "}~%")))
	 (go (destructuring-bind (name) (cdr code)
	       (format str "goto ~a" name)))
	 (defmacro (destructuring-bind ((name params) &rest macro-body) (cdr code)
		     (push (list :name name
				 :params params
				 :body macro-body)
			   *env-macros*)))
	 (macroexpand (destructuring-bind (macro &rest rest) (cdr code)
			(format str "~a" #+nil (intern (string-upcase (format nil "~a" macro)) :cl-cpp-generator-macros)
				(emit-cpp :code (macroexpand-1 macro)
						   
						   ))))
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
			   (progn
			     (push (list :name name
					 :params params
					 :ret ret
					 :ctor ctor
					 :specifier specifier
					 :body function-body)
				   *env-functions*)
			     (concatenate 'string
					  header
					  (emit-cpp :code `(compound-statement ,@function-body))))
			   (concatenate 'string header ";")))))
	 (access-specifier (format str "~a:~%" (cadr code))
			   ;; public, private or protected
			   )
	 (with-namespace (destructuring-bind (ns &rest compound-statement) (cdr code)
			   (format str "namespace ~a {~%~{~a~%~} } // namespace ~a ~%"
				   ns (loop for e in compound-statement collect 
					   (emit-cpp :code e))
				   ns)))
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
	 #+ispc (foreach (destructuring-bind (head &rest body) (cdr code) 
			   (if (listp (car head))
			       (progn  ;; foreach (i = 0 ... width, j = 1 .. height) {
				 (format str "foreach(~{~a~^,~}) ~a"
					 (loop for (var start n) in head collect
					      (format nil "~a = ~a ... ~a"
						      var (emit-cpp :code start) (emit-cpp :code n)))
					 (emit-cpp :code `(compound-statement ,@body)))
				 )
			       (destructuring-bind (var start n) head ;; foreach (i = 0 ... width) {
				(format str "foreach(~a = ~a ... ~a) ~a"
					var (emit-cpp :code start) (emit-cpp :code n)
					(emit-cpp :code `(compound-statement ,@body)))))
			   ))
	 #+ispc (foreach-active (destructuring-bind ((var) &rest body) (cdr code) ;; foreach_active (i) {
				  (format str "foreach_active(~a) ~a"
				   var
				   (emit-cpp :code `(compound-statement ,@body)))))

	 #+ispc (foreach-unique (destructuring-bind ((var seq) &rest body) (cdr code) ;; foreach_unique (val in x) {			  
				  (format str "foreach_uniq(~a in ~a) ~a"
					  var
					  (emit-cpp :code seq)
					  (emit-cpp :code `(compound-statement ,@body)))))

	 
	 (dotimes (destructuring-bind ((var n) &rest body) (cdr code)
		    (emit-cpp :code `(for ((,var 0 :type int) (< ,var ,(emit-cpp :code n)) (+= ,var 1))
					  ,@body))))
	 (if (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
	       (with-output-to-string (s)
		 (format s "if ( ~a ) ~a"
			 (emit-cpp :code condition)
			 (emit-cpp :code `(compound-statement ,true-statement)))
		 (when false-statement
		  (format s "else ~a"
			  (emit-cpp :code `(compound-statement ,false-statement)))))))
	 #+ispc (cif (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
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
	 (cast (destructuring-bind (type expr) (cdr code)
		 (format str "(( ~a ) ( ~a ))" type (emit-cpp :code expr))))
	 (ns  (let* ((args (cdr code))
		     (nss (butlast args))
		     (member (car (last args))))
		(with-output-to-string (s)
		  (loop for e in nss do
		       (format s "~a::" (emit-cpp :code e)))
		  (format s "~a"  (emit-cpp :code member)))))
	 (slot-value  (let* ((args (cdr code))
			     (objs (butlast args))
			     (member (car (last args))))
			(with-output-to-string (s)
			  (loop for e in objs do
			       (format s "~a." (emit-cpp :code e)))
			  (format s "~a"  (emit-cpp :code member)))))
	 (slot->value  (let* ((args (cdr code))
			(objs (butlast args))
			(member (car (last args))))
		   (with-output-to-string (s)
		     (loop for e in objs do
			  (format s "~a->" (emit-cpp :code e)))
		     (format s "~a"  (emit-cpp :code member)))))
	 (ref  (destructuring-bind (object) (cdr code)
		 (format str "(&(~a))" (emit-cpp :code object))))
	 (deref  (destructuring-bind (object) (cdr code)
		   (format str "(*(~a))" (emit-cpp :code object))))
	 (hex (destructuring-bind (number) (cdr code)
		(format str "0x~x" number)))
	 (string (destructuring-bind (string) (cdr code)
		   (format str "\"~a\"" string)))
	 (string+len (destructuring-bind (string) (cdr code)
		       (format str "\"~a\", ~a" string (length string))))
	 (array+lenbytes (destructuring-bind (name) (cdr code)
			   (format str "~a, ( 2 * sizeof( ~a ) )" name name)))
	 (list (destructuring-bind (&rest rest) (cdr code)
		 (format str "{~{~a~^,~}}" (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (comma-list (destructuring-bind (&rest rest) (cdr code)
		  (format str "~{~a~^,~}" (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (lisp (eval (cadr code)))
	 (statement ;; add semicolon
	  (cond ((member (second code) (append *binary-operator-symbol*
					       *computed-assignment-operator-symbol*
					       *logical-operator-symbol*
					       '(= return funcall raw go break)))
		 ;; add semicolon to expressions
		 (format str "~a;" (emit-cpp :code (cdr code))))
		((member (second code) '(if for dotimes compound-statement statements with-compilation-unit tagbody decl setf lisp case let macroexpand))
		 ;; if for, .. don't need semicolon
		 (emit-cpp :code (cdr code)))
		(t (format nil "not processable statement: ~a" code))))
	 
	 (t (cond ((and (= 2 (length code)) (member (car code)  '(- ~ !)))
		      ;; handle unary operators, i.e. - ~ !, this code
		      ;; needs to be placed before binary - operator!
		  (destructuring-bind (op operand) code
		    (format nil "(~a (~a))"
			    op
			    (emit-cpp :code operand))))
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
		  #+nil ((member (car code) (get-all-macros 'cl-cpp-generator-macros))
			 ;; if it is a macro in the cl-cpp-generator package, then expand it
			 (emit-cpp :code `(macroexpand ,@code)))
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
		       (single-float (format str "(~a)" (print-sufficient-digits-f32 code)))
		       (double-float (format str "(~a)" (print-sufficient-digits-f64 code)))))
		    ((complexp code)
		     (typecase (realpart code)
		       (single-float
			(substitute #\f #\e
				       (format str "((~a) + (~ai))"
					       (print-sufficient-digits-f32 (realpart code))
					       (print-sufficient-digits-f32 (imagpart code)))))
		       (double-float
			(substitute #\e #\d
				    (format nil "((~a) + (~ai))"
					    (print-sufficient-digits-f64 (realpart code))
					    (print-sufficient-digits-f64 (imagpart code)))))))))
	 ))
      ""))



#+nil
(defmacro with-c-file ((f fn) &body body)
  `(let ((,f :type FILE :init (funcall fopen ,fn)
	   ))
     ,@body))


#+nil
(with-output-to-string (s)
  (emit-cpp
   :str s
   :clear-env t
   
   :code 
   `(with-compilation-unit
	(dotimes (i (funcall max 2 3))
	  (funcall bla))
      (foreach (i (funcall max  1 0) (funcall min m n))
	       (funcall ata))
      (foreach ((i (funcall max  1 0) (funcall min m n))
		(j 0 n))
	       (funcall ata))
      (foreach-active (i)
		      (+= (aref a index) 1))
      (function (func ((v :type "uniform int")) "extern void"))
      (foreach-unique (val x)
	       (funcall func val)))))

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

#+nil
(emit-cpp :code '(with-compilation-unit (dotimes (i 12)
					  (if (== i 3)
					      (statements
					       (funcall (ns std (ns b b)) i)
					       (funcall (arrow  (ns a b c) c) i))
					      (statements
					       (funcall (ref b) i)
					       (funcall (dot (aref c 3) (deref a) c) i))))))

#+nil
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


