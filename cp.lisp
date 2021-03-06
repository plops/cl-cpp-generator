;;;; by Martin Kielhorn <kielhorn.martin@gmail.com>, 2016-2017.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



(defpackage :cl-cpp-generator
  (:use :cl)
  (:export
   #:include
   #:compound-statement
   #:single-float-to-c-hex-string
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


(defparameter *file-hashes* (make-hash-table))


(defun beautify-source (code)
  (let* ((code-str (emit-cpp
		   :clear-env t
		   :code code)))
    (with-input-from-string (s code-str)
      (with-output-to-string (o)
	(sb-ext:run-program "/usr/bin/clang-format" (list "-") :input s :output o :wait t)))))

(defun single-float-to-c-hex-string (f)
  (declare (type (single-float 0) f))
  (multiple-value-bind (a b c) (integer-decode-float f)
  (let ((significand (ash a 1)))
    (format nil "0x~x.~xp~d"
	    (ldb (byte 4 (* 6 4)) significand)
	    (ldb (byte (* 6 4) 0) significand)
	   (+ 23 b)))))

(defun write-source (name extension code &optional (dir (user-homedir-pathname)))
  (let* ((fn (merge-pathnames (format nil "~a.~a" name extension)
			      dir))
	(code-str (emit-cpp
		   :clear-env t
		   :code code))
	(fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
     (when (or (not exists) (/= code-hash old-code-hash))
       ;; store the sxhash of the c source in the hash table
       ;; *file-hashes* with the key formed by the sxhash of the full
       ;; pathname
       (setf (gethash fn-hash *file-hashes*) code-hash)
       (with-open-file (s fn
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	 (write-sequence code-str s))
      (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring fn)))))))

;; OOP A%3A C++-Grammatik (mit Links).html
(defparameter *special-symbol*
  '(! &=  ++    ->      /=      <<=     >>    |\||
    !=  |(|     +=      ->*     |:|     <=      >>=     |\|=|
    %   |)|     |,|     |.|     |::|    =       ?       |\|\||
    %=  *       -       |.*|    |;|     ==      ^       }
    &   *=      --      |...|   <       >       ^=      ~
    &&  +       -=      /       <<      >=      {))

(defparameter *unary-operator-symbol*
  '(+ - ~ !))

(defparameter *binary-operator-symbol*
  '(* / % + - ^ & |\|| << >>))

(defparameter *logical-operator-symbol*
  '(== != < > <= >= && |\|\||))

(defparameter *computed-assignment-operator-symbol*
  '(*= /= %= += -= ^= &= |\|=| <<= >>=))

(defparameter *class-key*
  '(class struct union))

(defun print-sufficient-digits-f32 (f)
  "print a single floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'single-float))
	 (s (format nil "~E" ff)))
    #+nil   (assert (= 0s0 (- ff
			      (read-from-string s))))
    (assert (< (abs (- ff
		       (read-from-string s)))
	       1d-4))
   (format nil "~af" s)))

#+nil
(print-sufficient-digits-f32 1s0)

(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'double-float))
	 (s (format nil "~E" ff)))
    #+nil (assert (= 0d0 (- ff
			    (read-from-string s))))
    (assert (< (abs (- ff
		       (read-from-string s)))
	       1d-12))
   (substitute #\e #\d s)))

#+nil
(print-sufficient-digits-f64 1d0)





(defparameter *env-functions* nil)
(defparameter *env-macros* nil)

(defun emit-cpp (&key code (str nil) (clear-env nil) )
  (when clear-env
    (setf *env-functions* nil
	  *env-macros* nil))
  (if code
      (if (listp code)
       (case (car code)
	 (:params (loop for e in (cadr code) collect
		       (destructuring-bind (name &key type default) e
			 (if default
			     (format str "~a ~a = ~a" type name (emit-cpp :code default))
			     (format str "~a ~a" type name)))))
	 (include (format str "#include ~s" (cadr code)))
	 (compound-statement (with-output-to-string (s)
			       (format s "{~%")
			       (loop for e in (cdr code) do
				    (format s "~&  ~a"  (emit-cpp :code (append '(statement) e))))
			       (format s "}")))
	 (statements (with-output-to-string (s)
		       (loop for e in (cdr code) do
			    (format s "~&  ~a"  (emit-cpp :code (append '(statement) e))))))
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
	 (function (destructuring-bind ((name params &optional ret &key ctor specifier parent-ctor) &rest function-body) (cdr code)
		     (let ((header (concatenate 'string
						(when ret (format nil "~a " ret))
						(format nil "~a(~{~a~^,~})"
							name
							(emit-cpp :code `(:params ,params)))
						(when specifier
						  (format nil " ~a" specifier))
						(when (or ctor parent-ctor)
						  (format nil ":~{~a~^,~}~%"
							  (append
							   (loop for (e f) in ctor collect
								(format nil " ~a( ~a )" e f))
							   (loop for e  in parent-ctor collect 
								(destructuring-bind (name &rest params) e
								  (format nil "~a"
									  (emit-cpp :code `(with-compilation-unit
											       (funcall ,name ,@params))))))))))))
		       (if function-body 
			   (progn
			     (push (list :name name
					 :params params
					 :ret ret
					 :ctor ctor
					 :specifier specifier
					 :parent-ctor parent-ctor
					 :body function-body)
				   *env-functions*)
			     (concatenate 'string
					  header 
					  (emit-cpp :code `(compound-statement ,@function-body))))
			   (concatenate 'string header ";")))))
	 (lambda (destructuring-bind ((params &key captures ret specifiers) &rest function-body) (cdr code)
		     (let ((header (concatenate 'string
						
						(format nil "[~{~a~^,~}](~{~a~^,~})"
							captures
							(emit-cpp :code `(:params ,params)))
						(when specifiers
						  (format nil "~{ ~a~}" specifiers))
						(when ret (format nil "~a " ret)))))
		       (concatenate 'string
				    header
				    (emit-cpp :code `(compound-statement ,@function-body))))))
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
	 (enum-class (destructuring-bind ((name &key type) &rest rest) (cdr code)
		 ;; C++11
		 (with-output-to-string (s)
		   (format s "enum class ~a ~a {~{ ~a~^,~}};~%"
			   (if name name "")
			   (if type (format nil ": ~a" type) "")
			   (loop for e in rest collect
				(if (listp e)
				    (format nil "~a = ~a" (first e) (emit-cpp :code (second e)))
				    (format nil "~a" e)))))))
	 (new (format str "new ~a" (emit-cpp :code (cadr code))))
	 (delete (format str "delete ~a" (emit-cpp :code (cadr code))))
	 (delete[] (format str "delete [] ~a" (emit-cpp :code (cadr code))))
	 (decl (destructuring-bind (bindings) (cdr code)
		 (with-output-to-string (s)
		   (loop for e  in bindings do
			(destructuring-bind (name &key (type 'auto) init ctor extra) e
			  (format s "~a ~a"
				  type (emit-cpp :code name))
			  (if extra
			      (format s "~a" (emit-cpp :code extra)))
			  (if init
			      (format s " = ~a" (emit-cpp :code init))
			      (if ctor
				  (format s "( ~a )" (emit-cpp :code ctor))
				  ))
			  (format s ";~%")
			  )))))
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

	 (while (destructuring-bind (condition &rest statement-list)
		  (cdr code)
		(format str "while(~a) ~a"
			(emit-cpp :code condition)
			(emit-cpp :code `(compound-statement ,@statement-list)))))
	 (do-while (destructuring-bind (condition &rest statement-list)
		  (cdr code)
		  (format str "do ~a while ( ~a )"
			  (emit-cpp :code `(compound-statement ,@statement-list))
			  (emit-cpp :code condition)
			)))
	 
	 #-conly
	 (for-range (destructuring-bind ((var-decl range) &rest statement-list)
		  (cdr code)
		(format str "for(~a : ~a) ~a"
			(if (atom var-decl)
			    (format nil "auto ~a" var-decl)
			    (destructuring-bind (name &key (type 'auto)) var-decl
			      (format nil "~a ~a" type name)))
			(emit-cpp :code range)
			(emit-cpp :code `(compound-statement ,@statement-list)))))
	 ;#+ispc
	 (foreach (destructuring-bind (head &rest body) (cdr code) 
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
					(emit-cpp :code `(compound-statement ,@body)))))))
	 ;#+ispc
	 (foreach-tiled (destructuring-bind (head &rest body) (cdr code)  ;; same semantics as foreach
			   (if (listp (car head))
			       (progn  ;; foreach_tiled (i = 0 ... width, j = 1 .. height) {
				 (format str "foreach_tiled(~{~a~^,~}) ~a"
					 (loop for (var start n) in head collect
					      (format nil "~a = ~a ... ~a"
						      var (emit-cpp :code start) (emit-cpp :code n)))
					 (emit-cpp :code `(compound-statement ,@body)))
				 )
			       (destructuring-bind (var start n) head ;; foreach_tiled (i = 0 ... width) {
				(format str "foreach_tiled(~a = ~a ... ~a) ~a"
					var (emit-cpp :code start) (emit-cpp :code n)
					(emit-cpp :code `(compound-statement ,@body)))))))
	 ;#+ispc
	 (foreach-active (destructuring-bind ((var) &rest body) (cdr code) ;; foreach_active (i) {
				  (format str "foreach_active(~a) ~a"
				   var
				   (emit-cpp :code `(compound-statement ,@body)))))

	 ;#+ispc
	 (foreach-unique (destructuring-bind ((var seq) &rest body) (cdr code) ;; foreach_unique (val in x) {			  
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
	 (? (destructuring-bind (condition true-statement &optional false-statement) (cdr code) ;; ternery if, note: user should supply multiple statements as a comma-list
	       (with-output-to-string (s)
		 (format s "( ~a ) ? ( ~a )"
			 (emit-cpp :code condition)
			 (emit-cpp :code true-statement))
		 (when false-statement
		  (format s ": ( ~a )"
			  (emit-cpp :code false-statement))))))
	 ;#+ispc
	 (cif (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
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
	 #+nil (aref (destructuring-bind (name &optional n) (cdr code)
		 (if n
		     (format str "~a[~a]" name (emit-cpp :code n))
		     (format str "~a[]" name))))
	 (aref (destructuring-bind (name &rest rest) (cdr code)
		 (if rest
		     (format str "~a~{[~a]~}" (emit-cpp :code name) (mapcar #'(lambda (x) (emit-cpp :code x)) rest))
		     (format str "~a[]" (emit-cpp :code name)))))
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
		(typecase number
		  (single-float
		   (format str "~a" (single-float-to-c-hex-string number)))
		  (number
		   (format str "0x~x" number)))))
	 (char (destructuring-bind (a) (cdr code)
		 (typecase a
		   (standard-char (format str "'~a'" a))
		   (number (format str "'~a'" (code-char a)))
		   (string (format str "'~a'" (elt a 0))))
		 ))
	 ;#+ispc
	 (bit (destructuring-bind (number) (cdr code)
		       (format str "0b~b" number)))
	 (string (destructuring-bind (string) (cdr code)
		   (format str "\"~a\"" string)))
	 (string+len (destructuring-bind (string) (cdr code)
		       (format str "\"~a\", ~a" string (length string))))
	 (array+lenbytes (destructuring-bind (name) (cdr code)
			   (format str "~a, ( 2 * sizeof( ~a ) )" name name)))
	 (list (destructuring-bind (&rest rest) (cdr code)
		 (format str "{~{~a~^,~}}" (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (paren-list (destructuring-bind (&rest rest) (cdr code)
		 (format str "(~{~a~^,~})" (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (comma-list (destructuring-bind (&rest rest) (cdr code)
		       (format str "~{~a~^,~}" (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (make-instance (destructuring-bind (object &rest rest) (cdr code)
		  (format str "~a{~{~a~^,~}}" object (mapcar #'(lambda (x) (emit-cpp :code x)) rest))))
	 (lisp (eval (cadr code)))
	 (statement ;; add semicolon
	  (cond ((member (second code) (append *binary-operator-symbol*
					       *computed-assignment-operator-symbol*
					       *logical-operator-symbol*
					       '(= return funcall raw go break new delete delete[] ? do-while slot-value)))
		 ;; add semicolon to expressions
		 (format str "~a;" (emit-cpp :code (cdr code))))
		((member (second code) '(if for-range while for foreach foreach-unique foreach-tiled foreach-active dotimes compound-statement statements with-compilation-unit tagbody decl setf lisp case let macroexpand struct class union function))
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
			(format str "((~a) + (~ai))"
				(print-sufficient-digits-f32 (realpart code))
				(print-sufficient-digits-f32 (imagpart code))))
		       (double-float
			(format nil "((~a) + (~ai))"
				(print-sufficient-digits-f64 (realpart code))
				(print-sufficient-digits-f64 (imagpart code))))))))
	 ))
      ""))


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

