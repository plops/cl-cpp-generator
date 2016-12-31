;; http://www.sbcl.org/manual/ 16.3 sb-cover
(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data
		   (speed 0)
		   (safety 3)
		   (debug 3)))

(compile-file "cp.lisp")
(load "cp.fasl")

(in-package :cl-cpp-generator)

(defun clang-format (str &optional
			   (fn "/dev/shm/o.cpp")
			   )
  (with-open-file (s fn
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-sequence str s))
  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/dev/shm/o.cpp"))
  (sleep .1)
  (with-open-file (s fn)
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))


 
(defun test (num code string)
  (if (string=
	   (clang-format (emit-cpp :str nil :code code))
	   (clang-format string)
	   )
      num
      (progn
	(clang-format (emit-cpp :str nil :code code) "/dev/shm/1")
	(clang-format string "/dev/shm/2")
	(assert (eq nil
		   (with-output-to-string (s)
		     (sb-ext:run-program "/usr/bin/diff" '("/dev/shm/1" "/dev/shm/2")
					 :output s)))))))


(progn	;; for loop
  (test 0
   '(for ((i a :type int) (< i n) (+= i 1))
     (+= b q))
   "for (int i = a; (i < n); i += 1) {
b += q;
}
")
  (test 1
   '(for (() (< i n) (+= i 1))
     (+= b q))
   "for (; (i < n); i += 1) {
b += q;
}
")
  (test 2
   '(for ((i a :type int) () (+= i 1))
     (+= b q))
   "for (int i = a; ; i += 1) {
b += q;
}
")
  (test 3
   '(for ((i a :type int) (< i n) ())
     (+= b q))
   "for (int i = a; (i < n);) {
b += q;
}
")
  (test 4
   '(for ((i a) (< i n) ())
     (+= b q))
   "for (auto i = a; (i < n);) {
b += q;
}
"))

(progn ;; if
  (test 5
   '(if (== a b) (+= a b) (-= a b))
   "if ((a == b)) {
  a += b;
}
else {
  a -= b;
}
")
  (test 6 
   '(if (== a b) (+= a b))
   "if ((a == b)) {
  a += b;
}
"))

(progn ;; setf
  (test 7 '(setf q (+ 1 2 3) l (+ 1 2 3))
	"q = (1 + 2 + 3);
l = (1 + 2 + 3);
")
  (test 8 '(setf q (+ 1 2 3))
	"q = (1 + 2 + 3);")
  )

(progn ;; decl
  (test 9 '(decl ((i :type int :init 0)
		(f :type float :init 3.2s-7)
		(d :type double :init 7.2d-31)
		(z :type "complex float" :init #.(complex 2s0 1s0))
		(w :type "complex double" :init #.(complex 2d0 1d0))))
	"int i = 0;
float f = (3.2000000000f-7);
double d = (7.200000000000000000e-31);
complex float z = ((2.0000000000f+0) + (1.0000000000f+0i));
complex double w = ((2.000000000000000000e+0) + (1.000000000000000000e+0i));
"))

(progn ;; let
  (test 10 '(let ((i :type int :init 0)
			(f :type float :init 3.2s-7)
			(d :type double :init 7.2d-31)
			(z :type "complex float" :init #.(complex 2s0 1s0))
			(w :type "complex double" :init #.(complex 2d0 1d0)))
			   (setf i (+ f d) j (- 3 j))
	  (+= j (+ 32 q)))
	"{
  int i = 0;
float f = (3.2000000000f-7);
double d = (7.200000000000000000e-31);
complex float z = ((2.0000000000f+0) + (1.0000000000f+0i));
complex double w = ((2.000000000000000000e+0) + (1.000000000000000000e+0i));

  i = (f + d);
j = (3 - j);

  j += (32 + q);
}
")
  )

(progn ;; computed assignment with complicated variable
  (test 11 '(+= "a::b" 3) "a::b += 3"))

(progn ;; class, struct and union; function declaration
 (test 12 '(with-compilation-unit
	 (include <stdio.h>)
	 (include "bla.h")
	 (with-namespace N
	   (class "gug::senso" ()
	    (access-specifier public)
	    (function (f ((a :type int)) int :specifier const)
		      (return i))
	    (function (h ((a :type int)) int))
	    (access-specifier private)
	    (function (f2
		       ((a :type int)) int))
	    (function (h2 ((a :type int)) int))
	    (decl ((i :type int)
		   (resetBus :type "Reset::BusCb"))))
	   (class sensor ("public p::pipeline"
			  "virtual public qqw::q"
			  "virtual public qq::q")
	    (function (sensor ((a :type char))))
	    (decl ((j :type int))))
	   (union "lag::sensor2" ("private p::pipeline2")
	    (decl ((j :type int)
		   (f :type float))))
	   (struct "lag::sensor2" ("private p::pipeline2")
	    (access-specifier public)
	    (decl ((j :type int)
		   (f :type float))))
		  
		  
	   ))
       "#include <stdio.h>
#include \"bla.h\"
namespace N {
class gug::senso {
public:

int f(int a) const{
  return i;
}

int h(int a);
private:

int f2(int a);
int h2(int a);
int i;
Reset::BusCb resetBus;

};

class sensor  : public p::pipeline, virtual public qqw::q, virtual public qq::q{
sensor(char a);
int j;

};

union lag::sensor2  : private p::pipeline2{
int j;
float f;

};

struct lag::sensor2  : private p::pipeline2{
public:

int j;
float f;

};

 } // namespace N

"))


(progn ;; function definition
  (test 13 '(function (g ((a :type char)
		       (b :type int*)) "complex double::blub")
	 (decl ((q :init b)))
	 (setf  "blub::q" (+ 1 2 3)
	  l (+ 1 2 3)))
       "complex double::blub g(char a,int* b){
  auto q = b;

  blub::q = (1 + 2 + 3);
l = (1 + 2 + 3);

}
")
  ;; constructor with initializers
  (test 14 '(function (bla ((a :type char)
				  (b :type int*)) ()
				  :ctor ((a 3)
				   (sendToSensorCb sendToSensorCb_)))
		   (+= a b)
	  )
	"bla(char a,int* b): a( 3 ), sendToSensorCb( sendToSensorCb_ )
{
  a += b;
}
"))

(progn ;; function call
  (test 15 '(function (g ((a :type char)
		       (b :type int*)) "complex double::blub")
	 (funcall add a b))
	"complex double ::blub g(char a, int *b) { add(a, b); }
"))

#+nil
(emit-cpp :str nil :code  '(with-compilation-unit
	 (include <stdio.h>)
	 (include "bla.h")
	 (with-namespace N
	   (class "gug::senso" ()
	    (access-specifier public)
	    (function (f ((a :type int)) int :specifier const)
		      (return i))
	    (function (h ((a :type int)) int))
	    (access-specifier private)
	    (function (f2
		       ((a :type int)) int))
	    (function (h2 ((a :type int)) int))
	    (decl ((i :type int)
		   (resetBus :type "Reset::BusCb"))))
	   (class sensor ("public p::pipeline"
			  "virtual public qqw::q"
			  "virtual public qq::q")
	    (function (sensor ((a :type char))))
	    (decl ((j :type int))))
	   (union "lag::sensor2" ("private p::pipeline2")
	    (decl ((j :type int)
		   (f :type float))))
	   (struct "lag::sensor2" ("private p::pipeline2")
	    (access-specifier public)
	    (decl ((j :type int)
		   (f :type float))))
		  
		  
	   )))



(sb-cover:report "/home/martin/stage/cl-cpp-generator/cover/")

#+nil
(sb-cover:reset-coverage)


