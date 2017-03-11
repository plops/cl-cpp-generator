;; http://www.sbcl.org/manual/ 16.3 sb-cover. load this file with C-c
;; C-l in slime if there are assert errors, the tests fail the section
;; "How to fix a broke test" in README.org explains how the tests can
;; be corrected.

;; if all tests run successfull, you can see a code coverage report in
;; coverage/*.html. This will tell which code lines were
;; checked. Please note that highlighted green in this report not
;; necessarily means that all cases are checked, e.g format calls with
;; ~^ in the format string will need to be checked with different
;; arguments. Any calls of member may also need many tests for full
;; coverage.
(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data
		   (speed 0)
		   (safety 3)
		   (debug 3)))
(push :ispc *features*) ;; for now i have to open cp.lisp and compile it again with C-c C-k, so that foreach works
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
float f = (3.2e-7);
double d = (7.2e-31);
complex float z = ((2.f+0) + (1.f+0i));
complex double w = ((2.e+0) + (1.e+0i));
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
float f = (3.2e-7);
double d = (7.2e-31);
complex float z = ((2.f+0) + (1.f+0i));
complex double w = ((2.e+0) + (1.e+0i));

  i = (f + d);j = (3 - j);

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


(progn ;; default parameter
  (test 16 `(function (blah ((a :type int :default 3)))
		      (raw "// "))
	"blah(int a = 3){
  // ;
}
"))

(progn ;; terniary operator
  (test 17
   `(statements (? a b c)
	       (? (<  a b) x)
	       (? (&& (<= 0 h) (< h 24))
		  (= hour h)
		  (comma-list (<< cout (string "bla")) (= hour 0))
		  ))
   "( a ) ? ( b ): ( c );
  ( (a < b) ) ? ( x );
  ( ((0 <= h) && (h < 24)) ) ? ( hour = h ): ( (cout << \"bla\"),hour = 0 );
"))


(progn
  (test 18 ;; for-range (c++)
	`(statements
	  (for-range (e (funcall make_iterator_range (string ".") (list ))))
	  (for-range ((e :type "auto&&") (funcall make_iterator_range (string ".") (list )))))
	"  for(auto e : make_iterator_range(\".\",{})) {
}

  for(auto&& e : make_iterator_range(\".\",{})) {
}
"))


(progn
  (test 19 ;; new, delete[]
	`(let ((buf :type "unsigned char*" :init (new (aref "unsigned char" size))))
	   (delete[] buf))
	"{
  unsigned char* buf = new unsigned char[size];

  delete [] buf;
}
")
  (test 20 ;; new, delete
	`(let ((buf :type "unsigned char*" :init (new "unsigned char")))
	   (delete buf))
	"{
  unsigned char* buf = new unsigned char;

  delete buf;
}
"))

(progn
  (test 21 ;; alignment on 64 byte boundary
	`(let (((aref buf (* width height)) :type "static int" :extra (raw " __attribute__((aligned(64)))"))))
	"{
  static int buf[(width * height)] __attribute__((aligned(64)));

}
"))

(progn
  (test 22 ;; enum class
	`(with-compilation-unit
	     (enum-class (ProtocolType) IP ICMP RAW)
	   (enum-class (fruit :type uint8_t) apple melon))
	"enum class ProtocolType  { IP, ICMP, RAW};

enum class fruit : uint8_t { apple, melon};
"))

(progn
  (test 23 ;; lambda (c++)
	`(lambda (((i :type int)) :ret "->int")  )
	"[](int i)->int {
}
"))

(progn
  (test 24 ;; do-while while
	`(statements
	  (while (< 1 a) (+= 1 a) (setf a b))
	  (do-while (< 1 a) (+= 1 a) (setf a b)))
	"  while((1 < a)) {
  1 += a;
  a = b;
}

  do {
  1 += a;
  a = b;
}
 while ( (1 < a) );
"))

(progn
  (test 25 ;; ||
	`(if (|\|\|| a b)
	    (statements (funcall bal)))
	"if ( (a || b) ) {
    bal();

}
"))

(progn
  (test 26 ;; ispc
	`(with-compilation-unit
	  (dotimes (i (funcall max 2 3))
	    (funcall bla))
	  (foreach (i (funcall max  1 0) (funcall min m n))
		   (funcall ata))
	  (foreach ((i (funcall max  1 0) (funcall min m n))
		    (j 0 n))
		   (funcall ata))
	  (foreach-active (i)
			  (+= (aref a index) (bit #b0110)))
	  (function (func ((v :type "uniform int")) "extern void"))
	  (foreach-unique (val x)
			  (funcall func val))
	  (let ((dx :type float :init (/ (- x1 x0) width))
		(dy :type float :init (/ (- y1 y0) height))
		)
	    (foreach (i (funcall max  1 0) (funcall min m n))
		     (funcall ata))
	    #+nil (foreach (i 0 width)
			   (let ((x :type float :init (+ x0 (* i dx)))
				 (y :type float :init (+ y0 (* i dy)))
				 (index :type int :init (+ i (* j width)))
				 )
			     (setf (aref output index) (funcall mandel x y #+nil max_iterations))))))
	"for(unsigned int i = 0; (i < max(2,3)); i += 1) {
  bla();
}

foreach(i = max(1,0) ... min(m,n)) {
  ata();
}

foreach(i = max(1,0) ... min(m,n),j = 0 ... n) {
  ata();
}

foreach_active(i) {
  a[index] += 0b110;
}

extern void func(uniform int v);
foreach_uniq(val in x) {
  func(val);
}

{
  float dx = ((x1 - x0) / width);
float dy = ((y1 - y0) / height);

  foreach(i = max(1,0) ... min(m,n)) {
  ata();
}

}
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


