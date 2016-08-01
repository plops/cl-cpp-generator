;; http://www.sbcl.org/manual/ 16.3 sb-cover
(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(compile-file "cp.lisp")
(load "cp.fasl")

(in-package :g)

(defun clang-format (str)
  (with-open-file (s "/dev/shm/o.cpp"
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-sequence str s))
  (sb-ext:run-program "/usr/bin/clang-format" '("-i" "/dev/shm/o.cpp"))
  (sleep .1)
  (with-open-file (s "/dev/shm/o.cpp")
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))



(defun test (code string)
  (assert (string=
	   (clang-format (emit-cpp :str nil :code code))
	   (clang-format string))))
(progn	;; for loop
  (test 
   '(for ((i a :type int) (< i n) (+= i 1))
     (+= b q))
   "for (int i = a; i < n; i += 1) {
b += q;
}
")
  (test 
   '(for (() (< i n) (+= i 1))
     (+= b q))
   "for (; i < n; i += 1) {
b += q;
}
")
  (test 
   '(for ((i a :type int) () (+= i 1))
     (+= b q))
   "for (int i = a; ; i += 1) {
b += q;
}
")
  (test 
   '(for ((i a :type int) (< i n) ())
     (+= b q))
   "for (int i = a; i < n;) {
b += q;
}
")
  (test 
   '(for ((i a) (< i n) ())
     (+= b q))
   "for (auto i = a; i < n;) {
b += q;
}
"))

(progn ;; if
  (test
   '(if (== a b) (+= a b) (-= a b))
   "if (a == b) {
  a += b;
}
else {
  a -= b;
}
")
  (test
   '(if (== a b) (+= a b))
   "if (a == b) {
  a += b;
}
"))

(progn ;; setf
  (test '(setf q (+ 1 2 3) l (+ 1 2 3))
	"q = (1 + 2 + 3);
l = (1 + 2 + 3);
")
  (test '(setf q (+ 1 2 3))
	"q = (1 + 2 + 3);
")
  )

(progn ;; decl
  (test '(decl ((i :type int :init 0)
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

(emit-cpp :str nil :code '(let ((i :type int :init 0)
			(f :type float :init 3.2s-7)
			(d :type double :init 7.2d-31)
			(z :type "complex float" :init #.(complex 2s0 1s0))
			(w :type "complex double" :init #.(complex 2d0 1d0)))
		    (setf i (+ f d))) )

(sb-cover:report "/home/martin/stage/cl-cpp-generator/cover/")

#+nil
(sb-cover:reset-coverage)


