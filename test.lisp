;; http://www.sbcl.org/manual/ 16.3 sb-cover
(eval-when (:compile-toplevel)
  (require :sb-cover)

  (declaim (optimize sb-cover:store-coverage-data))

  (compile-file "cp.lisp")
  (load "cp.fasl"))

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

(progn  ;; for loop
  (test 
   '(with-compilation-unit
     (for ((i a :type int) (< i n) (+= i 1))
      (+= b q)))
   "for (int i = a; i < n; i += 1) {
b += q;
}
")


  (test 
   '(with-compilation-unit
     (for (() (< i n) (+= i 1))
      (+= b q)))
   "for (; i < n; i += 1) {
b += q;
}
")

  (test 
   '(with-compilation-unit
     (for ((i a :type int) () (+= i 1))
      (+= b q)))
   "for (int i = a; ; i += 1) {
b += q;
}
")

  (test 
   '(with-compilation-unit
     (for ((i a :type int) (< i n) ())
      (+= b q)))
   "for (int i = a; i < n;) {
b += q;
}
"))

(sb-cover:report "/home/martin/stage/cl-cpp-generator/cover/")

#+nil
(sb-cover:reset-coverage)
