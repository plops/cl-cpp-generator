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


(string=
 (clang-format 
  (emit-cpp :str nil :code
	    '(with-compilation-unit
	      (for ((i a :type int) (< i n) (+= i 1))
	       (+= b q)))))
 (clang-format
  "for (int i = a; i < n; i += 1) {
b += q;
}
"))

(assert
 (string= 
  (with-output-to-string (s)
    (emit-cpp :str s :code
	      '(with-compilation-unit
		(for ((i a :type int) (< i n) (+= i 1))
		 (+= b q))
		)))
  "for(int i = a; i < n; i += 1) { b += q; }"))

(sb-cover:report "/home/martin/stage/cl-cpp-generator/cover/")
