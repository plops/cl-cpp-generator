;; http://www.sbcl.org/manual/ 16.3 sb-cover
(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(compile-file "cp.lisp")
(load "cp.fasl")

(sb-cover:report "/home/martin/stage/cl-cpp-generator/cover/")
