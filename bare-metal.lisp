(in-package :g)

(compile-cpp "/home/martin/stage/cl-cpp-generator/out/simple" ;; simple
	     '(with-compilation-unit
	       (function (main ((argc :type int)
				(argv :type "const char**"))
			  int)
		
		(for (() () ())
		 )
		(return 0))))

(compile-cpp "/home/martin/stage/cl-cpp-generator/out/simple_vec" ;; dynamic memory allocation
	     '(with-compilation-unit
	       (include <vector>)
	       (include <cstddef>)
	       (function (main ((argc :type int)
				(argv :type "const char**"))
			  int)
		(decl ((v :type "std::vector<int>")
		       (max-vec-size :type "static const int" :init 256)))
		
		(for ((i 0 :type int) (< i max-vec-size) (+= i 1))
		 (funcall v.push-back i))
		(return 0))))

(compile-cpp "/home/martin/stage/cl-cpp-generator/out/simple_vec_c_heap" ;; replace C++ heap with C
	     '(with-compilation-unit
	       (include <vector>)
	       (include <cstddef>)
	       (include <cstdlib>)
	       (include <new>)
	       (function (new ((size :type size_t)) "void* operator" :specifier noexcept)
		(return (funcall malloc size)))
	       (function (delete ((p :type void*)) "void operator" :specifier noexcept)
		(return (funcall free p)))
	       (function (new[] ((size :type size_t)) "void* operator" :specifier noexcept)
		(return (funcall malloc size)))
	       (function (main ((argc :type int)
				(argv :type "const char**"))
			  int)
		(decl ((v :type "std::vector<int>")
		       (max-vec-size :type "static const int" :init 256)))
		
		(for ((i 0 :type int) (< i max-vec-size) (+= i 1))
		 (funcall v.push-back i))
		(return 0))))
