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
	       ;; beware of third party C++ libraries that count on
	       ;; exception been thrown and do not check return value
	       ;; from perator new
	       (function (new ((size :type size_t)) "void* operator" :specifier noexcept)
		(return (funcall malloc size)))
	       (function (delete ((p :type void*)) "void operator" :specifier noexcept)
		(return (funcall free p)))
	       (function (new[] ((size :type size_t)) "void* operator" :specifier noexcept)
		(return (funcall malloc size)))
	       (function (delete[] ((p :type void*)) "void operator" :specifier noexcept)
		(return (funcall free p)))
	       (function (new ((size :type size_t) (b :type "std::nothrow_t")) "void* operator" :specifier noexcept)
	       	(return (funcall malloc size)))
	       (function (delete ((p :type void*)  (b :type "std::nothrow_t")) "void operator" :specifier noexcept)
	       	(return (funcall free p)))
	       (function (new[] ((size :type size_t)  (b :type "std::nothrow_t")) "void* operator" :specifier noexcept)
	       	(return (funcall malloc size)))
	       (function (delete[] ((p :type void*)  (b :type "std::nothrow_t")) "void operator" :specifier noexcept)
	       	(return (funcall free p)))

	       (function (main ((argc :type int)
				(argv :type "const char**"))
			  int)
		(decl ((v :type "std::vector<int>")
		       (max-vec-size :type "static const int" :init 256)))
		
		(for ((i 0 :type int) (< i max-vec-size) (+= i 1))
		 (funcall v.push-back i))
		(return 0))))
;; removing standard library and c++ runtime
;; update interrupt vector table
;; set up stack pointers for all modes of execution
;; zero .bss section
;; call initialisation functions for global objects
;; call main
;; perhaps define memcpy and memset


(compile-cpp "/home/martin/stage/cl-cpp-generator/out/test_cpp_statics" ;; static objects
	     '(with-compilation-unit
	       (class SomeObj ()

		(access-specifier public)
		(function (instanceGlobal () "static SomeObj&"))
		(function (instanceLocal () "static SomeObj&"))
		(access-specifier private)
		(function (SomeObj ((v1 :type int)
				    (v2 :type int))))
		(decl ((m_v1 :type int)
		       (m_v2 :type int)
		       (globalObj :type "static SomeObj"))))
	       (decl (("SomeObj::globalObj(1,2)" :type SomeObj)))
	       (function ("SomeObj::instanceGlobal" () SomeObj&)
		(return globalObj))
	       (function ("SomeObj::instanceLocal" () SomeObj&)
		(decl (("localObj(3,4)" :type "static SomeObj")))
		(return localObj))
	       (function ("SomeObj::SomeObj" ((v1 :type int)
					      (v2 :type int))
			  () :ctor ((m_v1 v1) (m_v2 v2)))
		(decl ((a :type int))))
	       (function (main ((argc :type int)
				(argv :type "const char**"))
			  int)
		(decl ((glob :type "auto&" :init "SomeObj::instanceGlobal()")
		       (local :type "auto&" :init "SomeObj::instanceLocal()")))
		(for (() () ()))
		(return 0))))
