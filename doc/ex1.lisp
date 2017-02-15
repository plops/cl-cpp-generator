(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro with-glfw-window ((win &key (w 512) (h 512) (title "glfw")) &body body)
  `(let ((,win :type GLFWwindow*))
     (if (! (funcall glfwInit))
         (statements (return -1)))
     (setf ,win (funcall glfwCreateWindow ,w ,h (string ,title) NULL NULL))
     (if (! ,win)
         (statements (funcall glfwTerminate)
                     (return -1)))
     (funcall glfwMakeContextCurrent ,win)
     ,@body
     (funcall glfwTerminate)))

(defmacro with-gl-primitive (prim &body body)
  `(statements
    (funcall glBegin ,prim)
    ,@body
    (funcall glEnd)))


(progn
 (with-open-file (s "emitted_code.cpp"
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
    :code 
    `(with-compilation-unit
	 (include "GLFW/glfw3.h")
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (decl ((argc :type (void))))
		 (macroexpand
		  (with-glfw-window (main_window :w 512 :h 512)
		    (funcall glfwSetWindowTitle main_window (aref argv 0))
		    #+black-on-white
		    (statements 
		     (funcall glClearColor 1.0 1.0 1.0 1.0)
		     (funcall glColor4f 0.0 0.0 0.0 1.0)
		     (funcall glLineWidth 6.0))
		    (for (() (! (funcall glfwWindowShouldClose main_window)) ())
			 (funcall glClear GL_COLOR_BUFFER_BIT)
			 (funcall glLoadIdentity)
			 (let ((count :type "static float" :init 0.0))
			   (funcall glRotatef (+= count 2.1) .0 .0 1.))
			 
			 (macroexpand (with-gl-primitive GL_LINE_LOOP
					,@(let ((n 10))
					       (loop for i below n collect
						    (let ((arg (coerce (/ (* 2 pi i) n)
								       'single-float)))
						     `(funcall glVertex2f ,(cos arg) ,(sin arg)))))))
                       
			 (funcall glfwSwapBuffers main_window)
                    
			 (funcall glfwPollEvents))))
		 (return 0)))))
 (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring "emitted_code.cpp"))))
