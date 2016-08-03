
;; spru513j.pdf page 195
(defun emit-cmd (&key code)
  (if code
      (if (listp code)
	  (case (car code)
	    (with-compilation-unit (format nil "~{~a~}"
				    (loop for e in (cdr code) collect 
					 (emit-cmd :code e))))
	    (raw (destructuring-bind (string) (cdr code)
		   (format nil "~a~%" string)))
	    (funcall (destructuring-bind (name &rest rest) (cdr code)
		       ;; supported functions in memory range entries:
		       ;; start(memory-range [, page=0])
		       ;; size(memory-range [, page=0])
		       ;; end(memory-range [, page=0])
		       (format nil "~a(~{~a~^,~})"
			       (emit-cmd :code name)
			       (mapcar #'(lambda (x) (emit-cmd :code x)) rest))))
	    (sections (destructuring-bind (&rest rest) (cdr code)
			;; describe how inpt sections are combined
			;; into output sections,
			
			;; define output sections in executable,
			
			;; control placement of sections relative to
			;; each other and in entire memory
			
			;; rename output sections
			(with-output-to-string (s)
			  (format s "SECTIONS~%")
			  (write-sequence (emit-cmd :code `(compound-statement ,@rest)) s))))
	    
	    (section-specification (destructuring-bind (name &rest properties) (cdr code)
				     ;; each section specification
				     ;; defines an output section in
				     ;; the output file
				     
				     ;; name .. can refer to section,
				     ;; subsection or archive library
				     ;; members
				     
				     ;; properties .. list of
				     ;; properties that define the
				     ;; sections contents and their
				     ;; allocation, separated by
				     ;; optional commas

				     ;; load = PROG, PAGE = 0 load = (0x0200)
				     ;; the = is optional, it can also be a >, value can optionally be enclosed in ()
				     ;; run = 0x010
				     ;; usually load and run location are the same, except with slow external memory (in our case flash)
				     ;; if load and run separate, than all parameters after load refer to load and all after run to run
				     ;; type = COPY
				     ;; type = DSECT
				     ;; type = NOLOAD
				     ;; fill = 0xffffffff
				     ;; { input_sections }
				     ;; align = 16
				     ;; .text : load = align(32)  .. allocate .text so that it falls on a 32-byte boundary
				     ;; palign .. pads the section to ensure size
				     ;; .text : palign(2) {} > PMEM  ..  equivalent to this:
				     ;; .text : palign = 2 {} > PMEM
				     ;;   section starts on 2-byte boundary and its size is guaranteed to be multiple of 2 bytes
				     ;; .mytext: palign(power2) {} > PMEM .. increases section size to the next power of two boundary
				     ;;   .mytext being 120 bytes and PMEM starting at 0x10020 will result in:
				     ;;   .mytext start=0x10080 size=0x80 align=128
				     ;; block(0x100)  .. section must fit between two adresses aligned to the blocking factor
				     ;; .. if too large it starts on an address boundary
				     ;; ebss : load = block(0x0080) .. entire section is contained in a single 128-byte page
				     ;;                                or begins on that boundary, block and align exclude each other
				     ;; order: aligned from largest to smallest, blocked from largest to smallest,
				     ;;        others from largest to smallest
				     ;; page = 1 .. if page is not specified then initialized sections go to 0 and uninitialized to 1
				     ;; .text: 0x0001000 .. binding 22-bit constant to location. sections must not overlap,
				     ;;                     incompatible with named memory
				     ;; .text: > (X)  .. bind to some executable memory,
				     ;;                  linker uses lower addresses first and avoids fragmentation when possible
				     ;; .stack : {} > RAM (HIGH)  .. location specifier, use this so that
				     ;;                              small changes in application don't lead to large changes in memory map
				     ;; .text: PAGE=0  .. anywhere in page0
				     ;; .text : { "f1-new.obj"(.text) "f3-a.obj"(.text,sec2) }
				     ;; .text : { *(.text) }   .. this is the default, if you dont write {..}
				     ;; *(.data) matches .dataspecial
				     ;; subsections are separated by colons: A:B:C or europe:north:norway
				     ;; .rts > BOOT2 { --library=rtsXX.lib(.text) }
				     ;; .text : {} > MEM1 | MEM2 | MEM3 .. list of ranges for the linker to choose from
				     ;; .text : {} >> MEM1 | MEM2 | MEM3 .. .text output section can be split
				     ;; .text : {} >> RAM .. split also works with a single target
				     ;; .text : {} >> (RW) .. finds sections with matching attributes
				     ;; some sections should not be split (.cinit autoinitialization C/C++, .pinit global constructors C++)
				     ;;    start end or size; run allocation of a union
				     
				     ;; to refer at run time to a load-time address the .label directive defines a special
				     ;; symbol with the sections load address. you don't need .label if you use the table operator
				     ;; group, union to organize and conserve memory
				     ;; group .. allocate contiguously in memory
				     ;; union .. allocate at same run address
				     ;; GROUP 0x1000 : { .data term_rec }  .. assume term_rec contains termination record for table in .data
				     ;;     you can't specify binding, alignment and allocation into named memory within the group
				     ;; if you have several routines that you want in fast memory at different stages of execution
				     ;; or share a block of memory for objects that are not active at the same time
				     ;; union occupies as much memory as its largest component
				     ;; UNION : run = FAST_MEM { .ebss:part1: { file1.obj(.ebss) }  .ebss:part2: { file2.obj(.ebss) } }
				     ;; UNION : run = FAST_MEM { .ebss:part1: load= SLOW_MEM { file1.obj(.ebss) }  .ebss:part2: load=SLOW_MEM { file2.obj(.ebss) } } 
				     
				     (with-output-to-string (s)
				       (format s "~a :" name)
				       (loop for (property-name value) in properties and i from 0 do
					    (if (= i 0)
						(format s " ~a = ~a" property-name value)
						(format s ", ~a = ~a" property-name value))))))
	    (section-blocks (destructuring-bind (&rest rest) (cdr code)
			      (with-output-to-string (s)
				(loop for (name target &key page load run type load-start load-end run-start) in rest do
				     (format s "~a~%" (emit-cmd :code (list 'section-block name target :page page :load load :run run :type type
									    :load-start load-start :load-end load-end :run-start run-start)))))))
	    (memory (destructuring-bind (&rest rest) (cdr code)
		      (with-output-to-string (s)
			(format s "MEMORY~%")
			(write-sequence (emit-cmd :code `(compound-statement ,@rest)) s))))
	    
	    (compound-statement (destructuring-bind (&rest lines) (cdr code)
				  (with-output-to-string (s)
				    (format s "{~%")
				    (loop for e in lines do
					 (format s "~a~%" (emit-cmd :code e)))
				    (format s "}~%"))))
	    (memory-range (destructuring-bind (name attr origin length &optional fill) (cdr code)
			    ;; page .. up to 32767 can be specified, overlap possible, defaults to 0
			    ;; name .. max 64 chars A-Za-z$._
			    ;; attr .. R W X I
			    ;; origin .. 32bit
			    ;; length .. 22bit
			    ;; fill .. fill character for range, increases output size!
			    (with-output-to-string (s)
			      (format s "~a" name)
			      (when attr (format s "( ~a )" attr))
			      (format s " : origin = ~a, length = ~a"
				      (emit-cmd :code origin)
				      (emit-cmd :code length))
			      (when fill
				(format s ", fill = ~a" fill)))))
	    (memory-ranges (destructuring-bind (&rest rest) (cdr code)
			     (with-output-to-string (s)
			       (loop for (name attr origin length &optional fill) in rest do
				    (format s "~a~%" (emit-cmd :code `(memory-range ,name ,attr ,origin ,length ,fill)))))))
	    (page-specifier (destructuring-bind (number) (cdr code)
			      (format nil "PAGE ~A:" number)))
	    (t (cond
		 ;; the following operations are supported: * / % + - , unary: - ~ !
		 ((and (= 2 (length code)) (member (car code)  '(- ~ !)))
		  ;; handle unary operators, i.e. - ~ !
		  (destructuring-bind (op operand) code
		    (format nil "(~a (~a))"
			    op
			    (emit-cmd :code operand))))
		 ((member (car code) '(* / % + -))
		  ;; handle binary operators
		  (with-output-to-string (s)
		    (format s "(")
		    (loop for e in (cdr code)
		       and i below (1- (length (cdr code))) do
			 (format s "~a ~a " (emit-cmd :code e) (car code)))
		    (format s "~a)" (emit-cmd :code (car (last (cdr code)))))))
		 ((member (car code)  '(<< >> == = < <= > >= & |\|| && ||||))
		  ;; handle logical operators, i.e. ==, &&, ...
		  (destructuring-bind (op left right) code
		    (format nil "(~a ~a ~a)"
			    (emit-cmd :code left)
			    op
			    (emit-cmd :code right))))
		     
		 (t (format nil "not processable: ~a" code)))))
	  (cond ((numberp code)
		 (format nil "0x~x" code))
		((or (symbolp code)
		     (stringp code)) ;; print variable
		 (substitute #\_ #\- (format nil "~a" code)))
		))))

#+nil
(emit-cmd :code '(sections
		  (section-blocks
		   (.cinit ">  FLASHC" :page 0)
		   (AppRamFuncs nil :load FLASHF :run RAML0 :load-start _RamfuncsLoadStart :load-end _RamfuncsLoadEnd :page 0))))

#+nil
(emit-cmd :code '(with-compilation-unit
		  (raw "#define BUFFER 0")
		  (memory
		   (page-specifier 0)
		   (memory-ranges
		    (ZONE0 RW #x4000 #x1000 #xffffffff)
		    (RAML0 () #x8000 (+ #x1000 (&& BUFFER (~ 1)))))
		   (page-specifier 1)
		   (memory-range BOOT_RSVD () (+ #x180 (funcall end RAML0 1)) #x50))))
