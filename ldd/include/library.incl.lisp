;; -*- Mode: Lisp; Lowercase: True -*-

;; library.incl.lisp -- Selectively loads modules from bound_lisp_library_ into
;; either the compiler or interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (mapc '(lambda (module)
	       (or (memq module (status features))
		 (load (catenate (car (namelist (truename infile)))
			       ">lisp_" module "_"))))
        '(backquote sharpsign destructuring_let defmacro setf
		macro_macros other_other defun defstruct loop)))

(eval-when (eval compile)
  (defun --load-myself-- macro (module)
    (setq module (cadr module))
    (list 'or
	(list 'status 'feature module)
	(list 'load (catenate (car (namelist (truename infile)))
			  ">lisp_" module "_")))))

(--load-myself-- runtime)
(--load-myself-- format)

;; This is necessary for (defprop a b macro) forms and defuns produced
;; by defmacro to appear in the object segment.
(declare (macros t))
