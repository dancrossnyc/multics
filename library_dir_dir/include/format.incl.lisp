;; -*- Mode: Lisp; Lowercase: True -*-

;; format.incl.lisp - Loads lisp_format_ into the interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (defun --load-myself-- macro (module)
    (setq module (cadr module))
    (list 'or
	(list 'status 'feature module)
	(list 'load (catenate (car (namelist (truename infile)))
			  ">lisp_" module "_")))))

(--load-myself-- format)

(declare (*lexpr format ferror))
