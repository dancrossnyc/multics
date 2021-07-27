;; -*- Mode: Lisp; Lowercase: True -*-

;; runtime.incl.lisp - Loads lisp_runtime_ into the interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (defun --load-myself-- macro (module)
    (setq module (cadr module))
    (list 'or
	(list 'status 'feature module)
	(list 'load (catenate (car (namelist (truename infile)))
			  ">lisp_" module "_")))))

(--load-myself-- runtime)

(declare (*expr fboundp fmakunbound fsymeval fset
	      ldb dpb
	      firstn butlast nbutlast
	      mem find-position-in-list ass rassq rassoc
	      circular-list-last)
         (*lexpr make-list rem remq remove symbolconc del))
