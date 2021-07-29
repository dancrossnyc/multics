;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1973 *
;;; *                                                            *
;;; **************************************************************
;;; lisp_old_io_.lisp

;;; This module provides the lisp functions uread, uwrite, ufile, ukill, and crunit
;;; It is written in terms of the new I/O system.
;;; coded 27-MAR-73 by DAM
;;; fasload added 14 Nov 73 by DAM
;;; Modified 17 Sep 74 by DAM to not use fremob, add uprobe, uappend, uclose functions

(declare (system-file t))
(declare (special old-io-defaults uread uwrite infile outfiles))

(setq uread nil uwrite nil old-io-defaults nil)

(defun uread fexpr (x)
     (setq uread (openi (fetch-uread-names x)))
     (setq old-io-defaults (namelist uread))
     (eoffn uread (function
		(lambda (x y) x (setq uread nil) y)))	;provide for clearing of (status uread) on EOF
     (setq infile uread)		;make ^Q cause input from here
     (status crunit))


(defun fetch-uread-names (x)
     (mergef    (cond ((null x) '(*.*))
			((or (null (cdr x)) (null (cddr x))) (cons '* x))
			((list (cadddr x) (car x) (cadr x))) )
		(or old-io-defaults (namelist nil)) ))

(defun fasload fexpr (x)
    (setq x (fetch-uread-names x))
    (and (eq (car (last x)) 'fasl)	;drop fasl suffix
         (setq x (nreverse (cdr (reverse x)))) )
    (load x))



(defun uwrite fexpr (x)
     (and uwrite (setq outfiles (delq uwrite outfiles 1)))
     (or x (setq x (status crunit)))
     (setq uwrite (openo (mergef (list (cadr x) '!lisp 'output)		;temp file name !lisp.output until ufiled
			   (or old-io-defaults (namelist nil)) )))
     (apply 'crunit x)					;kludgey way to set the defaults
     (setq outfiles (cons uwrite outfiles))			;make this where output goes
     (status crunit))


(defun crunit fexpr (x)
	(or x (setq x (status crunit)))
	(setq old-io-defaults (mergef (cons (cadr x) '*) (or old-io-defaults (namelist nil))))
	x)


(defun ufile fexpr (x)
     (setq x (cond	(x (list (car x) (cadr x)))
		(old-io-defaults (cdr old-io-defaults)) ))
     (setq old-io-defaults (mergef (cons '* x) (or (namelist uwrite) old-io-defaults (namelist nil)) ))
     (errset (deletef old-io-defaults) nil)	;delete old copy if there
     (rename uwrite old-io-defaults)	;and rename to new copy
     (close uwrite)
     (setq uwrite nil)
     (status crunit))



(defun ukill fexpr (x)
     (setq x (cond ((null x) '(* . *))
	         ((null (cddr x)) (cons '* x))
	         ((list (cadddr x) (car x) (cadr x))) ))
     (setq old-io-defaults (setq x (mergef x (or old-io-defaults (namelist nil)) )))
     (setq x (deletef x))
     (list (cadr x) (caddr x)))

(defun uappend fexpr (x)
    (setq x (fetch-uread-names x))
    (and uwrite (setq outfiles (delq uwrite outfiles 1)))
    (setq old-io-defaults x)
    (setq x (rename x '(* !lisp !append)))
    (setq uwrite (opena x))
    (setq outfiles (cons uwrite outfiles))
    (status crunit))

(defun uprobe fexpr (x)
    (setq old-io-defaults (mergef (setq x (fetch-uread-names x)) (or old-io-defaults (namelist nil))))
    (not (not (allfiles x))))

(defun uclose nil
    (and uread (close uread))
    (setq uread nil))

;;;remob old-io_defaults because user must not play with it, and
;;;the compiler generated function !g1 that we happen to know is generated.

(remob 'old-io-defaults)(remob '!g1)(remob 'fetch-uread-names)

;;;end
