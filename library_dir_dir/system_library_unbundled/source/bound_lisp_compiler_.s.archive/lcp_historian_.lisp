;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;;;
;;; This is the easy end of a hack of prodigious hair.  BSG 1/20/80
;;;

(declare (special known-compilers known-segs compiling-compilers))
(declare (*fexpr use))
(setq known-compilers nil compiling-compilers nil known-segs nil)

(linel nil 200.)

(defun deradix (x)
       (list (// x (* 64. 64.))
	   (\ (// x 64.) 64.)
	   (\ x 64.)))

(defun dec (x)(let ((base 10.)(*nopoint t))(implode (explodec x))))
(defun cv-date (x)
       (let ((decdate (mapcar 'dec (deradix x))))
	  (catenate (cadr decdate) "/" (caddr decdate) "/" (car decdate))))

(defun cv-time (x)
       (let ((dectime (mapcar 'dec (deradix x))))
	  (or (= (stringlength (car dectime)) 2)
	      (setq dectime (cons (catenate "0" (car dectime)) (cdr dectime))))
	  (catenate (car dectime) ":" (cadr dectime) ":" (caddr dectime))))

(defun get-compiler-history-variable (x)
       (let ((obarray obarray))
	  (use c)
	  (let ((var (intern (copysymbol x nil))))
	       (cond ((boundp var)(symeval var))
		   (t nil)))))

(defun historian ()
       (let ((cversion (get-compiler-history-variable 'compiler-version))
	   (chistory (get-compiler-history-variable 'compiler-history)))
	  (cond ((null chistory)
	         (princ (catenate "The compiler you invoked is " cversion "."))
	         (terpri)
	         (princ "Its ancestry has not been recorded."))
	        (t (princ "The version of the compiler you invoked is")
		 (terpri)
		 (environment-historian "" chistory 0)))
	  (terpri)))

(defun environment-historian (label hist level)
       (indent-levels level)
       (princ (catenate label (cond ((atom hist) hist)
			      (t (car hist)))
		    ","))
       (terpri)
       (indent-levels level)
       (cond ((member hist compiling-compilers)
	    (princ "which impossibly, inconsistently, and fraudulently")
	    (terpri)(indent-levels level)
	    (print "claims to have participated in its own compilation.")
	    (terpri))
	   ((member hist known-compilers)
	    (princ "which is described above.")
	    (terpri))
	   ((atom (prog2 (setq known-compilers (cons hist known-compilers))
		       hist))
	    (princ "whose ancestry has not been recorded.")
	    (terpri))
	   (t (let ((compiling-compilers (cons hist compiling-compilers))
		  (words (cond ((= level 0) "which consists of ")
			     (t "which consisted of"))))
		 (princ words)
		 (terpri)
		 (do segs (cdr hist)(cdr segs)(null segs)
		     (seg-historian (car segs)
				(+ level 3)))))))

(defun seg-historian (seg level)
       (indent-levels level)
       (cond ((let ((dts (cadr seg)))
	         (or (null dts)
		   (and (not (atom dts))(null (cdr dts)))
		   (and (atom dts)(samepnamep dts '??))))
	    (princ (catenate "A version of " (car seg) " of unknown ancestry."))
	    (terpri))
	   (t 
	     (princ (catenate (car seg) ", compiled " (cv-date (car (cadr seg)))
			  " at " (cv-time (cdr (cadr seg))) ","))
	     (terpri)
	     (let ((new-level (+ level 2 (stringlength (car seg)))))
		(cond ((member seg known-segs)
		       (indent-levels new-level)
		       (princ "which was described above.")
		       (terpri))
		      (t (setq known-segs (cons seg known-segs))
		         (environment-historian "by " (caddr seg) new-level)))))))

(defun indent-levels (n)
       (do x n (1- x)(= x 0)(princ " ")))
