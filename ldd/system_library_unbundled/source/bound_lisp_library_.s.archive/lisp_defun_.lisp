;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

(%include defmacro)
(%include other_other)

(declare (*expr let-macro-flush-declares let-macro-cons-declares))
(declare (*expr grok-&keyword-list))
(declare (special *body* *normal* *optional* *rest*))

(defmacro defun& (name args . body)
  (let ((twoprop nil)
        (prop 'expr)
        tem)
       (cond ((symbolp name)
	    (cond ((symbolp args)
		 (cond ((memq args '(expr fexpr macro))
		        (setq prop args)
		        (setq args (car body))
		        (setq body (cdr body)))
		       ((memq name '(expr fexpr macro))
		        (setq prop name)
		        (setq name args)
		        (setq args (car body))
		        (setq body (cdr body)))))
		((atom args)
		 (error "Bad second argument to defun: " args 'fail-act))))
	   ((atom name)
	    (error "Bad first argument to defun: " name 'fail-act))
	   (t (setq tem (intern
		        (make_atom
			(catenate (car name) " " (cadr name)))))
	      (setq twoprop `(defprop ,(car name) ,tem ,(cadr name)))
	      (setq name tem)))
       (if (null twoprop)
	 (defun&-internal name args body prop)
	 `(progn 'compile
	         ,twoprop
	         ,(defun&-internal name args body prop)))))

(defun defun&-internal (name args body prop)
  (if (atom args)
      `(defprop ,name (lambda ,args . ,body) ,prop)
      (let (*body* *normal* *optional* *rest*)
        (grok-&keyword-list args (let-macro-flush-declares body))
        (cond ((and (null *rest*) (null *optional*))
	     (do ((l *normal* (cdr l))
		(newargs nil)
		(ignr nil)
		(ll nil)
		(gen))
	         ((null l)
		(or (null ll)
		    (setq *body* `((let ,ll . ,*body*))))
		`(defprop ,name
			(lambda ,newargs .
			        ,(let-macro-cons-declares
				 body
				 `((comment args = ,args)
				   ,.ignr
				   . ,*body*)))
			,prop))
	      (cond ((null (car l))
		   (setq gen (gensym))
		   (push gen newargs)
		   (push gen ignr))
		  ((symbolp (car l))
		   (push (car l) newargs))
		  ((atom (car l))
		   (error "Illegal argument (defun): " (car l) 'fail-act))
		  (t
		    (setq gen (gensym))
		    (push gen newargs)
		    (push `(,(car l) ,gen) ll)))))
	    (t
	      (let ((n+o (+ (length *normal*) (length *optional*)))
		  (n (length *normal*))
		  (nargs (gensym)))
	       (or (null *rest*)
		 (setq *body*
		       `((let ((,*rest*
                                    ,(if (zerop n+o)
				 `(listify ,nargs)
				 `(and (> ,nargs ,n+o)
				       (listify (- ,n+o ,nargs))))))
			    . ,*body*))))
	       (do ((l *optional* (cdr l))
		  (j n+o (1- j))
		  (ps nil)) ;; plural of p (as in lessp)
		 ((null l)
		  (or (null ps)
		      (setq *body* `((let ,ps .,*body*)))))
	        (setq *body*
		    `((let ((,(caar l)
			    (cond ((> ,nargs ,(1- j))
				 ,@(or (null (cddar l))
				       (progn (push (caddar l) ps)
					    `((setq ,(caddar l) t))))
				 (arg ,j))
				(t ,(cadar l)))))
			 . ,*body*))))
	       (do ((l *normal* (cdr l))
		  (j n (1- j))
		  (ll nil `((,(car l) (arg ,j)) . ,ll)))
		 ((null l)
		  (or (null ll)
		      (setq *body*
			  `((let ,ll . ,*body*))))))
	       (setq *body*
		   `(,@(if (null *rest*)
			 `((and ,(if (zerop n)
				   `(> ,nargs ,n+o)
				   `(or (< ,nargs ,n) (> ,nargs ,n+o)))
			        (error "Wrong number of arguments: " (list ',name ,nargs) 'fail-act)))
			 (or (zerop n)
			     `((and (< ,nargs ,n)
				  (error "Wrong number of arguments: " (list ',name ,nargs) 'fail-act)))))
		       . ,*body*))
	       (setq *body* (let-macro-cons-declares 
			  body `((comment args = ,args)
			         . ,*body*)))
	       `(defprop ,name (lambda ,nargs . ,*body*) ,prop)))))))

(putprop 'defun (get 'defun& 'macro) 'macro)

(sstatus feature defun)
