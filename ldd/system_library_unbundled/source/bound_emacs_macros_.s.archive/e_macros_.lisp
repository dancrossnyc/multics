;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;	Macros for Programming in EMACS Environment

;;; HISTORY COMMENTS:
;;;  1) change(85-01-01,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Extracted from e-macros.incl.lisp, removed all the qwerty and macro-helper
;;;     stuff, and changed it to use defmacro; I actually ended up rewriting
;;;     many macros, either for efficiency or readability.
;;;     Previous journalization from e-macros.incl.lisp:
;;;               Written by BSG.
;;;               Added without-line-control, cleaned up a bit. 25 June 1981 RMSoley
;;;               Added protect, save-excursion-on-error 10 November 1981 RMSoley
;;;  2) change(85-01-27,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     removed (%include other_other),
;;;     added local push defmacro, and removed extraneous "macro" in
;;;     at-white defmacro.
;;;  3) change(86-02-24,Margolin), approve(86-02-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Fixed "protect" macro expansion to return the value of the body.
;;;                                                      END HISTORY COMMENTS

;;; Added by BSG 4/28/80: Include backquote in compilation.
(%include backquote)

(%include defmacro)

(declare (macros nil))
(defmacro push (val var)			;Can't %include other_other,
	`(setq ,var (cons ,val ,var)))	;as it defines the wrong IF.
(declare (macros t))

(defmacro bolp ()
	'(= curpointpos 0))

(defmacro eolp ()
	'(= curpointpos (1- curlinel)))

(defmacro at-white-char ()
	'(get (curchar) 'whiteness))

(defmacro with-mark (mark &body forms)
	`(let ((,mark nil))
	      (unwind-protect
	        (progn (setq ,mark (set-mark))
		     . ,forms)
	        (and ,mark (release-mark ,mark)))))

(defmacro save-excursion (&body forms)
       (let ((mark (gensym)))
	  `(with-mark ,mark
		    (unwind-protect
		      (progn .,forms)
		      (go-to-mark ,mark)))))

(defmacro save-excursion-on-error (&body forms)
	(let ((mark (gensym)))
	     `(with-mark ,mark
		       (protect (progn ,.forms)
			      &failure (go-to-mark   ,mark)))))

(defmacro save-excursion-buffer (&body forms)
	(let ((buffer (gensym)))
	     `(let ((,buffer current-buffer)
		  (previous-buffer previous-buffer))
		 (unwind-protect
		   (progn . ,forms)
		   (go-to-or-create-buffer ,buffer)))))

(defmacro protect-excursion (&body forms)
	(let ((buffer (gensym))
	      (prevbuf (gensym))
	      (mark (gensym)))
	     `(with-mark ,mark
		       (let ((,buffer current-buffer)
			   (,prevbuf previous-buffer)
			   (value))
			  (protect
			    (setq value (progn . ,forms))
			    &failure
			    (go-to-or-create-buffer ,buffer)
			    (setq previous-buffer ,prevbuf)
			    (go-to-mark ,mark))
			  value))))

(defmacro do-forever (&body forms)
	`(do nil (nil) . ,forms))

(defmacro with-the-mark-last (mark &body forms)
	`(and (or der-wahrer-mark
		(display-error "There is no true mark."))
	      (save-excursion
	        (with-mark ,mark
		         (cond ((point>markp der-wahrer-mark)
			      (go-to-mark der-wahrer-mark))
			     (t (move-mark ,mark der-wahrer-mark)))
		         (progn . ,forms)))))

(defmacro if-at (char &body forms)
	`(Multics-Emacs-if (at ,char) . ,forms)))

(defmacro at-white ()
	'(get (curchar) 'whiteness))

(defmacro stop-doing ()
	'(return nil))

(defmacro dispatch-on-current-char (&body forms)
	(do ((gs (gensym))
	     (clauses forms (cdr clauses))
	     (outs nil (cons s outs))
	     (s))
	    ((null clauses)
	     `(let ((,gs (curchar)))
		 (cond . ,(nreverse outs))))
	    (let ((thing (caar clauses))
		(result (cdar clauses))
		(condition))
	         (cond ((eq thing 'else)
		      (setq condition 't))
		     (t (cond ((stringp thing)
			     (setq thing `',(getchar thing 1))))
		        (setq condition `(eq ,gs ,thing))))
	         (setq s `(,condition .,result)))))

(defmacro Multics-Emacs-if (condition &rest forms)
	(do ((ifs)
	     (elses)
	     (l forms (cdr l)))
	    ((null l)
	     (cond (elses
		   `(cond (,condition .,(nreverse ifs))
			(t .,(cdr (nreverse elses)))))
		 (t `(cond (,condition .,(nreverse ifs))))))
	    (let ((form (car l)))
	         (cond ((eq form 'else)
		      (setq elses (list nil)))
		     (elses (push form elses))
		     (t (push form ifs))))))

;;; See Bawden "if" treaty of 5/9/80 -BSG
(or (getl 'if '(macro expr subr))
    (putprop 'if (get 'Multics-Emacs-if 'macro) 'macro))

(defmacro at-end-of-buffer ()
	'(and (eolp) (lastlinep)))

(defmacro at-beginning-of-buffer ()
       '(and (bolp) (firstlinep)))

(defmacro walk-through-region (&body forms)
	(let ((mark (gensym)))
	     `(with-the-mark-last
	        ,mark
	        (do ()
		  ((mark-reached ,mark))
		  . ,forms))))

(defmacro without-saving (&body forms)
	`(let ((dont-stash t))
	      dont-stash			;keep lcp from complaining
	      .,forms))

(defmacro do-times (howmany &body forms)
	(let ((dovar (gensym)))
	     `(do ,dovar ,howmany (1- ,dovar) (< ,dovar 1)
		. ,forms)))

(defmacro if-back-at (thing &body forms)
	`(Multics-Emacs-if (back-at ,thing) . ,forms))

(defmacro at (thing)
	(cond ((stringp thing)
	       (setq thing `',(getchar thing 1))))
	`(eq (curchar) ,thing))

(defmacro back-at (thing)
	(cond ((stringp thing)
	       (setq thing `',(getchar thing 1))))
	`(eq (lefthand-char) ,thing)))

(defmacro dispatch-on-lefthand-char (&body forms)
	(do ((gs (gensym))
	     (clauses forms (cdr clauses))
	     (outs nil (cons s outs))
	     (s))
	    ((null clauses)
	     `(let ((,gs (lefthand-char)))
		 (cond . ,(nreverse outs))))
	    (let ((thing (caar clauses))
		(result (cdar clauses))
		(condition))
	         (cond ((eq thing 'else)
		      (setq condition 't))
		     (t (cond ((stringp thing)
			     (setq thing `',(getchar thing 1))))
		        (setq condition `(eq ,gs ,thing))))
	         (setq s `(,condition .,result)))))

(defmacro without-modifying (&body forms)
	`(let ((read-only-flag nil)
	       (buffer-modified-flag t))
	      read-only-flag buffer-modified-flag    ;so lcp doesn't complain
	      .,forms))

(defmacro display-as-printout (&body forms)
	`(progn
	   (save-excursion-buffer
	     (go-to-or-create-buffer (gensym))
	     (putprop current-buffer t 'temporary-buffer)
	     (init-local-displays)
	     (progn . ,forms)
	     (display-buffer-as-printout))
	   (end-local-displays)))

(defmacro defvar (var-specs &optional (single-value nil value-given))
       (let ((specials nil)
	   (inits nil)
	   (nothing (ncons nil)))
	  (cond ((atom var-specs)		;(defvar <var> {<val>})
	         (setq var-specs
		     `((,var-specs ,(cond (value-given single-value)
				      (t nothing)))))))
	  (mapc '(lambda (spec)
		       (let ((v) (init))
			  (cond ((atom spec)
			         (setq v spec
				     init nothing))
			        ((null (cdr spec))
			         (setq v (car spec) init nil))
			        (t (setq v (car spec)
				       init (cadr spec))))
			  (push v specials)
			  (or (eq init nothing)
			      (push `(or (boundp ',v) (setq ,v ,init))
				  inits))))
	        var-specs)
	  (setq specials (nreverse specials))
	  (cond ((null inits) `(declare (special . ,specials)))
	        (t 
		`(progn 'compile
		        (declare (special .,specials))
		        . ,(reverse inits))))))

;;; Macro to invisibly (and temporarily) turn off line control
;;; 25 June 1981 Richard Mark Soley
(defmacro without-line-control (&body forms)
       `(let ((read-only-flag nil) (line-control:buffer 0))
	   read-only-flag line-control:buffer	;so lcp won't complain
	   . ,forms))


;;; Macro to be more useful than unwind-protect.
;;; (protect stuff to do &success stuff &failure stuff &always stuff)
;;; 10 November 1981 Richard Mark Soley
(defmacro protect (&body forms)
	(do ((form forms (cdr form))
	     (body ())
	     (success ())
	     (failure ())
	     (always ())
	     (current 'body))
	    ((null form)
	     (protect/ MACRO/ build
	       (nreverse body) (nreverse success) (nreverse failure)
	       (nreverse always)))
	    (let ((this (car form)))
	         (cond
		 ((eq this '&success) (setq current 'success))
		 ((eq this '&failure) (setq current 'failure))
		 ((eq this '&always ) (setq current 'always))
		 ((eq current 'body) (push this body))
		 ((eq current 'success) (push this success))
		 ((eq current 'failure) (push this failure))
		 ('else (push this always))))))

(defun protect/ MACRO/ build (body success failure always)
       (cond ((and (null success) (null failure))
	    `(unwind-protect
	       (progn ,.body)
	       ,.always))
	   (t (let ((protect-done-variable (gensym)))
		 `(let ((,protect-done-variable nil))
		       (unwind-protect
		         (prog1 (progn .,body)
			      (setq ,protect-done-variable t))
		         (cond (,protect-done-variable . ,success)
			     (t . ,failure))
		         . ,always))))))

(sstatus feature e-macros)
