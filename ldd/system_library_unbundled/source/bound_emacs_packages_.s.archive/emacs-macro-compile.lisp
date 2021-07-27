;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;;
;;;	A hairy toy all for show.
;;;	The Emacs keyboard macro compiler.
;;;	BSG 2/18,24-25/78

(%include e-macros)
(declare (special comment-column))


;;;
;;;	Interim grinder.
;;;

(defun macomp-output-to-buffer (x)
       (macomp-bufout-r x nil)		;recurse
       (new-line))

(defun macomp-bufout-r (x indent)
       (if (null indent)(setq indent (cur-hpos))
	 else
	 (whitespace-to-hpos indent))
       (cond  ((fixp x)(insert-string (decimal-rep x))(insert-string "."))
	    ((atom x)(insert-string (maknam (explode x))))
	    ((memq (car x) '(if if-at if-back-at lambda cond let))
	     (insert-string "(")
	     (insert-string (car x))
	     (insert-string " ")
	     (macomp-bufout-finish-form (cdr x)(cur-hpos)))
	    ((eq (car x) 'defun)
	     (insert-string "(defun ")
	     (let ((hp (cur-hpos)))
		(macomp-bufout-r (cadr x) hp)
		(insert-string " ")
		(if (null (caddr x))	;null lambda list
		    (insert-string "()")
		    else
		    (macomp-bufout-r (caddr x) nil))
		(new-line)
		(macomp-bufout-finish-form (cdddr x) hp)))
	    ((and (eq (car x) 'quote)(null (cddr x)))
	     (insert-string "'")
	     (macomp-bufout-r (cadr x)(1+ indent)))
	    ((eq (car x) 'do-forever)
	     (insert-string "(do-forever ")
	     (new-line)
	     (macomp-bufout-finish-form (cdr x)(+ 2 indent)))
	    ((memq (car x) '(prog2 progn))
	     (insert-string "(")
	     (insert-string (car x))
	     (insert-string " ")
	     (macomp-bufout-finish-form (cdr x)(cur-hpos)))
	    ((eq (car x) 'prog)
	     (insert-string "(prog ")
	     (let ((hp (cur-hpos)))
		(macomp-bufout-r (cadr x) hp)
		(new-line)
		(do l (cddr x)(cdr l)(null l)
		    (if (atom (car l))
		        (macomp-bufout-r (car l) 0)
		        (if (> (cur-hpos)(1+ hp))(insert-string " "))
		        (setq l (cdr l)))
		    (macomp-bufout-r (car l) hp)
		    (if (not (null (cdr l)))(new-line)))
		(insert-string ")")))
	    (t (macomp-bufout-random-list (car x)(cdr x) indent))))

(defun macomp-bufout-random-list (the-car the-cdr indent)
       (insert-string "(")
       (macomp-bufout-r the-car (1+ indent))
       (if (> (+ (cur-hpos) 4) comment-column)
	 (setq indent (if (atom the-car)(+ 2 indent)
		        else (+ 1 indent)))
	 else
	 (if (atom the-car))(setq indent (1+ (cur-hpos)))
	 else (setq indent (1+ indent)))
       (do l the-cdr (cdr l) nil
	 (if (null l)(insert-string ")")(stop-doing))
	 (if (atom l)
	     (insert-string " . ")
	     (macomp-bufout-r l  nil)
	     (insert-string ")")
	     (stop-doing))
	 (if (and (> (+ (cur-hpos) 4) comment-column)
		(or (not (atom (cdr l)))
		    (not (atom (car l)))))
	     (new-line)
	     (whitespace-to-hpos indent)
	     else (if (not (and (back-at '/) )(not (atom (car l)))))
		    (insert-string " ")))
	 (macomp-bufout-r (car l) nil)))
		    
	         

(defun macomp-bufout-finish-form (x  hp)		
       (do l x (cdr l)(null l)
	 (macomp-bufout-r (car l) hp)
	 (if (not (null (cdr l)))(new-line)))
       (insert-string ")"))


;;;
;;;	The actual displaylist-keyboard-macro to Lisp compiler.
;;;

(declare (special macomp-last-cmd macomp-prog-needed-p macomp-default-search-string))

(defun macomp-compile-to-expr (name interp)
       (setq macomp-last-cmd 'noop macomp-prog-needed-p nil
	   macomp-default-search-string nil)
       (do ((outl nil)(inl (map 'macomp-preoptimize interp)(cdr inl))
		  (thisform)(thisfun)(lastfun '@)
		  (thisct)(lastct -1))
	 ((null inl)
	  (setq outl (nreverse outl))
	  (if macomp-prog-needed-p
	      (setq outl (list (cons 'prog (cons '() outl)))))
	  (append (list 'defun name '()) outl))
	 (setq thisform (macomp-term-compile inl))
	 (if (not (null thisform))
	     (setq thisfun (cond ((eq (car thisform) 'do-times)
			      (setq thisct (cadr thisform))
			      (caddr thisform))
			     (t (setq thisct 1) thisform)))
	      (if (equal thisfun lastfun)
 	         (setq outl
		     (cons (list 'do-times
			       (setq thisct (+ thisct lastct))
			        thisfun)
			 (cdr outl)))
	         else
	         (if (and (eq (car thisfun) 'insert-string)
		        (eq (car lastfun) 'insert-string))
		   (setq outl (cons (list 'insert-string
				      (catenate (cadr lastfun)
					      (cadr thisfun)))
				(cdr outl)))
		   else
		   (setq outl (cons thisform outl))))
	     (setq lastct thisct lastfun thisfun))))

(defun macomp-preoptimize (term)
       (let ((fun (cdar term)))
	  (cond ((eq fun 'quote-char)
	         (cond ((eq (cdadr term) 'String)
		      (rplacd (cadr term) 'Input/ Characters)))
	         (cond ((eq (cdadr term) 'Input/ Characters)
		      ;;cant happen from macro edit buffer
		      (cond ((samepnamep (caadr term) (ascii 15))
			   (rplaca (cadr term)(get_pname NL))))
		      (rplaca term (cons (get_pname
				       (maknam (explode (caadr term))))
				     'String))
		      (rplacd term (cddr term)))
		     (t (rplaca term
			      '("Quote-char saw no input" . %macomp-ierr)))))
	        ((eq fun 're-execute-command)
	         (rplacd (car term) macomp-last-cmd))
	        ((not (memq fun '(noop Numeric/ argument multiplier noop)))
	         (setq macomp-last-cmd fun)))))
	         
(defun macomp-term-compile (term)
       (let ((sym (caar term))(fun (cdar term)))
	  (cond ((eq fun 'noop) nil)
	        ((eq fun '%macomp-ierr)
	         (list 'error sym))
	        ((eq fun 'String)
	         (setq sym (read-from-string sym))
	         (do-forever		;Reduce strings.
		 (or (memq (cdadr term) '(rubout-char String))(stop-doing))
		 (if (eq (cdadr term) 'rubout-char)
		     (if (not (> (stringlength sym) 0))(stop-doing))
		     (rplacd term (cddr term))
		     (setq sym (substr sym 1 (1- (stringlength sym)))))
		 (if (eq (cdadr term) 'String)
		     (setq sym (catenate sym (read-from-string (caadr term))))
		     (rplacd term (cddr term))))
	         (if (> (stringlength sym) 0)
		   (list 'insert-string sym)))
	        ((eq (cdadr term) 'Numeric/ argument)
	          (macomp-comp-multipliers term))
	        ((eq fun 'multiplier)
	         (macomp-comp-multipliers term))
	        ((let ((prop (get fun 'search-command)))
		    (and prop (macomp-comp-searches prop term))))
	        ((memq fun '(next-line-command prev-line-command))
	         (let ((template
		       (cond ((eq fun 'prev-line-command)
			    '(if (firstlinep)(command-quit) else (prev-line)))
			   (t '(if (lastlinep)(command-quit) else (next-line))))))
		    (if (get (cdadr term) 'linepos-insensitive)
		        template
		        else
		        (list fun))))
	        ((eq fun 'macro-query)
	         (setq macomp-prog-needed-p t)
	         '(if (not (macro-query-get-answer))(return nil)))
	        (t (list fun)))))

(mapc '(lambda (x)(putprop x t 'linepos-insensitive))
      '(go-to-beginning-of-line go-to-end-of-line skip-over-indentation
			  indent-to-lisp indent-relative
			  prev-line-command next-line-command))

(defun macomp-comp-searches (prop term)
       (prog (string cmd strterm escterm)
	   (setq cmd (car prop) strterm (cdr term) escterm (cdr strterm))
	   (if (memq (cdar strterm) '(escape new-line))	;null string
	       (setq escterm strterm strterm '(("""""" . String))))
	   (if (and (eq (cdar strterm) 'String)
		  (memq (cdar escterm) '(new-line escape)))
	       (setq string (read-from-string (caar strterm)))
	       (or (stringp (setq string (macomp-search-defaultify string)))
		 (go sdf-err))
	       (setq cmd (list cmd string))
	       (if (eq (car cmd) 'regexp-search)
		 (setq cmd (list 'let (list (list 'm cmd))
			       '(and m (progn (release-mark m) t)))))
	       (return (prog2 0
			  (list 'if (list 'not cmd)
			        '(search-failure-annunciator))
			  (rplacd term (cdr escterm))))
	       else
	       (setq string (caar strterm))
	       (if (and (eq (cdar strterm) 'Input/ characters)
		      (= (getcharn string (stringlength string)) 33))
		 (setq string (substr string 1 (1- (stringlength string))))
		 (or (stringp (setq string (macomp-search-defaultify string)))
		     (progn (setq escterm strterm)
			  (go sdf-err)))
		 (return (prog2 0
			      (list 'if (list 'not (list cmd string))
				  '(search-failure-annunciator))
			      (rplacd term (cdr strterm))))))
	   (return '(error "Search string too complex. Edit the macro first."))
sdf-err
	   (rplacd term (cdr escterm))
	   (return '(error "Default search string may not be assumed in extension."))))


(defun macomp-search-defaultify (s)
       (cond ((nullstringp s) macomp-default-search-string)
	   (t (setq macomp-default-search-string s))))

(mapc '(lambda (x)(putprop (car x)(cdr x) 'search-command))
      '((string-search	 forward-search)
        (reverse-string-search reverse-search)
        (regexp-search-command regexp-search)
        (incremental-search	forward-search)
        (reverse-incremental-search reverse-search)
        (multi-word-search WORD-SEARCH-FRAMMIS)))

(defun macomp-stfix-to-fixnum (x)
       (let ((ibase 10.))(read-from-string x)))

(defun macomp-comp-multipliers (term)
       (let ((rest term)(num 1))
	  (do-forever
	    (cond ((eq (cdadr rest) 'Numeric/ argument)
		 (setq num (macomp-stfix-to-fixnum (caadr rest)))
		 (setq rest (cddr rest)))
		((eq (cdar rest) 'multiplier)
		 (setq num (* 4 num))
		 (setq rest (cdr rest)))
		(t (stop-doing))))
	  (prog2 0
	         (let ((fun (cdar rest))
		     (data (caar rest)))		;look at function
		    (cond ((eq fun 'String)
			 (setq data (read-from-string data))
			 (rplaca (car rest)
			         (maknam (explode
				         (catenate
					 (do ((l nil (cons c l))
					      (x 0 (1+ x))
					      (c (getchar data 1)))
					     ((= x num)
					      (get_pname (maknam l))))
					 (substr data 2)))))
			 nil)
			((get fun 'argwants)
			 (setq rest (cdr rest))
			   (list 'do-times num (list fun)))
			(t (setq rest (cdr rest))
			   (list 'let (list (list 'numarg num))(list fun)))))
	         (rplacd term rest))))
