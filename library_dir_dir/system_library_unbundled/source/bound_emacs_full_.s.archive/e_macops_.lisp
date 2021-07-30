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
;;;	EMACS extended functions
;;;	 BSG, archy, GMP, RSL, RMSoley, etc
;;;
;;; How about some journalization:

;;; HISTORY COMMENTS:
;;;  1) change(1984-01-30,Margolin), approve(1986-08-20,MCRunknown),
;;;     audit(1986-08-20,UNKNOWN), install(1986-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;     Modified: June 1982 by Barmar - to install JSL's new string-search
;;;                   and global-print commands, using all his new hairy
;;;                   defcom features.
;;;     Modified: 31 August 1982 by Barmar - to move query-replace here from
;;;                   emacs-extended-searches.
;;;     Modified: 1 November 1983 by Barmar - to fix replace to not fill up the
;;;                   kill-ring with the old string, and fix replace and query-replace
;;;                   to use backward-char instead of reverse-search.
;;;     Modified: 25 November 1983 by Barmar - to fix the read-only bug in
;;;                   word-operator, also converting macros to defmacro.
;;;     Modified: ?? January 1984 by Barmar - to rewrite underlining code.
;;;  2) change(1984-12-25,Margolin), approve(1986-02-24,MCR7186),
;;;     audit(1986-08-12,Harvey), install(1986-08-20,MR12.0-1136):
;;;     to move iox_$control out to e_defpl1_.lisp, changed lambda into let.
;;;  3) change(1984-12-30,Margolin), approve(1986-02-24,MCR7186),
;;;     audit(1986-08-12,Harvey), install(1986-08-20,MR12.0-1136):
;;;     to move fillon, fill-mode-off, and
;;;     filloff to e_basic_, with fill-mode; changed speedtype to
;;;     interact with previous key bindings better.
;;;  4) change(1984-12-31,Margolin), approve(1986-02-24,MCR7186),
;;;     audit(1986-08-12,Harvey), install(1986-08-20,MR12.0-1136):
;;;     change comout-get-output to check
;;;     read-only-flag, add unwind-protect to comout, comout-get-output,
;;;     and eval-multics-command-line; move reset_more
;;;     from comout-command to eval-multics-command-line; changed
;;;     comout-get-output to use "file_output -truncate" rather than
;;;     calling hcs_$set_bc.
;;;  5) change(1985-01-27,Margolin), approve(1986-02-24,MCR7186),
;;;     audit(1986-08-12,Harvey), install(1986-08-20,MR12.0-1136):
;;;     add some *expr declarations.
;;;  6) change(1985-02-03,Margolin), approve(1986-02-24,MCR7186),
;;;     audit(1986-08-12,Harvey), install(1986-08-20,MR12.0-1136):
;;;     changed speedtype to special-case
;;;     CR, rather than expecting it to be in fill-mode-delimiters.
;;;  7) change(1985-02-24,Margolin), approve(1987-01-27,MCR7607),
;;;     audit(1987-02-13,RBarstad), install(1987-03-25,MR12.1-1014):
;;;     added comout-to-buffer, changed comout to use it.  Added
;;;     (%include defun), and changed comout to use &rest.  Changed
;;;     comout-command to turn off read-only-flag (with a warning in the
;;;     prompt), added comout-command-to-buffer.
;;;  8) change(1986-11-22,Margolin), approve(1987-01-27,MCR7607),
;;;     audit(1987-02-13,RBarstad), install(1987-03-25,MR12.1-1014):
;;;     Changed comout-command and comout-command-to-buffer to check whether
;;;     the output buffer contains an unwritten nonempty buffer, and to use
;;;     local displays for the warnings.  Changed comout-to-buffer to support
;;;     the comout-local-display option.
;;;  9) change(2016-12-27,Swenson), approve(2016-12-25,MCR10015),
;;;     audit(2016-12-27,GDixon), install(2016-12-27,MR12.6f-0003):
;;;     Updated date function, used in header of make-wall-chart, to handle
;;;     21st century years.
;;; 10) change(2018-07-30,Swenson), approve(2018-07-28,MCR10051),
;;;     audit(2018-08-01,GDixon), install(2018-08-17,MR12.6g-0014):
;;;     Updated (date) function to handle new 4-digit year return value
;;;     from (status date).
;;;                                                      END HISTORY COMMENTS

(%include e-macros)
(%include defmacro)
(declare (macros nil))
(%include defun)
(%include other_other)

(defmacro defkill (name type) `(defprop ,name ,type kills))

(declare (genprefix /!e_macops_))
(declare (*lexpr ncline comout minibuffer-remark gratuitous-mark-setter
	       report-error report-error-noabort))
(declare (*expr autofill-self-insert buffer-kill close-line iox_$control
	      e_pl1_$set_emacs_tty_modes e_pl1_$set_multics_tty_modes
	      ed-cv-fixnum-check extended-command forward-regexp-search
	      get-buffer-state
	      go-to-hpos hcs_$set_bc hcs_$truncate_file e_pl1_$get_iocb
	      intern-minibuf-response mark-at-current-point-p mark-tag-fun
	      local-display-buffer-info delete-word search-charset-forward
	      minibuffer-response push-mark-ring search-charset-backwards
	      self-insert skip-to-whitespace-in-line exists-buffer
	      search:numeric-prompt set-the-mark-here backward-n-chars
	      check-read-only get-key-binding parse-key-description))
(declare (special curpointpos good-word-charactertbl whitespace-charactertbl
	        iocb_ptr null-pointer non-speedtype-bindings
	        CRET last-input-char current-buffer-mode buffer-minor-modes
	        pdir-temp-ename suppress-redisplay-flag two-window-mode 
	        number-of-lines-in-buffer known-buflist fpathname 
	        buffer-modified-flag read-only-flag fill-prefix NLCHARSTRING
	        tab-equivalent comment-column comment-prefix firstline
	        lastline curline pdir-temp-pathname hard-enforce-fill-column
	        varlist fill-column fill-mode-delimiters minibufferp
	        OPEN-PAREN CLOSE-PAREN SEMI SINGLEQUOTE DOUBLEQUOTE SLASH
	        sexp-searcher-mark-list MCS-editing-characters mark-ring
	        MCS-escape-character process-dir named-mark-list
	        macro-execution-in-progress tty-no-upmotionp damaged-flag
	        last-minibuf-response previous-command e-lisp-error-mode
	        completion-list))

(defvar ((comout-local-display nil)
         (*comout-command-default-buffer* 'file_output)
         *comout-buffer-force*
         *comout-command-buffer*))

;;;
;;;	Named mark management
;;;


(defun get-named-mark (name)
       (let ((m (let ((tag (mark-tag-fun current-buffer)))
		 (get name tag))))
	  (and (null m)
	       (report-error 'no-named-mark " " name))
	  m))


(defun set-named-mark- (markname)
       (let ((tag (mark-tag-fun current-buffer)))
	  (let ((current (get markname tag)))
	       (cond (current (set-mark-here current))
		   (t (setq current (set-mark))
		      (putprop markname current tag)
		      (setq named-mark-list
			  (cons markname named-mark-list))))
	       current)))


(defcom delete-named-mark
        &arguments ((markname &symbol &prompt "Delete named mark: "
			&default &eval (report-error 'need-mark-name)))
        (let ((m (get-named-mark markname)))
	   (setq named-mark-list (delq markname named-mark-list))
	   (release-mark m)
	   (remprop markname (mark-tag-fun current-buffer))))

(defun produce-named-mark-list ()
       (let ((tag (mark-tag-fun current-buffer)))
	  (mapcar '(lambda (x)(list x (get x tag))) named-mark-list)))

;;; Character-oriented commands.

(defcom twiddle-chars
        &undo-function twiddle-chars
        (backward-char)
        (backward-char)
        (insert-char (prog2 0 (curchar) (delete-char) (forward-char))))

;;; Word-oriented commands.

(defcom replace
        &arguments ((original &default
			&eval (get-search-string "Replace old string"))
		(new &prompt "Replace new string: " NL))
        (if (not (forward-search original))
	  (search-failure-annunciator))
        (let ((old-length (stringlength original)))
	   (do-forever
	     (with-mark m			;have already searched, flush it
		      (backward-n-chars old-length)
		      (without-saving (wipe-point-mark m)))
	     (insert-string new)
	     (if (not (forward-search original))     ;look again
	         (stop-doing)))))		;not found, done

(defcom twiddle-words
        &undo-function twiddle-words
        (save-excursion
	(and (charset-member (curchar) good-word-charactertbl)
	     (forward-word))
	(search-charset-backwards good-word-charactertbl)
	(with-mark bow 
		 (backward-word)
		 (let ((second (point-mark-to-string bow)))
		      (without-saving (wipe-point-mark bow))
		      (search-charset-backwards good-word-charactertbl)
		      (cond ((at-beginning-of-buffer)
			   (insert-string second)
			   (command-quit)))
		      (let ((in-between (point-mark-to-string bow)))
			 (without-saving (wipe-point-mark bow))
			 (backward-word)
			 (insert-string second)
			 (insert-string in-between)
			 (forward-word))))))

(defcom underline-word
        &undo-function remove-underlining-from-word
        (or (bolp)(backward-char))
        (forward-word)
        (with-mark m
	         (backward-word)
	         (underline-point-mark m)))

(declare (special underline-whitespace))
(defcom underline-region
        &numeric-function de-underline-region
        &undo-function de-underline-region
        (let ((point-at-end))
	   (with-the-mark-last done
	     (setq point-at-end
		 (mark-at-current-point-p der-wahrer-mark))
	     (underline-point-mark done))
	   (or point-at-end (exchange-point-and-mark))
	   (cond ((and (looking-at BACKSPACE)
		     (alphalessp (lefthand-char) "_"))
		(forward-char) (forward-char)))
	   (or point-at-end (exchange-point-and-mark))))

(defun underline-point-mark (mark)
       (do ((this (curchar) (curchar)))
	 ((mark-reached mark))
	 (cond ((and (samepnamep this TAB) underline-whitespace)
	        (insert-string
		(gen-repetitive
		  (- tab-equivalent (\ (cur-hpos) tab-equivalent))
		  "_"))
	        (delete-char))
	       ((and (samepnamep this SPACE) underline-whitespace)
	        (delete-char) (insert-string "_"))
	       ((or (samepnamep this "_")	;don't underline underscore
		  (samepnamep this "�")	;skip control chars
		  (alphalessp this SPACE))	; ""    ""      ""
	        (forward-char))
	       ((and (charset-member this whitespace-charactertbl)
		   (not underline-whitespace))
	        (forward-char))
	       ((alphalessp this "_")
	        (forward-char)
	        (cond ((samepnamep (curchar) BACKSPACE)	;already overstruck
		     (forward-char) (forward-char))
		    (t (insert-string BACKSPACE)
		       (insert-string "_"))))
	       ((samepnamep (lefthand-char) BACKSPACE)	;already overstruck
	        (forward-char))
	       (t (insert-string "_")
		(insert-string BACKSPACE)
		(forward-char)))))
	  
(defcom de-underline-region
        &undo-function underline-region
        (with-the-mark-last done
	 (de-underline-point-mark done)))

;;; Removes underlining from the point to a given mark.
;;; Caller must ensure that mark is beyond point.
(defun de-underline-point-mark (mark)
       (do-forever
         (if (mark-reached mark) (stop-doing))
         (cond ((or (looking-at "_")
		(looking-at "_"))
	      (delete-char) (delete-char))
	     ((and underline-whitespace
		 (looking-at "_"))
	      (delete-char) (insert-string SPACE))
	     ('else (forward-char)))))

(defcom remove-underlining-from-word
        (or (bolp)
	  (backward-char))
        (forward-word)
        (with-mark m
	         (backward-word)
	         (de-underline-point-mark m)))

;;; Line-oriented commands.

(defcom open-space
        &numeric-argument (&repeat &lower-bound 0)
        (insert-char NL)
        (backward-char))


(defcom set-fill-prefix
        &arguments ((new-prefix &default
			  &eval
			  (with-mark m
				   (go-to-beginning-of-line)
				   (prog2 nil
					(point-mark-to-string m)
					(go-to-mark m)))))
        (setq fill-prefix new-prefix))


(defcom center-line
        (save-excursion
	(go-to-beginning-of-line)
	(delete-white-sides)
	(go-to-end-of-line)
	(delete-white-sides)
	(let ((hp (cur-hpos)))
	     (go-to-beginning-of-line)
	     (and (< hp fill-column)
		(whitespace-to-hpos (// (- fill-column hp) 2))))))

(defcom split-line
        (let ((hpos (cur-hpos)))
	   (save-excursion
	     (insert-char NL)
	     (whitespace-to-hpos hpos))))

(defcom delete-line-indentation
        &numeric-argument (&pass)
        (if numarg (next-line))
        (go-to-beginning-of-line)
        (delete-white-sides)
        (rubout-char))

(defcom mark-whole-buffer
        (go-to-end-of-buffer)
        (set-the-mark)
        (go-to-beginning-of-buffer))

;;; Speedtype

(defcom speedtype-expander
        &numeric-argument (&repeat &lower-bound 0)
        (prog (the-word)
	    (backward-char)
	    (cond ((not (charset-member (curchar) good-word-charactertbl))
		 (forward-char))
		(t (forward-char)
		   (with-mark m
			    (backward-word)
			    (setq the-word
				(internedp (point-mark-to-string m)))
			    (if (symbolp the-word)
			        (setq the-word (get the-word 'speedtype))
			        else (setq the-word nil))
			    (if the-word (wipe-point-mark m)
			        (insert-string the-word)
			        else (go-to-mark m)))))
	    (let ((old-binding
		  (or (cdr (assq last-input-char non-speedtype-bindings))
		      'self-insert)))
	         (funcall old-binding))))


;;; Hack to check obarray for a "word" without interning it
(defun internedp (string)
       (cond ((= (stringlength string) 1)
	    (ascii (getcharn string 1)))
	   (t (do ((l (obarray (\ (sxhash string) 509.)) (cdr l)))
		((null l) string)
		(and (samepnamep (car l) string)
		     (return (car l)))))))

(defun setab n				;reverted to old-command
        (do ((arg-list (listify n)(cddr arg-list)))
	  ((null arg-list))
	  (let ((name (intern (make_atom (e_lap_$trim (car arg-list)))))
	        (value (cadr arg-list)))
	       (if (nullstringp value)
		 (display-error "No expansion supplied for " name "."))
	       (putprop name value 'speedtype))))

(defcom speedtype
        (assert-minor-mode 'speedtype)
        (register-local-var 'non-speedtype-bindings)
        (setq non-speedtype-bindings nil)
        (mapc '(lambda (x)
		   (push (cons x
			     (get-key-binding (parse-key-description x)))
		         non-speedtype-bindings)
		   (set-key x 'speedtype-expander))
	    (cons CRET fill-mode-delimiters)))

(defcom speedtypeoff
        (negate-minor-mode 'speedtype)
        (mapc '(lambda (x)
		   (set-key (car x) (cdr x)))
	    non-speedtype-bindings))

;;; Uncle-and-aunt indenter, BSG 12/10/78

(defcom indent-relative
        &numeric-argument (&pass)
        (go-to-beginning-of-line)
        (tab-to-previous-columns))

(defcom tab-to-previous-columns
        &numeric-argument (&pass)
        (skip-over-whitespace-in-line)
        (and (bolp) (eolp) (setq numarg nil))
        (let ((orighpos (cur-hpos)))
	   (with-mark m
		    (delete-white-sides)
		    (do-forever
		      (if (firstlinep) (stop-doing)
			else (prev-line))
		      (if (line-is-blank)
			else (if numarg
			         (skip-over-indentation)
			         (if (< (cur-hpos) orighpos)
				   (stop-doing))
			         else
			         (stop-doing))))
		    (if (not numarg)
		        (go-to-hpos orighpos)
		        (if (at-white-char)
			  (skip-over-whitespace-in-line)
			  (if (eolp) (go-to-beginning-of-line))
			  else
			  (if (not (> (cur-hpos) orighpos))
			      (skip-to-whitespace-in-line)
			      (skip-over-whitespace-in-line))))
		    (setq orighpos (cur-hpos))
		    (go-to-mark m)
		    (whitespace-to-hpos orighpos))))


(defcom cret-and-indent-relative
        &numeric-argument (&pass)
        (cond (minibufferp (insert-string NLCHARSTRING))
	    (t (new-line) (indent-relative))))

;;;
;;;  Adapted from Killian, BSG 10/15/80
;;;

(defcom indent-rigidly
     &numeric-argument (&pass)
     (setq numarg (or numarg 0))
    (save-excursion
      (with-the-mark-last m
		      (do-forever
		        (if (mark-at-current-point-p m)(stop-doing))
		        (if (not (line-is-blank))
			  (skip-over-indentation)
			  (let ((hpos (cur-hpos)))
			       (delete-white-sides)
			       (whitespace-to-hpos
			         (max 0 (+ hpos numarg)))))
		        (if (mark-on-current-line-p m)(stop-doing))
		        (next-line)))))

;;;
;;;	Variable managers
;;;

(setq varlist nil)

(defcom put-variable
        &prologue &eval (if der-wahrer-mark
		        else (report-error 'mark-not-set))
        &arguments ((x &symbol &prompt "Set variable: "
		   &default &eval (display-error-noabort
				"You must supply a variable name.")))
        (or (memq x varlist) (setq varlist (cons x varlist)))
        (wipe-region)
        (putprop x (kill-pop) 'editvalue))

(defcom get-variable
        &arguments ((x &symbol &prompt "Get variable: "
		   &default &eval (display-error-noabort
				"You must supply a variable name.")))
        (let ((stuff (get x 'editvalue)))
	   (if stuff (set-the-mark) (insert-string stuff)
	       else (display-error x " has no value."))))

(defcom list-variables
        (prog ()
	    (if (not varlist) (display-error "No variables to list."))
	    (save-excursion-buffer
	      (go-to-or-create-buffer (maknam (explodec "Variable list")))
	      (putprop current-buffer t 'temporary-buffer)
	      (insert-string "Current string variables")
	      (new-line)(new-line)
	      (insert-string "Name")
	      (format-to-col 10.)
	      (insert-string "#Chars")
	      (new-line)(new-line)
	      (do l varlist (cdr l)(null l)
		(let ((v (car l))
		      (s (get (car l) 'editvalue)))
		     (insert-string v)
		     (format-to-col 10.)
		     (insert-string (decimal-rep (stringlength s))))
		(new-line))
	      (display-buffer-as-printout))
	    (end-local-displays)))

(defcom-synonym lvars list-variables)

;;;
;;;      Buffer lister
;;;

(declare (special previous-buffer))

;;; Got rid of old horrible crock, replaced using local-display-buffer-info
;;; Richard Mark Soley, 5 August 1981
(defcom list-buffers
        (init-local-displays)
        (local-display-generator-nnl "Listing of Current Buffers")
        (local-display-generator-nnl "")
        (do ((buffer known-buflist (cdr buffer)))
	  ((null buffer))
	  (local-display-buffer-info (car buffer)))
        (end-local-displays))

(defun format-to-col (x)
       (cond ((not (< curpointpos x))(insert-char SPACE))
	   (t (whitespace-to-hpos x))))

;;; Case changing commands and primitives.

(defmacro word-operator (count &body forms)
	`(progn
	   (cond ((or (null ,count)
		    (and (charset-member (curchar) good-word-charactertbl)
		         (plusp ,count)))
		(or (and (not (charset-member (lefthand-char)
					good-word-charactertbl))
		         (charset-member (curchar) good-word-charactertbl))
		    (backward-word)))
	         ((minusp ,count)
		(do-times (abs ,count) (backward-word)))
	         (t (search-charset-forward good-word-charactertbl)))
	   (do ((n (abs (or ,count 1)) (1- n)) (word) (quit))
	       ((or quit (zerop n)) (and quit (ring-tty-bell)))
	       (setq word
		   (with-mark here
			    (forward-word)
			    (prog1 (point-mark-to-string here)
				 (go-to-mark here))))
	       (protect ,@forms
		      &success (without-saving (delete-word)))
	       (or (= n 1)
		 (search-charset-forward good-word-charactertbl)
		 (setq quit t)))
	   (cond ((and (fixp ,count) (minusp ,count))
		(do-times (abs ,count) (backward-word))
		(or (at-beginning-of-buffer) (backward-char))))))

(defcom capitalize-initial-word
        &numeric-argument (&pass)
        (word-operator numarg
		   (insert-char (uppercase (substr word 1 1)))
		   (insert-string (lowercase (substr word 2)))))

(defcom upper-case-word
        &numeric-argument (&pass)
        (word-operator numarg
		   (insert-string (uppercase word))))

(defcom lower-case-word
        &numeric-argument (&pass)
        (word-operator numarg
		   (insert-string (lowercase word))))

(defun uppercase (string)
       (maknam
         (mapcar '(lambda (x) (cond ((and (> x (1- (CtoI "a")))
				  (< x (1+ (CtoI "z"))))
			       (- x 40))
			      (t x)))
	       (exploden string))))

(defun lowercase (string)
       (maknam
         (mapcar '(lambda (x) (cond ((and (> x (1- (CtoI "A")))
				  (< x (1+ (CtoI "Z"))))
			       (+ x 40))
			      (t x)))
	       (exploden string))))

(defun upper-case-char (c)
       (let ((cn (getcharn c 1)))
	  (cond ((and (< cn (1+ (CtoI "z")))
		    (> cn (1- (CtoI "a"))))
	         (ascii (- cn 40)))
	        (t c))))

(defun lower-case-char (c)
       (let ((cn (getcharn c 1)))
	  (cond ((and (< cn (1+ (CtoI "Z")))
		    (> cn (1- (CtoI "A"))))
	         (ascii (+ cn 40)))
	        (t c))))

(defcom lower-case-region
        (with-the-mark-last m
		        (do-forever
			(if (mark-reached m)(stop-doing))
			(insert-char (lower-case-char (curchar)))
			(delete-char))))


(defcom upper-case-region
        (with-the-mark-last m
		        (do-forever
			(if (mark-reached m)(stop-doing))
			(insert-char (upper-case-char (curchar)))
			(delete-char))))

;;;
;;;	Auto-documenting features
;;;	BSG and archy 5/24/78
;;;	BSG moved 'em all to e_self_documentor_ 10/7/78, leaving
;;;	only the following pieces that sort of gotta stay around.
;;;

(defcom find-key
        &arguments ((fun &symbol &prompt "Function name: "
		     &default &eval (display-error-noabort "You must supply a function name.")))
        (prog (env)
	    (setq env (ncons fun))
	    (map-over-emacs-commands
	      '(lambda (symbol suspect arg)
		     (cond ((eq suspect (car arg))
			  (rplacd arg symbol))))
	      env)
	    (cond ((cdr env)
		 (minibuffer-print fun " is on " (printable (cdr env))))
		(t (minibuffer-print fun " is not on any key.")))))


(defun display-buffer-as-printout ()
       (save-excursion
         (init-local-displays)
         (go-to-beginning-of-buffer)
         (do-forever
	 (local-display-generator (curline-as-string))
	 (if (lastlinep) (stop-doing))
	 (next-line))))


(defcom skip-over-indentation
        (go-to-beginning-of-line)
        (do-forever
	(if (eolp)(stop-doing))
	(if (not (at-white-char))(stop-doing))
	(forward-char)))

;;;
;;;	Hirsute blank line deleter, 6/12/78
;;;

(defcom delete-blank-lines
        (prog (orig-was-blank eobp)
	    (setq orig-was-blank (line-is-blank))
	    (if (not orig-was-blank)(go-to-end-of-line)
	        (delete-white-sides)
	        (insert-char NL)
	        else (skip-back-whitespace)
	        (go-to-end-of-line)
	        (if (not (bolp))(forward-char)))     ;bolp => bobp
	    (with-mark m
		     (skip-over-whitespace)
		     (setq eobp (at-end-of-buffer))
		     (or eobp (go-to-beginning-of-line))
		     (without-saving (wipe-point-mark m)))
	    (if (and eobp (not orig-was-blank))(rubout-char))
	    (if (and (not eobp) orig-was-blank)(insert-char NL))))

;;;
;;;	Sentence hackery, 5/78, converted by bsg & archy
;;;	from a noble attempt by Margulies.
;;;

(defun at-end-of-sentence ()
       (prog ()
	   (if (at-white-char)
	       (dispatch-on-lefthand-char
	         ("." (return t))
	         ("?" (return t))
	         ("!" (return t))))
	   (if (or (at-end-of-buffer)(at-beginning-of-buffer))
	       (return t))
	   (if (and (eolp)(save-excursion (next-line)(line-is-blank)))
	       (return t))))

(defcom forward-sentence
        &numeric-argument (&repeat)
        &negative-function backward-sentence
        (skip-over-whitespace)
        (if (at-end-of-buffer)(command-quit))
        (do-forever
	(skip-over-whitespace)
	(skip-to-whitespace)
	(if (at-end-of-sentence) (stop-doing))))


(defcom backward-sentence
        &numeric-argument (&repeat)
        &negative-function forward-sentence
        (skip-back-whitespace)
        (if (at-beginning-of-buffer)(command-quit))
        (skip-back-to-whitespace)		;in case we ARE are  end-of-sentence
        (do-forever
	(if (at-end-of-sentence)
	    (skip-over-whitespace)
	    (stop-doing))
	(skip-back-to-whitespace)
	(skip-back-whitespace)))


(defprop kill-to-end-of-sentence forward kills)
(defcom kill-to-end-of-sentence
        &numeric-argument (&repeat)
        &negative-function kill-backward-sentence
        (with-mark m
	         (forward-sentence)
	         (kill-backwards-to-mark m)
	         (merge-kills-forward)))


(defprop kill-backward-sentence reverse kills)
(defcom kill-backward-sentence
        &numeric-argument (&repeat)
        &negative-function kill-to-end-of-sentence
        (with-mark m
	         (backward-sentence)
	         (kill-forward-to-mark m)
	         (merge-kills-reverse)))

;;;
;;;	Comments, anyone?
;;;	BSG 6/25/78
;;;

(defcom set-comment-column
        &arguments ((column &integer
		        &default &eval (if numarg numarg
				       else (1+ (cur-hpos)))))
        &numeric-argument (&pass)
        (setq comment-column (1- column))
        (minibuffer-print "Comment column = " (decimal-rep (1+ comment-column))))


(defcom set-comment-prefix
        &arguments ((prefix &prompt "Comment prefix: "))
        (setq comment-prefix prefix))


(defcom prev-comment-line
        (cond ((firstlinep))
	    (t (prev-line)
	       (indent-for-comment))))


(defcom down-comment-line
        (cond ((lastlinep))
	    (t (next-line)
	       (indent-for-comment))))


(defun indent-to-fill-prefix ()
       (go-to-beginning-of-line)
       (delete-white-sides)
       (insert-string fill-prefix))


(defcom indent-for-comment
        (go-to-beginning-of-line)
        (if (nullstringp comment-prefix)(indent-for-nondelimited-comment)
	  else (if (forward-search-in-line comment-prefix)     ;already got one)
		 (do-times (stringlength comment-prefix)(backward-char))
		 (if (= (cur-hpos) comment-column) nil
		     else (delete-white-sides)
		     (do ((column comment-column (+ column 5)))
		         ((> column (cur-hpos))
			(whitespace-to-hpos column))))
		 (do-times (stringlength comment-prefix)(forward-char))
		 else (go-to-end-of-line)
		 (delete-white-sides)
		 (insert-string comment-prefix)
		 (indent-for-comment))))


(defun indent-for-nondelimited-comment ()
       ;;at bol from above.
       (if (go-to-hpos comment-column)		;line at least that long.
	 (if (get (lefthand-char) 'whiteness)	;last was whitespace
	     (if (> (cur-hpos) comment-column)	;tab or such
	         (rubout-char)
	         (insert-string " ")
	         (indent-for-nondelimited-comment)
	         else			;we are right at it.
	         (do-forever
		 (if (eolp)(stop-doing))
		 (if (at-white-char)(delete-char)
		     else (stop-doing))))
	     else				; foo|__com
	     (skip-to-whitespace-in-line)
	     (if (not (eolp))(delete-white-sides))
	     (insert-char " "))
	 else				;line not that long
	 (whitespace-to-hpos comment-column)))


(defprop kill-comment forward kills)
(defcom kill-comment
        (go-to-beginning-of-line)
        (if (nullstringp comment-prefix)
	  (if (go-to-hpos comment-column)
	      (if (not (get (lefthand-char) 'whiteness))
		(skip-to-whitespace)))
	  (skip-back-whitespace-in-line)
	  (with-mark m (go-to-end-of-line)(wipe-point-mark m))
	  else
	  (if (forward-search-in-line comment-prefix)
	      (reverse-search-in-line comment-prefix)
	      (skip-back-whitespace-in-line)
	      (with-mark m (go-to-end-of-line)(wipe-point-mark m))
	      else
	      (killsave-string ""))))

;;;
;;;	Stuff grabbed from old start_up
;;;

(defun ncline n			;lexprish cline
       (e_cline_ (apply 'catenate
		  (mapcan '(lambda (x)(append '(" ") (explodec x)))
			 (listify n)))))


(defcom dp
        (ncline 'dp fpathname))


(defcom loadit
        (let ((fpathname nil))
	   (write-out-file pdir-temp-pathname)
	   (loadfile pdir-temp-pathname)
	   (hcs_$truncate_file process-dir pdir-temp-ename 0)))

(defun comout-get-output n
       (check-read-only)
       (unwind-protect
         (progn (ncline "file_output" pdir-temp-pathname
		    "-ssw user_output -ssw error_output -truncate")
	      (apply 'ncline (listify n)))
         (e_cline_ "revert_output -ssw user_output -ssw error_output"))
       (let ((fpathname nil))
	  (read-in-file pdir-temp-pathname))
       (hcs_$truncate_file process-dir pdir-temp-ename 0))

(defun comout (&rest command)
       (apply 'comout-to-buffer (cons 'file_output command)))

(defun comout-to-buffer (buffer &rest command)
       (unwind-protect
         (progn (go-to-or-create-buffer buffer)
	      (apply 'comout-get-output command))
         (cond (comout-local-display
	       (display-buffer-as-printout)
	       (go-to-buffer previous-buffer))
	     (t (select-buffer-find-window current-buffer 'cursize)))))

(defcom comout-command
        &numeric-function comout-command-to-buffer
        &arguments ((command-line
		  &prompt &eval
			(progn (setq *comout-buffer-force*
				   (comout-buffer-warning
				     *comout-command-default-buffer*))
			       "Multics command: ")))
        (comout-command-internal *comout-command-default-buffer* *comout-buffer-force*
			   command-line))

(defun comout-command-internal (buffer force-flag command)
       (setq command (e_lap_$trim command))
       (unless (nullstringp command)
	     (when force-flag
		 (save-excursion-buffer
		   (go-to-buffer buffer)
		   (setq fpathname nil)
		   (setq read-only-flag nil)))
	     (comout-to-buffer buffer command)))

(defcom comout-command-to-buffer
        &numarg &ignore
        &arguments ((buffer &symbol
		        &default &eval
		        (let ((completion-list known-buflist))
			   (setq *comout-command-buffer*
			         (make_atom (minibuffer-response "Buffer: ")))))
		(command-line
		  &prompt &eval
			(progn (setq *comout-buffer-force*
				   (comout-buffer-warning
				     *comout-command-buffer*))
			       "Multics command: ")))
        (comout-command-internal buffer *comout-buffer-force*
			   command-line))

(defun comout-buffer-warning (buffer)
       (and (exists-buffer buffer)
	  (let ((ro (get-buffer-state buffer 'read-only-flag))
	        (not-empty (not (empty-buffer-p buffer)))
	        (path (get-buffer-state buffer 'fpathname))
	        (force))
	       (setq force (or ro path))
	       (setq path (and not-empty path))
	       (when (or ro path)
		   (ring-tty-bell)
		   (init-local-displays)
		   (when ro
		         (local-display-generator-nnl
			 (catenate "Output buffer '" buffer "' is READ ONLY.")))
		   (when path
		         (local-display-generator-nnl
			 (catenate "Output buffer '" buffer
				 "' contains unwritten changes to "))
		         (local-display-generator-nnl
			 (catenate "     " path ".")))
		   (local-display-generator-nnl "It will be overwritten if you continue (type ^G to abort).")
		   (end-local-displays))
	       force)))

(defcom linecounter
        (let ((lineno 1)
	    (curlineno 0)
	    (nnlp (save-excursion
		  (go-to-end-of-buffer)
		  (not (and (eolp)(bolp))))))
	   (save-excursion
	     (with-mark m
		      (go-to-beginning-of-buffer)
		      (do-forever
		        (if (mark-on-current-line-p m)
			  (setq curlineno lineno)
			  (stop-doing))
		        (if (lastlinep) (stop-doing))	;just in case
		        (setq lineno (1+ lineno))
		        (next-line))))
	   (setq lineno (if nnlp number-of-lines-in-buffer
			else (1- number-of-lines-in-buffer)))
	   (minibuffer-remark
	     (decimal-rep lineno)
	     (if (= lineno 1) " line" else " lines")
	     (if nnlp " (NO NEWLINE)" else "")
	     ", current = "
	     (decimal-rep curlineno) ", column = "
	     (decimal-rep (1+ (cur-hpos))))))

;;;
;;; Global Print command.
;;; New version by JSL - June 1982

(defcom global-print
        &numeric-function global-regexp-print
        &arguments ((string &string &default
		        &eval (get-search-string "Global print")))
        (let ((foundflag)
	    (line-count 0))
	   (save-excursion
	     (go-to-beginning-of-buffer)
	     (do-forever
	       (if (not (forward-search string)) (stop-doing))
	       (if (not foundflag)
		 (setq foundflag t)
		 (with-mark m
			  (reverse-search string)
			  (do-forever
			    (if (mark-on-current-line-p m)
			        (stop-doing))
			    (setq line-count (1+ line-count))
			    (next-line)))
		 (init-local-displays))
	       (do-times line-count (prev-line))
	       (do-times line-count
		       (local-display-current-line)
		       (next-line))
	       (local-display-current-line)
	       (if (lastlinep) (stop-doing))
	       (next-line)))
	   (if foundflag (end-local-displays)
	       else (search-failure-annunciator))))

;;;
;;;	Option manager
;;;	BSG 7/29/78
;;;

(defprop option opt expr)

(declare (special list-of-known-options))

(defun opt n				;lexpr
       (cond ((= n 0)			;noargs
	    (display-error "Usage: opt list/opt status XXX/opt XXX <VALUE>"))
	   ((= n 1)
	    (if (eq (arg 1) 'list)
	        (save-excursion-buffer
		(go-to-or-create-buffer (intern (make_atom "Opt List")))
		(putprop current-buffer t 'temporary-buffer)
		(insert-string "Options in effect")(new-line)
		(new-line)
		(mapc '(lambda (x)
			     (insert-string x)(format-to-col 33.)
			     (insert-string (option-value-string x))(new-line))
		      list-of-known-options)
		(display-buffer-as-printout))
	        (end-local-displays)
	        else (opt)))		;barfo
	   ((= n 2)
	    (if (eq (arg 1) 'status)
	        (if (memq (arg 2) list-of-known-options)
		  (minibuffer-print "Option " (arg 2) ": " (option-value-string (arg 2)))
		  else (display-error "Unknown option: " (arg 2)))
	        else (if (memq (arg 1) list-of-known-options)
		       (option-set-value (arg 1)(arg 2))
		       else (opt 'status (arg 1)))))
	   ((= n 3)	; opt register foo bar
	    (cond ((eq (arg 1) 'register)
		 (register-option (arg 2)(arg 3)))
		(t (opt))))
	   (t (opt))))


(defun option-value-string (x)
       (cond ((not (boundp x)) 'Off)
	   ((numberp (setq x (symeval x)))(decimal-rep x))
	   ((eq x 'nil) 'Off)
	   ((eq x 't) 'On)
	   ((stringp x) x)
	   ((symbolp x) x)
	   (t "-- ???? ---")))


(defun option-set-value (sym val)
       (if (stringp val) (setq val (intern (make_atom val))))
       (cond ((or (and (boundp sym) (numberp(symeval sym)))
	        (get sym 'value-must-be-numeric))
	    (if (numberp val) (set sym val)
	        else (display-error "Value for " sym " must be numeric.")))
	   ((or (and (boundp sym)(memq (symeval sym) '(t nil)))
	        (get sym 'value-ok-true-false))
	    (cond ((memq val '(t T True true ok Ok On ON on y yes Yes))
		 (set sym t))
		((memq val '(nil no No Off OFF off false))
		 ;; I thank Bawden for this randomness
		 (set sym nil))
		((get sym 'value-ok-anything)
		 (set sym val))
		(t (display-error "The value for " sym
			        " must be either ""on"" or ""off""."))))
	   (t (set sym val))))

;;;
;;;	The printing-tty-man's friend,
;;;	BSG 08/06/76
;;;

(defcom view-lines
        &numeric-argument (&pass)
        (if numarg (if (= numarg 0) (view-region-as-lines)
		   else (init-local-displays)
		   (if (< numarg 0)
		       (setq numarg (- numarg))
		       (do y numarg (1- y)(= y 0)
			 (if (firstlinep)
			     (setq numarg (1- numarg))
			     else
			     (prev-line))))
		   (do x numarg (1- x) (< x 1)     ;lisp do
		       (local-display-current-line)
		       (if (or (lastlinep) (= numarg 1)) (stop-doing)
			 else (next-line)))
		   (end-local-displays))
	  else (setq numarg 1) (view-lines)))


(defcom view-region-as-lines
        (with-the-mark-last m
		        (init-local-displays)
		        (do-forever
			(local-display-current-line)
			(if (mark-on-current-line-p m) (stop-doing))
			(next-line))
		        (end-local-displays)))

;;;
;;;	Word Searches.
;;;	BSG 8/26/78
;;;

(defun compare-word-to-list-rep-word (list)
       (prog (text-char)
a 	   (cond ((null list)		;no more source
	    (do-forever
	      (if-at (ascii 10)(forward-char)	;run out trailing _ BS
		   else (if-at "_" (forward-char)
			     else (stop-doing))))
		(cond ((charset-member (curchar) good-word-charactertbl)
		       (search-for-first-not-charset-line good-word-charactertbl)
		       (return nil))	;lost
		      (t (return t))))
	         ((= (car list) 52)		;star
		(search-for-first-not-charset-line good-word-charactertbl)
		(return t)))
	   (if (eolp)			;lost
	       (return nil))
	   (setq text-char (getcharn (curchar) 1))
	   ;; Quick check for good case.

	   (if (and (< text-char (1+ (CtoI "Z")))(> text-char (1- (CtoI "A"))))
	       (setq text-char (+ 40 text-char)))    ;lower case it.
	   (if (= text-char (car list))	;local winnage
	       (setq list (cdr list))
	       (forward-char)
	       (go a))
	   (if (or (= text-char 10)(= text-char 137))	;BS or US
	       (forward-char)
	       (go a))
	   ;; Wholesale mismatch.
	   (search-for-first-not-charset-line good-word-charactertbl)
	   (return nil)))


(defun word-search (word)
	 (setq word (listify-word word))
	 (with-mark m
	 (if (do-forever
	      (if (not (search-charset-forward-nosavepos good-word-charactertbl))
		(return nil))
	      (if (compare-word-to-list-rep-word word)
		(return t))
	      (if (at-end-of-buffer)
		(return nil)))
	    else (go-to-mark m)
	         nil)))


(defun listify-word (w)
       (mapcar '(lambda (x)
		    (cond ((and (< x (1+ (CtoI "Z"))) (> x (1- (CtoI "A")))) (+ x 40))
			(t x)))
	     (exploden w)))


(defcom multi-word-search
        &arguments ((wordlist &default
			&eval (string-to-list-of-strings (get-search-string "Word Search"))))
        &numeric-argument (&pass)
        (let ((bjflag numarg))
	   (setq wordlist (mapcar 'listify-word wordlist))
	   (with-mark m
		    (if bjflag (go-to-beginning-of-buffer))
		    (if
		      (prog (trials temp-list)
			  (return
			    (do-forever
			      (if (not (search-charset-forward-nosavepos  good-word-charactertbl))
				(go-to-mark m)
				(search-failure-annunciator))
			      (setq trials 0 temp-list wordlist)
a 			      (cond ((null temp-list)
				   (return t)))
			      (cond ((compare-word-to-list-rep-word (car temp-list))
				   (setq temp-list (cdr temp-list))
				   (cond ((null temp-list)(return t)))
				   (setq trials (1+ trials))
				   (if (search-charset-forward-nosavepos good-word-charactertbl)
				       else (go-to-mark m) (search-failure-annunciator))
				   (go a)))
			      ;; Has failed. Found 2 good words, 1 bad one. trials = 2.
			      ;; Back up 2 words.
			      (do-times trials  (backward-word)))))
		      else (go-to-mark m)
		      (search-failure-annunciator)))))


(defun string-to-list-of-strings (instr)
       (prog (answer locanswer ch1)
	   (setq instr (exploden instr))
a 	   (do-forever
	     (if (null instr)(stop-doing))
	     (setq ch1 (car instr))
	     (if (or (= ch1 40)(= ch1 11)(= ch1 12)) ;blank, tab newline
	         (setq instr (cdr instr))
	         else (stop-doing)))
	   (if (null instr)			;all done
	       (return (nreverse answer)))
	   ;; collect one string
	   (setq locanswer nil)
	   (do-forever
	     (if (null instr)(stop-doing))
	     (setq ch1 (car instr) instr (cdr instr))
	     (if (or (= ch1 40)(= ch1 11)(= ch1 12))
	         (stop-doing)
	         else
	         (setq locanswer (cons ch1 locanswer))))
	   (setq answer (cons (maknam (nreverse locanswer)) answer))
	   (go a)))


(defun search-charset-forward-nosavepos (charset)
       (do-forever
         (cond ((search-for-first-charset-line charset)
	      (return (curchar)))
	     ((lastlinep)
	      (return nil))
	     (t (next-line)))))

;;;	Not L I S P mode.
;;;	Things dealing with parenthese balancing to placate those
;;;	who miss native Lisp mode.
;;;	BSG 9/11/78

(defun unwind-sexp-searchers-marks-and-nlgoto ()
       (mapc 'release-mark sexp-searcher-mark-list)
       (setq numarg nil)
       (setq sexp-searcher-mark-list  nil)
       (command-quit))


(defcom balance-parens-forward
        &numeric-argument (&repeat)
        &negative-function balance-parens-backward
        (let ((count nil))
	   (save-excursion-on-error
	     (do-forever
	       (dispatch-on-current-char
	         ("("	(if (not count)(setq count 1)
			    else (setq count (+ 1 count))))
	         (")"	(if count (setq count (- count 1)))
			(if (and count (= count 0))
			    (forward-char) (stop-doing))))
	       (if (at-end-of-buffer)
		 (display-error "Unbalanced Parentheses"))
	       (forward-char)))))

(defcom balance-parens-backward
        &numeric-argument (&repeat)
        &negative-function balance-parens-forward
        (let ((count nil))
	   (save-excursion-on-error
	     (do-forever
	       (dispatch-on-lefthand-char
	         (")"	(if (not count)(setq count 1)
			    else (setq count (+ 1 count))))
	         ("("	(if count (setq count (- count 1)))
			(if (and count (= 0 count))
			    (backward-char) (stop-doing))))
	       (if (at-beginning-of-buffer)
		 (display-error "Unbalanced Parentheses"))
	       (backward-char)))))

;;;
;;;		Page-other-window copped from Larry Johnson
;;;		BSG 12/11/78 .. ditto go-to-line-number
;;;


(declare (special selected-window))

(defcom page-other-window
        &numeric-argument (&pass)
        (if (not two-window-mode)
	  (display-error "Not in 2 window mode")
	  else
	  (let ((origwindow selected-window))
	       (unwind-protect
	         (progn
		 (select-other-window)
		 (if (null numarg)(next-screen)
		     else (if (> numarg 0)(next-screen)
			    else (setq numarg (- numarg))
			    (prev-screen))))
	         (select-window origwindow)))))


;;; New non-losing go-to-line-number 22 April 1981 RMSoley
;;; Prompt for line number if wasn't given.
(defcom go-to-line-number
        &numeric-argument (&pass)
        (let ((count numarg))
	   (cond ((null count)
		(setq count
		      (1-
		        (integer-minibuf-response
			"Go to line number: " NL))))
	         ('else
		 (setq count (1- count))))
	   (and (< count 0) (setq count 0))
	   (go-to-beginning-of-buffer)
	   (do-times count
		   (and (lastlinep) (command-quit))
		   (next-line))))

(defun integer-minibuf-response (Question Char)
       (let ((ans (minibuf-response Question Char)))
	  (or (ed-cv-fixnum-check ans)
	      (display-error "Non-numeric answer: " ans "."))))

;;;
;;;	Untabifiers, moved from Rmail 1/18/79
;;;

(defcom untabify
        &arguments ((x &default
		   &eval (if numarg numarg
			   else tab-equivalent)))
        &numeric-argument (&pass)
        (let ((tab-equivalent x))
	   tab-equivalent
	   (save-excursion
	     (go-to-beginning-of-buffer)
	     (do-forever
	       (if (forward-search TAB)
		 (let ((hp (cur-hpos)))
		      (rubout-char)
		      (let ((nhp (cur-hpos)))
			 (do ((x nhp (1+ x)))((= x hp))
			     (insert-char " "))))
		 else
		 (stop-doing))))))

(defcom-synonym mmuntabify untabify)

;;;
;;; Mark commands.
;;;

(defkill wipe-region ambiguous)
(defcom wipe-region
        (cond ((not der-wahrer-mark)
	     (report-error 'mark-not-set)
	     (setq previous-command nil))	;be sure not accidently repeated
	    (t (wipe-point-mark der-wahrer-mark))))

(defcom copy-region
        (cond (der-wahrer-mark (killsave-string
			   (point-mark-to-string der-wahrer-mark)))
	    (t (report-error 'mark-not-set))))

;;;
;;;	Named mark routines  -- BSG 8/19/79
;;;

(defcom set-named-mark
        &arguments ((markname &symbol &prompt "Set named mark: "
			&default &eval (display-error "You must supply a mark name.")))
        (set-named-mark- markname)
        (minibuffer-print "Set " markname))


(defcom go-to-named-mark
        &arguments ((mn &symbol &prompt "Go to named mark: "
		    &default &eval (display-error "You must supply a mark name.")))
        (let ((m (get-named-mark mn)))
	   (set-the-mark)
	   (go-to-mark m)))



(defcom list-named-marks ()
        (let ((ml (produce-named-mark-list)))
	   (if (null ml)(display-error "No named marks in this buffer."))
	   (init-local-displays)
	   (local-display-generator-nnl "Line #	Mark name")
	   (local-display-generator-nnl "")
	   (save-excursion
	     (let ((lnno 1))
		(go-to-beginning-of-buffer)
		(do-forever
		  (mapc '(lambda (x)
			       (if (mark-on-current-line-p (cadr x))
				 (local-display-generator-nnl
				   (catenate (decimal-rep lnno) TAB (car x)))))
		        ml)
		  (if (lastlinep)(stop-doing))
		  (setq lnno (1+ lnno))
		  (next-line))))
	   (end-local-displays)))

(defun date ()				;general utility BSG 10/31/79
       (let ((statdate (mapcar 'decimal-rep (status date))))
	  (catenate (cadr statdate)
		  "/"
		  (caddr statdate)
		  "/"
		  (car statdate))))


;;; Buffer commands.

;;; Command to switch buffers
;;; Fixed 17 April 1981 RMSoley to prompt with default.
;;; Added &completions 6 August 1981 RMSoley
(defcom select-buffer
        &prologue insure-intelligent-previous-buffer
        &arguments ((buffer &symbol
		        &prompt &eval (catenate "Select buffer ("
					  previous-buffer
					  "): ")
		        &completions known-buflist))
        (select-buffer-window buffer 'default-cursize))

;;; Make sure that previous-buffer isn't nil or same as current
;;; buffer for select-buffer and kill-buffer.
;;; 6 Aprill 1982 Richard Soley
(defun insure-intelligent-previous-buffer ()
       (or (exists-buffer previous-buffer)
	 (setq previous-buffer
	       (cond ((eq (car known-buflist) current-buffer)
		    (cadr known-buflist))
		   ('else (car known-buflist)))))
       (and (eq previous-buffer current-buffer)
	  (setq previous-buffer 'main))
       (or previous-buffer (setq previous-buffer 'main)))

;;; Command to delete a buffer
;;; Fixed 17 April 1981 RMSoley to prompt with default.
;;; Fixed 6 August 1981 RMSoley for &completions
(defcom kill-buffer
        &arguments ((buffer &symbol &prompt "Kill buffer: "
		        &completions known-buflist
		        &default &eval current-buffer))
        (cond ((not (memq buffer known-buflist))
	     (display-error "Buffer does not exist: " buffer))
	    ((eq buffer current-buffer)
	     (insure-intelligent-previous-buffer)
	     (do nil (nil)
	         (ring-tty-bell)
	         (let ((newbuf
		       (let ((completion-list known-buflist))
			  (intern-minibuf-response
			    (catenate
			      "Killing current buffer.  "
			      "Select new buffer ("
			      previous-buffer
			      "): ")
			    NL))))
		    (and (nullstringp newbuf)
		         (progn (setq newbuf previous-buffer)
			      (minibuffer-print-noclear newbuf)))
		    (cond ((eq buffer newbuf)
			 (display-error-noabort "Killing buffer "
					    buffer
					    ". Can't go there."))
			(t (select-buffer-window newbuf 'default-cursize)
			   (buffer-kill buffer)
			   (return nil)))))
	     (setq previous-buffer
		 (cond ((eq (car known-buflist) current-buffer)
		        (cadr known-buflist))
		       (t (car known-buflist))))
	     (or previous-buffer (setq previous-buffer current-buffer)))
	    (t (buffer-kill buffer))))

;;; Command to mark the current buffer unmodified
(defcom unmodify-buffer 
        (setq buffer-modified-flag nil
	    damaged-flag t)			;makes redisplay happy
        (close-line)			;lest this, nobody'd check
        (minibuffer-remark "Not modified."))

;;; Yank and search commands and esc-esc.

(defkill merge-last-kills-with-next ambiguous)
(defcom merge-last-kills-with-next nil)

(defcom yank-minibuf
        (set-the-mark)
        (insert-string last-minibuf-response))

;;; JSL's new searching commands - June 1982
;;; These are changed to lowercase the noninitial words in the prompt,
;;; and to only set gratuitous marks when the search succeeds.
;;; If the search succeeds partially, the mark is always set.

(defcom string-search
        &cleanup search:command-cleanup
        &prologue search:command-prologue
        &epilogue search:command-epilogue
        &inverse reverse-string-search
        &negative-function reverse-string-search
        &numeric-argument &repeat
        &arguments ((search-string &string &default
			     &eval (get-search-string
				   (search:numeric-prompt
				     "String search"))))
        (forward-search search-string))


(defcom reverse-string-search
        &cleanup search:command-cleanup
        &prologue search:command-prologue
        &epilogue search:command-epilogue
        &inverse string-search
        &negative-function string-search
        &numeric-argument &repeat
        &arguments ((search-string &string &default
			     &eval (get-search-string
				   (search:numeric-prompt
				     "Reverse string search"))))
        (reverse-search search-string))

(defun search:command-prologue ()
       (cons 0 (set-mark)))


(defun search:command-cleanup (prologue-info)
       (if prologue-info
	 (if (cdr prologue-info)
	     (go-to-mark (cdr prologue-info))
	     (release-mark (cdr prologue-info)))))


(defun search:command-epilogue (prologue-info result last-time)
       (cond (result
	     (rplaca prologue-info (1+ (car prologue-info)))
	     (and last-time
		(or (gratuitous-mark-setter (cdr prologue-info))
		    (release-mark (cdr prologue-info)))
		(rplacd prologue-info nil)))		; For cleanup.
	   ((zerop (car prologue-info))
	    (search-failure-annunciator))
	   (t (set-the-mark-here (cdr prologue-info))
	      (rplacd prologue-info nil)))		; For cleanup.
       (or result (search:announce-partial-failure (car prologue-info))))



(defun search:announce-partial-failure (count)
       (display-error "Search failed after " (decimal-rep count)
		  " successful search"
		      (cond ((= count 1) "") (t "es"))
		      ".  Mark set at start.")
       (and macro-execution-in-progress (command-quit)))

(defun search-failure-annunciator ()
       (display-error "Search fails.")
       (and macro-execution-in-progress (command-quit)))



(defcom regexp-search-command
        &arguments ((search-string &default
			     &eval (get-search-string "Regexp Search")))
        (setq search-string (forward-regexp-search search-string))
        (cond ((null search-string) (search-failure-annunciator)) 
	    (t				;found it set mark around it
	      (let ((y (set-mark)))		;save-excursion
		 (go-to-mark search-string)
		 (set-the-mark)
		 (go-to-mark y)
		 (release-mark search-string)
		 (release-mark y)))))

;;;
;;;	Query replace by Carl Hoffman
;;;


;;; read macro 12/3/78 by BSG
(eval-when (compile eval)
(setsyntax '/# 'macro
	 '(lambda ()
		(cond ((= (tyipeek) 57)
		       (tyi)
		       (tyi))
		      ((= (tyipeek) 136)
		       (tyi)
		       (- (boole 1 137 (tyi)) 100)))))
);;;end of eval-when

(defcom query-replace
        &arguments ((old &default
		     &eval (get-search-string "Query replace old string"))
		(new &prompt "Query replace new string: " NL))
        (assert-minor-mode '|query replace|)
        (if (not (forward-search old))
	  (minibuffer-print "No occurrences of old string found.")
	  else
	  (query-replace-execute old new)
	  (minibuffer-print "Done."))
        (negate-minor-mode '|query replace|))

;  This function does all of the work.  When it is invoked, the point
;  is to the right of the first occurrence of the old string.

(defun query-replace-execute (old new)
       (catch (do-forever
	      (redisplay)
	      (query-replace-dispatch old new (get-char))
	      (if (not (forward-search old)) (stop-doing)))
	    done))

(defun query-replace-dispatch (old new response)
       (do-forever
         (cond ((= response #/,)
	      (query-replace-swap-strings old new)
	      (redisplay)
	      (stop-doing))
	     ((= response #/ )
	      (query-replace-swap-strings old new)
	      (stop-doing))			;don't redisplay 10/15/80
	     ((or (= response #^M)		;return = 15
		(= response 177))		;rubout = 177
	      (stop-doing))
	     ((= response #/!)		;! is replace to end
	      (query-replace-swap-strings old new)
	      (do-forever
	        (if (forward-search old) (query-replace-swap-strings old new)
		  else (throw t done))))
	     ((= response #/.)
	      (query-replace-swap-strings old new)
	      (throw t done))
	     ((or (= response #^G) (= response 33)) ;altmode
	      (throw t done))
	     ((= response #^J))		;newline = 12
	     ((= response #^L)
	      (redisplay))
	     ((or (= response #/?) (= response #^_))
	      (query-replace-documentation))
	     (t (display-error-noabort "Unknown query replace response.")
	        (redisplay)))
         (setq response (get-char))))

(defun query-replace-swap-strings (old new)
       (with-mark m
	        (backward-n-chars (stringlength old))
	        (without-saving (wipe-point-mark m))
	        (insert-string new)))

(defun query-replace-documentation ()
       (init-local-displays)
       (mapc 'local-display-generator-nnl
	   '("Query replace options:" ""
	     "SPACE         Replace this occurrence and go on"
	     "CR, DEL       Skip this occurrence and go on"
	     ". (period)    Replace this occurrence and stop"
	     "^G, ESC       Stop now"
	     ", (comma)     Replace this occurrence, redisplay, and go on"
	     "!             Replace all following occurrences without querying"
	     "LF            Nothing"
	     "^L            Redisplay"
	     "?, ^_         Print this description"
	     "" "Type any character to remove this display."))
       (end-local-displays)
       (redisplay)
       (get-char))

(defcom eval-multics-command-line
        &arguments ((command-line &prompt "Multics: "))
        (let ((e-quit-transparency 'transparent))
	   e-quit-transparency
	   (unwind-protect
	     (progn (e_pl1_$set_multics_tty_modes)
		  (e_cline_ command-line))
	     (iox_$control (e_pl1_$get_iocb) "reset_more" null-pointer)
	     (e_pl1_$set_emacs_tty_modes))))

(declare (special eval:eval eval:assume-atom eval:correct-errors
	        eval:prinlevel eval:prinlength + - *))

(defcom eval-lisp-line
        &numeric-argument (&pass)
        (cond ((not eval:eval)
	     (extended-command (minibuffer-response "Command: ")))
	    (t (eval:internal  (minibuffer-response "Eval: ")))))

(defun eval:internal (lisp-form)
       (let ((prinlevel eval:prinlevel) (prinlength eval:prinlength))
	  (eval:evaluate
	    (let ((e-lisp-error-mode 'read-lisp-fun-lossage-handler))
	         e-lisp-error-mode
	         (read-from-string (evl-parenify lisp-form))))))

(defun eval:evaluate (form)
       (setq - form)
       (setq * (let ((e-lisp-error-mode 'eval-lisp-line-lossage-handler))
		(eval form)))
       (setq + form)
       (minibuffer-print "Value: " (maknam (explode *))))

(defun paren-counter (string)
       (let ((left 0)
	   (right 0)
	   (balanced nil)
	   (quote-open nil)
	   (bar-open nil)
	   (spacep nil)
	   (skip-this-char nil)
	   (in (explodec string))
	   (len (stringlength string)))
	  (do ((sofar in (cdr sofar))
	       (a 1 (1+ a)))
	      ((> a len))
	      (let ((this (car sofar))
		  (open (or bar-open quote-open)))
		 (cond (skip-this-char (setq skip-this-char nil))
		       ((eq this '/" )
		        (or bar-open
			  (setq quote-open (null quote-open)))
		        (or open (setq spacep t)))
		       ((eq this '/| )
		        (or quote-open
			  (setq bar-open (null bar-open)))
		        (or open (setq spacep t)))
		       ((eq this '/  )
		        (or open (setq spacep t)))
		       ((eq this '// )
		        (or open (setq skip-this-char t)))
		       ((eq this '/; )
		        (or open (setq a len)))
		       ((eq this '/' )
		        (or open (setq spacep t)))
		       ((eq this '/( )
		        (or open (setq left (1+ left))))
		       ((eq this '/) )
		        (or open (setq right (1+ right))))))
	      (cond ((not (or (= a 1) (= a len)))
		   (setq balanced (or balanced (= left right))))))
	  (cons left
	        (cons right
		    (cons balanced
			(cond (quote-open
			        (list 'error
				    "unbalanced quotes."))
			      (bar-open
			        (list 'error
				    "unbalanced vertical bars."))
			      (skip-this-char
			        (list 'error
				    "too much slashification."))
			      (t spacep)))))))

(defun evl-parenify (string)
       (let ((parens (paren-counter string)))
	  (let ((l>r (> (car parens) (cadr parens)))
	        (dif (abs (- (car parens) (cadr parens))))
	        (symbol (and (not (cadddr parens))
			 (= (car parens) 0)
			 (= (cadr parens) 0)
			 eval:assume-atom))
	        (balanced (caddr parens)))
	       (cond ((not (or eval:correct-errors (zerop dif)))
		    (display-error "Error in syntax of Lisp form - "
			         "unbalanced parentheses."))
		   ((eq (cadddr parens) 'error)
		    (or eval:correct-errors
		        (display-error
			"Error in syntax of Lisp form - "
			(car (cddddr parens))))
		    (evl-parenify	;fix user's error
		      (catenate
		        string
		        (let ((kludge (substr (car (cddddr parens))
					12.
					1)))
			   (cond ((samepnamep kludge "q") """")
			         ((samepnamep kludge "v") "|")
			         (t " "))))))
		   (t (let ((intermediate
			    (cond ((= dif 0) string)
				(l>r
				  (catenate string
					  (gen-repetitive dif ")")))
				(t (catenate (gen-repetitive dif "(")
					   string)))))
			 (cond (symbol string)
			       (balanced
			         (catenate "(" intermediate ")"))
			       (t intermediate))))))))

(defun gen-repetitive (number string)
       (and (nullstringp string) (setq string SPACE))
       (and (minusp number) (setq number 0))
       (do ((string string (catenate string string)))
	 ((not (< (stringlength string) number))
	  (substr string 1 number))))

(defun eval-lisp-line-lossage-handler x x (command-quit))

(defun read-lisp-fun-lossage-handler (arg)
       arg
       (display-error "Syntax error in Lisp form."))
