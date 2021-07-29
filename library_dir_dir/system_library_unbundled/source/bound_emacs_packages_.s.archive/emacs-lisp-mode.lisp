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
;;;	Lisp Mode.  Extracted and modified from e_macops_,

;;; HISTORY COMMENTS:
;;;  1) change(80-05-06,Greenberg), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;               BSG & WMY 9/11/78
;;;               GMP, 09/16/78 to add evaluation functions.
;;;               Indented by indent-to-lisp 9/18!!
;;;               Hook to LDEBUG BSG 2/25/79
;;;               Clean up compiler segs, elcp feature, backquote, comma BSG 5/6/80
;;;  2) change(85-01-03,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Changed eval-lisp-region to load e_macros_ and e_define_command_,
;;;     defvar'ed loaded-e-macros.
;;;  3) change(85-01-27,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Declared lots of functions *expr.
;;;                                                      END HISTORY COMMENTS


(%include e-macros)

(declare (special
	 OPEN-PAREN CLOSE-PAREN SEMI SINGLEQUOTE DOUBLEQUOTE SLASH
	 BACKQUOTE COMMA elcp
	 lisptable sexp-searcher-mark-list instack infile
	 env-dir lisp-indent-fuzz
	 fill-column comment-column comment-prefix
	 current-buffer-mode whitespace-charactertbl
	 include-dir tty-no-upmotionp)
         (*lexpr comout-get-output))
(declare (*expr delete_$path hcs_$initiate_count indent-for-comment
	      kill-contents-of-line mark-whole-buffer one-back-is-a
	      redisplay-current-window-relative search-charset-backwards
	      search-charset-forward unwind-sexp-searchers-marks-and-nlgoto
	      view-region-as-lines))

(setq OPEN-PAREN '/( CLOSE-PAREN '/) SEMI '/;
      DOUBLEQUOTE '/" SLASH '// SINGLEQUOTE '/' BACKQUOTE '/` COMMA '/,)


(defvar ((sexp-searcher-mark-list nil)
         (elcp t) ;t 9/12/80
         (lisp-mode-clean-up-lcp-temps-list nil)
         (lisptable (charscan-table (catenate TAB SPACE SEMI OPEN-PAREN CLOSE-PAREN NL 
				      COMMA DOUBLEQUOTE SINGLEQUOTE SLASH BACKQUOTE)))
         (lisp-mode-hook nil)))

(register-option 'elcp t)

(define-autoload-lib emacs-lisp-debug-mode ldebug-set-break)

;;; Extended command to enter LISP mode
(defun lisp-mode ()
       (establish-local-var 'compiler 'lisp_compiler)
       (establish-local-var 'compile-options "")
       (setq current-buffer-mode 'Lisp
	   comment-column 50.
	   comment-prefix ";")
       (mapc '(lambda (x)
		  (set-key (car x) (cadr x)))
	   '((TAB indent-to-lisp)
	     ("ESC-(" lisp-one-less-paren)
	     ("ESC-)" lisp-one-more-paren)
	     (ESC-/& ldebug-set-break)
	     (ESC-Q  lisp-indent-function)
	     (ESC-^A begin-defun)
	     (ESC-^B backward-sexp)
	     (ESC-^C compile-function)
;;;	     (^Z^C   compile-buffer)		;file-output kind
	     (ESC-^D down-list-level)
	     (ESC-^E end-defun)
	     (ESC-^F forward-sexp)
	     (ESC-^H mark-defun)
	     (ESC-^I indent-to-lisp)
	     (ESC-^K kill-sexp)
	     (ESC-^M lisp-cret-and-indent)
	     (ESC-^N forward-list)
	     (ESC-^P backward-list)
	     (ESC-^Q lisp-indent-region)
	     (ESC-^R move-defun-to-screen-top)
	     (ESC-^T mark-sexp)
	     (ESC-^Z eval-top-level-form)))
       (if tty-no-upmotionp			;if not on a display
	 (set-key 'ESC-^V 'view-defun))	;add this useful function
       (and lisp-mode-hook (errset (funcall lisp-mode-hook))))

(defun begin-defun ()
       (do-forever
         (go-to-beginning-of-line)
         (if (firstlinep) (stop-doing))
         (if-at OPEN-PAREN (stop-doing))
         (prev-line)))


(defun end-defun ()
       (begin-defun)
       (forward-sexp))


(defun mark-defun ()
       (begin-defun)
       (set-the-mark)
       (forward-sexp))


(defun view-defun ()
       (mark-defun)
       (view-region-as-lines))


(defun skip-lisp-whitespace-and-comments ()
       (do-forever
         (skip-over-whitespace)
         (dispatch-on-current-char
	 (SEMI
	   (if (lastlinep)(stop-doing))
	   (next-line)
	   (go-to-beginning-of-line))
	 (else (stop-doing)))))

(defun forward-sexp ()
       (prog ()
	   (skip-close-parens-and-comments-and-whitespace)
retry
	   (dispatch-on-current-char
	     (CLOSE-PAREN   (return t))
	     (OPEN-PAREN    (forward-char)
			(forward-list))
	     (SINGLEQUOTE   (forward-char)(forward-sexp))
	     (BACKQUOTE     (forward-char)(forward-sexp))
	     (COMMA         (forward-char)(forward-sexp))
	     (SLASH         (forward-char)
			(forward-char)
			(go retry))
	     (DOUBLEQUOTE   (forward-char)
			(if (forward-search DOUBLEQUOTE)
			    else
			    (display-error-noabort "Unbalanced doublequote.")
			    (unwind-sexp-searchers-marks-and-nlgoto))
			(if-at DOUBLEQUOTE (go retry))
			(return nil))
	     (else (if (search-charset-forward lisptable)
		     (if-at SLASH (forward-char)
			  (forward-char)
			  (go retry))
		     (return t)
		     else (error "forward-sexp: whaah? delim?"))))))

(defun skip-close-parens-and-comments-and-whitespace ()
       (do-forever
         (skip-lisp-whitespace-and-comments)
         (dispatch-on-current-char
	 (CLOSE-PAREN (forward-char))
	 (else (stop-doing)))))

(defun forward-list ()
       (skip-lisp-whitespace-and-comments)
       (with-mark mm
	        (setq sexp-searcher-mark-list (cons mm sexp-searcher-mark-list))
	        (if (at-end-of-buffer) (display-error "Unbalanced Parentheses")
		  else
		  (do-forever
		    (if-at CLOSE-PAREN (forward-char)(stop-doing))
		    (if (at-end-of-buffer)
		        (display-error-noabort "Unbalanced Parentheses.")
		        (go-to-mark mm)
		        (unwind-sexp-searchers-marks-and-nlgoto))
		    (if (or (at-white-char)(looking-at ";"))
		        (skip-lisp-whitespace-and-comments)
		        (if-at CLOSE-PAREN (forward-char)(stop-doing)))
		    (forward-sexp)))))

(defun down-list-level ()
       (do-forever
         (skip-close-parens-and-comments-and-whitespace)
         (if (at-end-of-buffer)(stop-doing))
         (if-at "(" (forward-char)(stop-doing))
         (forward-sexp)))

(defprop nextlist-sexp forward-list expr)

(defun backward-sexp ()
       (prog ()
	   (skip-backwards-open-parens-comments-and-other-cruft)
retry
	   (if (one-back-is-a SLASH)
	       (if (and (back-at """")
		      (lisp-mode-slash-quote-sneak))	;heh heh
		 else
		 (do-times 2 (backward-char))
		 (go retry)))

	   (dispatch-on-lefthand-char
	     (OPEN-PAREN	(return t))
	     (CLOSE-PAREN	(backward-char)	;get closeparen out
			(backward-list)
			(do-forever (if (memq (lefthand-char)
					  '(/' /` /,))
				      (backward-char)
				      else (stop-doing))))
	     (SINGLEQUOTE	(backward-char)(go retry))
	     (BACKQUOTE	(backward-char)(go retry))
	     (COMMA	(backward-char)(go retry))
	     (DOUBLEQUOTE	(backward-char)
			(if (reverse-search DOUBLEQUOTE)
			    else (display-error-noabort "Unbalanced Doublequote.")
			    (unwind-sexp-searchers-marks-and-nlgoto))
			(if-back-at DOUBLEQUOTE (go retry))
			(return nil))
	     (else (if (search-charset-backwards lisptable)
		     (if (one-back-is-a SLASH)(go retry))
		     (do-forever (if-back-at SINGLEQUOTE (backward-char)
				         else (stop-doing)))
		     (return t)
		     else (return nil))))))

(defun lisp-mode-slash-quote-sneak ()
       (save-excursion
         (with-mark m			;go thru balancing act
		(let ((qct))
		     (go-to-beginning-of-line)
		     (do-forever
		       (if (mark-reached m)(return t))
		       (dispatch-on-current-char
		         (SEMI   (if qct (forward-char)
				 else (return t)))	; WAS quoted
		         (DOUBLEQUOTE (setq qct (not qct))
				  (forward-char))
		         (SLASH  (forward-char)
			       (if (mark-reached m)(return t))
			       ;; The above should never happen.
			       (if (not qct)
				 (forward-char)
				 (if (mark-reached m)(return nil))))
		         ;; The above finds slashed quotes.
		         (else  (forward-char))))))))

(defun backward-list ()
       (with-mark mm
	        (setq sexp-searcher-mark-list (cons mm sexp-searcher-mark-list))
	        (if (at-beginning-of-buffer)
		  ;;fall through to test for same below
		  else
		  (do-forever
		    (if-back-at OPEN-PAREN (backward-char)(stop-doing))
		    (if (at-beginning-of-buffer)
		        (display-error-noabort "Unbalanced Parentheses.")
		        (go-to-mark mm)
		        (unwind-sexp-searchers-marks-and-nlgoto))
		    (if (or (bolp)(get (lefthand-char) 'whiteness))
		        (skip-backwards-lisp-whitespace-comment-cruft)
		        (if (and (back-at OPEN-PAREN)
			       (not (one-back-is-a SLASH)))
			  (backward-char)
			  (stop-doing)))
		    (backward-sexp)))))

(defun skip-backwards-open-parens-comments-and-other-cruft ()
       (do-forever
         (skip-backwards-lisp-whitespace-comment-cruft)
         (dispatch-on-lefthand-char
	 (OPEN-PAREN (backward-char)
		   (if-back-at SLASH (forward-char)(stop-doing)))
	 (else (stop-doing)))))

(defun skip-backwards-lisp-whitespace-comment-cruft ()
       (do-forever
tbolp    (if (at-beginning-of-buffer)(stop-doing))
         (if (bolp)(backward-char)
	   (if (bolp)(go tbolp))
	   (skip-backwards-possible-lisp-comment)
	   (go tbolp))
         (dispatch-on-lefthand-char
	 (TAB		(backward-char))
	 (SPACE		(backward-char))
	 (NL		(backward-char))
	 (SLASH		(forward-char)(stop-doing))
	 (else		(stop-doing)))))

(defun skip-backwards-possible-lisp-comment ()
       (go-to-end-of-line)
       (find-lisp-comment-start))

(defun find-lisp-comment-start ()
       (prog (qct foundit)
	   (go-to-beginning-of-line)
	   (if (not (forward-search-in-line ";"))
	       (go-to-end-of-line)
	       (return nil)
	       else (go-to-beginning-of-line))
	   (setq qct nil)
	   (do-forever
	     (if (eolp)(stop-doing))
	     (dispatch-on-current-char
	       (DOUBLEQUOTE (setq qct (not qct))(forward-char))
	       (SEMI	(if qct (forward-char)
			    else (setq foundit t)
			    (stop-doing)))
	       (SLASH	(forward-char)
			(if (eolp)(stop-doing))
			(if (not qct) (forward-char)))
	       (else	(forward-char))))
	   (return foundit)))

(defprop prevlist-sexp backward-list expr)

(defun mark-sexp ()
       (skip-lisp-whitespace-and-comments)
       (if-at CLOSE-PAREN (forward-char)
	    else (forward-sexp))
       (set-the-mark)
       (backward-sexp)
       (exchange-point-and-mark))

(defprop kill-sexp forward kills)
(defun kill-sexp ()(with-mark m
			(forward-sexp)
			(kill-backwards-to-mark m)
			(merge-kills-forward)))

(defun move-defun-to-screen-top ()
       (begin-defun)
       (redisplay-current-window-relative 0))
;;;
;;;
;;;	Your're not going to believe this, but...
;;;	Function compiling functions.
;;;	BSG and archy 7/28/78
;;;

(defun compile-function ()
       (prog (fnname)
	   (if elcp (return (elcp-compile-top-level-form-from-buffer)))
	   (compile-string
	     (save-excursion
	       (begin-defun)
	       (down-list-level)
	       (forward-sexp)
	       (skip-lisp-whitespace-and-comments)
	       (with-mark n
		        (forward-sexp)
		        (killsave-string (setq fnname (point-mark-to-string n))))
	       (begin-defun)
	       (with-mark m
		        (forward-sexp)
		        (point-mark-to-string m)))
	     fnname)))

(defun compile-string (stuff function-name)
       (set-emacs-epilogue-handler  '(lisp-mode-clean-up-lcp-temps) t)
       (let ((source-name (catenate process-dir ">!!e!lcptemp!.lisp"))
	   (object-name (catenate "!ect" (maknam (explodec (runtime))))))
	  (save-excursion-buffer
	    (go-to-or-create-buffer 'compiler-temp)
	    (putprop current-buffer t 'temporary-buffer)
	    (setq buffer-modified-flag t)
	    (destroy-buffer-contents)
	    (insert-string "(declare (use c))")
	    (new-line)
	    (insert-string "(declare (setq seg-name ""[pd]>")
	    (insert-string object-name)
	    (insert-string """)(use w))")
	    (new-line)
	    (insert-string "(declare (inpush (openi """)
	    (insert-string include-dir)
	    (insert-string ">e-macros.incl.lisp"")))")
	    (new-line)
	    (insert-string stuff)
	    (write-out-file source-name)
	    (setq lisp-mode-clean-up-lcp-temps-list
		(cons object-name lisp-mode-clean-up-lcp-temps-list))
	    (display-error-noabort "Compiling " function-name " ..."))
	  (display-as-printout
	    (comout-get-output "lisp_compiler" source-name))
	  (loadfile (catenate process-dir ">" object-name))
	  (sstatus uuolinks nil)))

(defun lisp-mode-clean-up-lcp-temps ()
       (delete_$path process-dir "!!e!lcptemp!.lisp" (lsh 44 30.) "emacs")
       (mapc '(lambda (x)
		  (delete_$path process-dir x (lsh 44 30.) "emacs"))
	   lisp-mode-clean-up-lcp-temps-list))

;;; 

;;;
;;;	Functions for evaluating LISP
;;;	 GMP, 09/16/78
;;;


(defvar loaded-e-macros nil)			; non-nil => don't loadlib e-macros

(defun eval-lisp-region ()			; evaluate the current region
       (with-the-mark-last
         m
         (if (not loaded-e-macros)
	   (load (catenate env-dir ">e_macros_"))
	   (load (catenate env-dir ">e_define_command_"))
	   (setq loaded-e-macros t))
         (let ((answer (car (errset
			(eval (read-from-string
			        (catenate "(progn "
				        (point-mark-to-string m)
				        " )")))))))
	    (let ((prinlevel 3)
		(prinlength 6))
	         (minibuffer-print "Value: " (maknam (explode answer)))))
         (do ((next-file infile (car instack)))
	   ((eq infile t))
	   (close next-file))		; close any loaded files
         (sstatus uuolinks nil)))

(defun eval-top-level-form ()			; command (ESC-^Z) to evaluate form
       (save-excursion
         (mark-defun)			; marks any form starting in column one
         (eval-lisp-region)))


(defun eval-buffer ()			; extended command to eval buffer
       (save-excursion
         (mark-whole-buffer)
         (eval-lisp-region)))

;;; 

;;;
;;;	Lisp indenter
;;;	Made winning 9/18 by archy & BSG
;;;

(register-option 'lisp-indent-fuzz 1)

(defun indent-to-lisp ()			;this one's a goody, kids!
       (go-to-beginning-of-line)
       (indent-to-lisp-1))

(defun indent-to-lisp-1 ()
       (if (charset-member (curchar) lisptable)
	 (delete-white-sides)
	 (if (not (bolp))(insert-char " "))
	 (whitespace-to-hpos
	   (save-excursion

	     (do-forever			;get to right line
	       (backward-sexp)
	       (if (not (and (bolp)(not (charset-member (curchar) lisptable))))
		 (stop-doing)))		;find non-label last sexp

	     (cond ((not (skip-back-whitespace-in-line))) ;'twas all white
		 ((back-at "(") (skip-over-whitespace)) ; (cond ((FOO.. etc
		 (t (with-mark
		      start-of-predecessor
		      (backward-list)
		      (if (mark-on-current-line-p start-of-predecessor)
			(down-list-level)
			(forward-sexp)
			(skip-lisp-whitespace-and-comments)
			else
			(down-list-level)
			(do-forever
			  (skip-lisp-whitespace-and-comments)
			  (if (and (mark-on-current-line-p start-of-predecessor)
				 (or (mark-reached start-of-predecessor)
				     (and (bolp)(at "("))
				     (not (bolp))))
			      (stop-doing))
			  (forward-sexp))))))
	     (if (and (back-at OPEN-PAREN)
		    (not (at OPEN-PAREN)))
	         (+ (cur-hpos) lisp-indent-fuzz)
	         else (cur-hpos))))
	 else
	 (forward-sexp)
	 (search-for-first-not-charset-line whitespace-charactertbl)
	 (if (not (or (eolp)(at ";")))
	     (indent-to-lisp-1))))

(defun lisp-cret-and-indent ()
       (delete-white-sides)
       (new-line)
       (insert-char " ")			;not a label
       (indent-to-lisp))

(defun lisp-indent-region ()
       (copy-region)
       (with-the-mark-last
         m
         (do-forever
	 (if (line-is-blank)(without-saving (kill-contents-of-line))
	     else
	     (go-to-beginning-of-line)	;Rule out comment lines
	     (if-at OPEN-PAREN		;Don't indent these lines.
		  else (search-for-first-not-charset-line whitespace-charactertbl)
		  (if (not (at ";"))(indent-to-lisp)))
	     (if (find-lisp-comment-start)
	         (place-lisp-comments)))
	 (if (mark-on-current-line-p m)(stop-doing))
	 (next-line)))))


(defun place-lisp-comments ()
       (cond ((looking-at ";;;")(delete-white-sides))
	   ((looking-at ";;")(indent-to-lisp))
	   (t (indent-for-comment))))

(defun lisp-indent-function ()
       (mark-defun)
       (lisp-indent-region))




;;;
;;;	BSG 5/6/80 put his favorite two fcns here..
;;;

(defcom lisp-one-more-paren
        &na (&repeat)
        (save-excursion
	(go-to-beginning-of-line)
	(skip-backwards-lisp-whitespace-comment-cruft)
	(insert-char ")"))
        (indent-to-lisp))

(defcom lisp-one-less-paren
        &na (&repeat)
        (save-excursion
	(go-to-beginning-of-line)
	(skip-backwards-lisp-whitespace-comment-cruft)
	(if-back-at ")"
		  (rubout-char)
		  else
		  (display-error "Previous s-exp doesn't end in close paren.")))
        (indent-to-lisp))


;;;
;;;   In-house LCPery, integrated 5/6/80
;;;

;;;
;;; 5/1/80 BSG
;;;

(declare (*expr runoff-fill-region compile-top-level-forms))
(declare (special elcp-@seg-name lisp-system-dir elcp-internmes elcp-spake))
(declare (special elcp-@undfuns elcp-@being-compiled))

(setq elcp-internmes
      ;;This slight inelegance has to duplicate the global list of the compiler
      ;;because by time the compiler can even be looked at, it has already
      ;;interned its own things on the wrong obarray.  This is unclean, but..
      '(cf cl pause genprefix nfunvars special fixnum flonum fixsw flosw notype arith array* closed muzzled
	unspecial reducible irreducible noargs mapex symbols lisp
	put-in-tree	;request of H. Lieberman
	expr-hash system-file compile-top-level-forms	;for GSB & BSG 5/4/80
	sobarray cobarray eoc-eval compiler-state compile maklap top-level coutput gofoo ;jonl's crocks for owl
	nocompile
	-db -debug -eval -tm -time -times -ps -pause -pause_at -mc -macros -gp -gnp
	-genprefix -nw -nowarn -tt -total -total_time -list -ls -long -lg
	-all_special -pathname -pn -p -no_compile -ncp
	-ck -check -ioc -messioc -mioc -hd -hold -pedigree -pdg -brief -bf arith
	*expr *fexpr *lexpr **array messioc check debug macros dataerrp barfp
	defpl1 update return ignore fixed bin binary float packed-pointer packed-ptr
	pointer ptr bit aligned unaligned character varying char lisp array
	l le g ge n e))

(defun elcp-load-lcp ()
       (let ((obarray (get '*VIRGIN-OBARRAY* 'array))
	   (errlist errlist))		;clever bastard
	  (makoblist 'compiler-obarray)
	  (setq obarray (get 'compiler-obarray 'array))
	  (mapc 'intern elcp-internmes)
	  (putprop (intern (copysymbol 'use nil)) 'elcp-use 'expr)
	  (putprop (intern (copysymbol 'global nil)) 'elcp-global 'expr)
	  (setq elcp-@seg-name (intern (copysymbol 'seg-name nil)))
	  (setq elcp-@undfuns (intern (copysymbol 'undfuns nil)))
	  (setq elcp-@being-compiled (intern (copysymbol 'being-compiled nil)))
	  (set (intern (copysymbol 'compiler-revision nil)) "Emacs")
	  (mapc '(lambda (x)
		       (hcs_$initiate_count lisp-system-dir x  x 0)
		       ;; lisp_cg_utility_ snaps link to x$symbol_table
		       (load (catenate lisp-system-dir ">" x)))
	        '(lcp_semant_ lcp_cg_))
	  (putprop (intern (copysymbol 'printmes nil)) 'elcp-lcp-error-printer 'expr)))


(defun elcp-use fexpr (x)
       (let ((x (getchar (car x) 1)))		; get the first char of the argument.
	  (cond ((eq x 'c) (setq obarray (get 'compiler-obarray 'array))
		         'compiler-obarray)
	        ((eq x 'w) (setq obarray (get 'obarray 'array))
		         'working-obarray)
	        ((eq x 'n) (setq obarray (get '*VIRGIN-OBARRAY* 'array))
		         (makoblist 'obarray) ; copy it
		         (setq obarray (get 'obarray 'array))
		         'new-working-obarray)
	        (t (display-error-noabort "use: argument must be c, w, or n.")))
	  nil))

(defun elcp-global fexpr (x)
       (let ((obarray (get 'obarray 'array)))
	  (mapc '(lambda (y)
		       (setq x (intern y))
		       (or (eq x y)
			 (display-error-noabort "elcp-global: obarray ""already interned"" conflict: " y)))
	        x)))

(defun cfun (fname)
       (let ((prop (getl fname '(expr fexpr macro))))
	  (or prop
	      (display-error "cfun: " fname " not a function"))
	  (elcp-compile-and-load
	    `((defprop ,fname ,(cadr prop) ,(car prop))))))

(defun elcp-compile-and-load (forms)
       (set-emacs-epilogue-handler  '(lisp-mode-clean-up-lcp-temps) t)
       (setq elcp-spake nil)
       (if (null (get 'compiler-obarray 'array))
	 (display-error-remark "Loading LCP into Emacs environment...")
	 (elcp-load-lcp)
	 (display-error-remark "Precompiling e-macros.incl.lisp...")
	 (setq loaded-e-macros t)
	 (compile-top-level-forms
	   `((declare
	       (setq eoc-eval		;idea is no obj seg.
		   '((cf ,(catenate include-dir ">e-macros.incl.lisp"))))))
	   nil))
       (let ((segname
	     (catenate "!ect" (maknam (explodec (runtime)))))
	   (fname (cond ((or (atom forms)(atom (car forms))))
		      ((cdr forms) "...")
		      ((memq (caar forms) '(defun defcom define-command defmacro defstruct))
		       (cadar forms))
		      (t "...."))))
	  (setq lisp-mode-clean-up-lcp-temps-list
	        (cons segname lisp-mode-clean-up-lcp-temps-list))
	  (display-error-remark "Compiling " fname "...")

	  (compile-top-level-forms forms (catenate "[pd]>" segname))

	  (let ((undfuns (symeval elcp-@undfuns)))
	       (setq undfuns
		   (mapcan '(lambda (x)(cond ((getl x '(subr lsubr fsubr expr fexpr))
					nil)
				         (t (list x))))
			undfuns))
	       (if undfuns
		 (elcp-lcp-error-printer undfuns " - functions referenced but not defined. " nil)
		 (set elcp-@undfuns nil)))
	  (minibuffer-print-noclear " Loading ..")
	  (loadfile (catenate process-dir ">" segname))
	  (sstatus uuolinks nil)
	  (and (symbolp fname)(killsave-string fname))
	  (minibuffer-print "Compiled."))
       (and elcp-spake (end-local-displays)))

(defun elcp-compile-top-level-form-from-buffer ()
       (let ((stuff
	     (save-excursion (mark-defun)
			 (with-the-mark-last
			   m
			   (car (errset (read-from-string
				        (point-mark-to-string m))))))))
	  (elcp-compile-and-load (list stuff))
	  (and stuff
	       (not (atom stuff))
	       (not (cdr stuff))
	       (not (atom (car stuff)))
	       (cdar stuff)
	       (memq (caar stuff)'(defun defmacro defcom define-command defstruct))
	       (putprop (caar stuff) current-buffer 'tagbuf))))

(defun elcp-lcp-error-printer (data msg error-type)
       (if (not elcp-spake)
	 (init-local-displays)
	 (setq elcp-spake t))
       (save-excursion-buffer
         (go-to-or-create-buffer 'Compiler/ Diagnostics)
         (go-to-end-of-buffer)
         (without-modifying
	 (if (not (at-beginning-of-buffer))
	     (new-line))
	 (set-the-mark)
	 (if (and (boundp elcp-@being-compiled)
		(symeval elcp-@being-compiled))
	     (if (not (at-beginning-of-buffer))
	         (new-line)
	         (set-the-mark))
	     (insert-string "*** DIAGNOSTICS FOR   ")
	     (insert-string (maknam (explodec (symeval elcp-@being-compiled))))
	     (insert-string " ***")
	     (new-line)
	     (set elcp-@being-compiled nil)
	     (elcp-filled-print-region)
	     (new-line)
	     (set-the-mark))
	 (setq error-type
	       (let ((obarray (get 'obarray 'array)))
		  (intern error-type)))
	 (insert-string (cdr (assq error-type
			       '((warn . "Warning: ")
			         (nonfatal . "Error: ")
			         (data . "Severe error: ")
			         (barf . "Compiler error: ")
			         (nil . "lisp_compiler: ")))))
	 (if data
	     (insert-string " ")
	     (insert-string
	       (let ((prinlevel 3)(prinlength 6))
		  (maknam (explode data))))
	     (new-line))
	 (insert-string " ")
	 (insert-string msg)
	 (new-line)
	 (elcp-filled-print-region))))


(defun elcp-filled-print-region ()
       (without-saving (runoff-fill-region))
       (with-mark x
	        (go-to-mark der-wahrer-mark)
	        (do-forever
		(local-display-current-line)
		(if (mark-on-current-line-p x)(stop-doing))
		(next-line)))
       (go-to-end-of-buffer))

