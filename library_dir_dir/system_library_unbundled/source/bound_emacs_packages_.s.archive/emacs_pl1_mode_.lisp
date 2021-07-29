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
;;;	Multics EMACS PL/I mode
;;;	Bernie Greenberg and statement_type_, Memorial Day Weekend, '78.
;;;
;;;	Change History
;;;
;;;	1) Paul Schauble
;;;	   03/05/79  Added hooks for compile-buffer and error scan mode.
;;;	   03/07/79  Added comment mode for pl1.
;;;
;;; 	2) Gary Dixon
;;;  	   04/02/79  Added improved pl1dcl (ESC-^D)
;;;	   05/07/80  Merge GDixon's changes with installed pl1-mode
;;;	   05/13/80  Added pl1-comment-current-line and comment style
;;;	3) Richard Mark Soley
;;;	   24 November 1981 Fixed pl1dcl to not use backward-word.
;;;	4) Barry Margolin
;;;	   3 December 1983 Fixed ^ZI to give a reasonable error if
;;;		         fpathname is null, and to leave the cursor
;;;		         in a more reasonable place.
;;;	   19 January 1984 Fixed pl1-error-list-builder to recognize
;;;		         FATAL ERROR, not misuse pl1-indentation,
;;;		         and move register-option forms to e_option_defaults_.
;;;

(%include e-macros)

(declare
  (*expr delete-blank-lines delete-line-indentation delete-word
         exit-error-scan-mode filloff fillon get-key-binding
         mark-at-current-point-p open-space runoff-fill-region
         set-fill-column unwind-sexp-searchers-marks-and-nlgoto
         backward-n-chars))

(declare (special 
	 pl1-interesting-keywords 
	 good-word-charactertbl pl1-wordscantable
	 pl1-com-quote-lab-charactertbl
	 comment-prefix comment-prefix-trim		; comment data
	 comment-suffix
	 pl1-box-start pl1-mid-box
	 fill-prefix fill-column			; fill-mode data
	 fill-mode-delimiters pl1-key-bindings-pre-fill
	 error-list-builder error-list e-list		; error scan data
	 mode-identification
	 macro-execution-in-progress			; prevent redisplay for printing tty's
	 buffer-minor-modes buffer-uid		; buffer stuff
	 current-buffer-mode
	 compiler compile-options			; compile-buffer 
	 pl1-compile-options pl1-inding-style 		; pl1-mode options
	 pl1-dcl-style pl1-dcl-column pl1-line-length
	 pl1-comment-column pl1-comment-column-delta
	 pl1-comment-style))

(declare (defpl1 entry_point_dcl_ "get_entry_point_dcl_$emacs"
	       (char (*)) (fixed bin) (fixed bin)	; pl1dcl support 
	       (return char(2000.) varying) (return char(32.) varying)
	       (return char (100.) varying)))

(declare (defpl1 cv_dec_check_ "" (char(*)) (return fixed bin(35.))
	       (return fixed bin(35.))))

(setq pl1-wordscantable (charscan-table
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_0123456789"))

(setq pl1-com-quote-lab-charactertbl (charscan-table """/:"))

(setq pl1-interesting-keywords
      '(proc end begin do procedure if else on dcl declare))

(defvar ((pl1-mode-hook nil)
         (COLON '/:)
         (OPEN-PAREN '/( )
         (CLOSE-PAREN '/) )
         (SEMI '/; )
         (SLASH '// )
         (FF (ascii 14))
         (TAB (ascii 11))
         (pl1-indentation 5)
         (pl1-first-column 10.)))
;;;
;;;
;;;	PL/I MODE
;;;

;;; (register-option 'pl1-indentation 5) ;moved to e_option_defaults_
;;; (register-option 'pl1-first-column 10.) ;moved to e_option_defaults_
;;; (register-option 'pl1-compile-options "-table") ;moved to e_option_defaults_
;;; (register-option 'pl1-inding-style 1) ;moved to e_option_defaults_
;;; (register-option 'pl1-dcl-style 1) ;moved to e_option_defaults_
;;; (register-option 'pl1-dcl-column 41.) ;moved to e_option_defaults_
;;; (register-option 'pl1-line-length 112.) ;moved to e_option_defaults_
;;; (register-option 'pl1-comment-style 1) ;moved to e_option_defaults_
;;; (register-option 'pl1-comment-column 61.) ;moved to e_option_defaults_
;;; (register-option 'pl1-comment-column-delta 10.) ;moved to e_option_defaults_


(defcom pl1-mode
        &numeric-argument (&reject)
       (register-local-var 'compiler)		; per-buffer compiler for a
					;   possible CDS mode, etc.
       (establish-local-var 'compile-options pl1-compile-options)
       (register-local-var 'pl1-line-length)	; per-buffer maximum line len
       (register-local-var 'pl1-inding-style)	; per-buffer indenting style
	; =1: do;  /* like the indent command */
	;          .....
	;     end;
	;
	; =2: do;
	;          .....
	;          end;
	;
       (register-local-var 'pl1-dcl-style)	; per-buffer declare style
	; =0: dcl x entry (...);
	;       /* no line breaking, 1 space between tokens.
	; 
	; =1: dcl  x entry (...,
	;               ....,....);
	;       /* like indent command, assumes dcl in column 0, followed
	;          by 2 spaces and variable name.  Lines are folded into
	;	 column 10 when longer than pl1-line-length. */
	;
	; =2:      dcl  x		entry (....,
	;			     ....,....);
	;       /* dcl begins in column 5, variable name in column 10,
	;	 attributes in column 40 (to make variable names easier
	;	 to find), with lines folding into column 45 when 
       	;	 longer than pl1-line-length. See also ESC-SP. */
	;
       (register-local-var 'pl1-dcl-column)	; per-buffer column in which
					;   attributes start when in
					;   pl1-dcl-style 2
       (register-local-var 'pl1-comment-style)
	; per-buffer comment control for comments on lines whose text extends
	; beyond comment column.  Values are:
	; 
	; =1:   comment placed after text on the current line
	;
	; =2:   if text extends beyond 
	;         (pl1-comment-column + pl1-comment-column-delta)
	;       then comment placed on a new line below current line.  Else
	;       comment placed after text on the current line.
	; 
	; =3:   comment placed on new line following text
	;
       (register-local-var 'pl1-comment-column)	; per-buffer comment column
       (register-local-var 'pl1-comment-column-delta)
					; per-buffer comment delta
       (register-local-var 'good-word-charactertbl)
					; per-buffer PL/I word tbl
       (register-local-var 'mode-identification)	; per-buffer error scan mode
       (register-local-var 'error-list-builder)	; per-buffer name of error
					;   scan routine. 
       (register-local-var 'comment-prefix)	; per-buffer comment start
       (register-local-var 'comment-prefix-trim)
       (register-local-var 'comment-suffix)	; per-buffer comment end
       (register-local-var 'pl1-key-bindings-pre-fill)
					; per-buffer saved key bind.
       (register-local-var 'pl1-box-start)	; per-buffer comment box mark
       (register-local-var 'pl1-mid-box)	; mid comment box insert flag
       (setq current-buffer-mode 'PL//I		; PL/I mode
	   comment-prefix "/* "		; PL/I comment delimiters
	   comment-prefix-trim "/*"
	   comment-suffix "*/"
	   good-word-charactertbl pl1-wordscantable
					; Use PL/I words for word 
					;   scanning
	   compiler "pl1"			; Use pl1 compiler
	   compile-options pl1-compile-options
	   mode-identification -2		; Setup for error scan mode
	   error-list-builder 'pl1-error-list-builder)

       (if (boundp 'error-list)	; end error scan mode if needed.
	 (if error-list (exit-error-scan-mode))
	 else
	 (setq error-list nil e-list nil))

       (negate-minor-mode 'electric)
       (mapc '(lambda (x)			; Establish key bindings
		  (set-key (car x) (cadr x)))
	   '((TAB 	indent-pl1-statement)
	     (/:		self-insert)	;electro negate
	     (/;		self-insert)
	     (ESC-N	pl1-comment-next-line)
	     (ESC-P	pl1-comment-prev-line)
	     (ESC-Q  	undefined-command)	;   Avoid paragraph-fill
	     (ESC-^A	pl1-backward-statement)
	     (ESC-^C	compile-buffer)
	     (ESC-^D	pl1dcl)
	     (ESC-^E	pl1-forward-statement)
	     (ESC-^H	roll-back-pl1-indentation)
;;;	     (ESC-^Q	pl1-reindent-region)	; This is not ready
	     (ESC-TAB	pl1-tab-one-more-level)
	     (ESC-CR	pl1-cret-and-indent)
	     (ESC-/;	pl1-comment-current-line)
	     (ESC-*	pl1-comment-end)
	     (ESC-SPACE	pl1-skip-to-dcl-column)
	     (^X^D	locate-next-error)
	     (^XT		exit-error-scan-mode)
	     (^XC		pl1-comment-box)
	     (^ZC		pl1-refill-comment-box-region)
	     (^ZD		pl1-line-between-procs)
	     (^ZI		pl1-include-file-comment-start-end)))
       (if pl1-mode-hook
	 (errset (funcall pl1-mode-hook))))

;;;
;;;	ELECTRIC PL/I MODE
;;;

(defcom electric-pl1-mode
        &numeric-argument (&reject)
       (pl1-mode)
       (electric-mode))

(defprop elpl1 electric-mode expr)

(defcom electric-mode
        &numeric-argument (&reject)
       (set-key '/; 'electric-pl1-semicolon)
       (set-key '/: 'electric-pl1-colon)
       (assert-minor-mode 'electric))

;;;
;;;	The following set of functions do PL/I lexical analysis.
;;;

(defun get-pl1-token-forwards ()
       (prog ()
lupo
	   (skip-over-whitespace)
	   (if (at-end-of-buffer)(return nil))
	   (let ((rh (curchar)))
	        (if (looking-at comment-prefix-trim)
		  (skip-forwards-pl1-comment)
		  (go lupo))
	        (if (memq rh '(/- /+ /* // /, /. /; /: /& /| /^ /= /< /> /( /) /%))
		  (forward-char)
		  (return rh))
	        (if-at "/" (forward-char)(return rh))
	        (if-at """" (return (get-pl1-quoted-string-forward)))
	        (return (with-mark start-token
			       (forward-word)
			       (point-mark-to-string start-token))))))


(defun skip-forwards-pl1-comment ()
       (do-times (stringlength comment-prefix-trim) (forward-char))
       (if (not (forward-search comment-suffix))
	 (display-error-noabort "Unbalanced Comment")
	 (unwind-sexp-searchers-marks-and-nlgoto)))


(defun get-pl1-quoted-string-forward ()
       (with-mark
         bgqs
         (prog ()
loop 	     (forward-char)
	     (if (forward-search """")
	         else (display-error-noabort "Unbalanced PL/I quotes")
	         (go-to-mark bgqs)
	         (release-mark bgqs)
	         (unwind-sexp-searchers-marks-and-nlgoto))
	     (if-at """" (go loop))
	     (return (point-mark-to-string bgqs)))))


(defun skip-pl1-whitespace ()
       (do-forever
         (skip-over-whitespace)
         (if (looking-at comment-prefix-trim)
	   (skip-forwards-pl1-comment)
	   else (return nil))))


(defun get-pl1-token-backwards ()
       (prog ()
lupo
	   (skip-back-whitespace)
	   (let ((lh (lefthand-char)))
	        (if (at-beginning-of-buffer) (return nil))
	        (if (looking-back-at comment-suffix)
		  (skip-backwards-pl1-comment)
		  (go lupo))
	        (if (memq lh '(/- /+ /* // /, /. /; /: /& /| /^ /= /< /> /( /) /%))
		  (backward-char)
		  (return lh))
	        (if (eq lh  '/")
		  (return (get-pl1-quoted-string-backwards)))
	        (if-back-at '//
			(backward-char)
			(return lh))
	        (return (with-mark
		        endtoken
		        (backward-word)
		        (point-mark-to-string endtoken))))))


(defun get-pl1-quoted-string-backwards ()
       (with-mark endqs
	        (prog ()
loop 		    (backward-char)
		    (if (reverse-search """")
		        else
		        (display-error-noabort "Unbalanced PL/I quotes")
		        (go-to-mark endqs)
		        (release-mark endqs)
		        (unwind-sexp-searchers-marks-and-nlgoto))
		    (if-back-at '/" (go loop))
		    (return (point-mark-to-string endqs)))))


(defun skip-backwards-pl1-comment ()
       (do-times (stringlength comment-suffix) (backward-char))
       (if (not (reverse-search comment-prefix-trim))
	 (display-error-noabort "Unbalanced comment")
	 (unwind-sexp-searchers-marks-and-nlgoto)))


(defun get-pl1-statement-backwards ()
       (let ((lt (get-pl1-token-backwards)))
	  (and lt
	       (do ((a-building (ncons lt) (cons curtoken a-building))
		  (curtoken))
		 (nil)
		 (setq curtoken (get-pl1-token-backwards))
		 (if (eq curtoken nil)
		     (return (cons lt a-building)))
		 (if (eq curtoken SEMI)
		     (forward-char)
		     (return (cons lt a-building)))))))


(defun pl1-find-start-prev-sta ()
       (save-excursion
         (prog (prev-sta incomplete-flag)
chomp-backwards-some-more
	     (setq prev-sta (get-pl1-statement-backwards))
	     (or prev-sta (return 'first-statement))
	     (if (eq (car prev-sta) COLON)	;guy just typed label
	         (go chomp-backwards-some-more))
	     (if (not (eq (car prev-sta) SEMI))
	         (setq incomplete-flag t))
	     (setq prev-sta (cdr prev-sta))	;real stuff
	     (setq  prev-sta (pl1-skip-over-labels prev-sta t))
	     (skip-pl1-whitespace)
	     (return (list (set-mark)(cur-hpos) prev-sta incomplete-flag)))))

;;;

;;;
;;;	This set of functions parse PL/I statements, ALL PL/I statements.
;;;

(defun pl1-skip-over-labels (sta parsit)
       (prog (close-ptr)
rescan
	   (if (eq (cadr sta) COLON)
	       (if  parsit
		  (pl1-parse-chk (car sta))
		  (pl1-parse-chk COLON))
	       (setq sta (cddr sta))
	       (go rescan))
	   (if (and (stringp (car sta))	; could be label array!
		  (eq (cadr sta) OPEN-PAREN)
		  (stringp (caddr sta))
		  (pl1-string-fixnump (caddr sta))
		  (eq (cadddr sta) CLOSE-PAREN)
		  (eq (car (cddddr sta)) COLON))   ;got one
	       (if parsit
		 (pl1-parse-chk (car sta))	;foo
		 (pl1-parse-chk OPEN-PAREN)	; (
		 (pl1-parse-chk (caddr sta))	; 13
		 (pl1-parse-chk CLOSE-PAREN)	;)
		 (pl1-parse-chk COLON))
	       (setq sta (cdr (cddddr sta)))
	       (go rescan))
	   (if (and (eq (car sta) OPEN-PAREN)	;c/b a condition prefix
		  (setq close-ptr (memq CLOSE-PAREN (cdr sta)))
		  (eq (cadr close-ptr) COLON)
		  (progn
		    (do x sta (cdr x) (eq x (cddr close-ptr))
		        (and parsit (pl1-parse-chk (car sta)))
		        (setq sta (cdr sta)))
		    (go rescan))))
	   (return sta)))


(defun pl1-string-fixnump (x)
       (and (stringp x)
	  (let ((ch1 (getcharn x 1)))
	       (and (< ch1 (1+ (CtoI "9")))
		  (> ch1 (1- (CtoI "0")))))))

(defun pl1-declare-p (prev-sta)
       (and (not (atom prev-sta))
	  (memq (cadr (pl1-typify-statement (caddr prev-sta) nil))
	        '(dcl declare /%))))


(defun pl1-typify-statement (sta parsit)
       (prog (key)
	   (setq key (car sta))
	   (if (eq key SEMI)(return (list nil 'null)))
	   (if (not (stringp key))(return (list nil 'random)))
	   (setq key (intern (make_atom key)))
	   (if (not (memq key pl1-interesting-keywords))
	       (return (list sta 'random)))
	   (if (eq (cadr sta) SEMI)
	       (if parsit (pl1-parse-chk (car sta)))
	       (return (list (cdr sta) key)))
	   (if (eq key 'if)(return (pl1-typify-if-hacker sta parsit)))
	   (if (and (symbolp (cadr sta))
		  (not (eq (cadr sta) OPEN-PAREN)))
	       (return (list sta 'random)))
	   (if (eq key 'begin)
	       (if (stringp (cadr sta))
		 (return (list sta key))
		 else
		 (return (list sta 'random))))
	   (if (eq key 'on)(return (pl1-typify-on-hacker sta parsit)))
	   (if (eq key 'do)(return (pl1-typify-do-hacker sta)))
	   (if (eq key 'else)
	       (if parsit (pl1-parse-chk "else"))
	       (return (list (cdr sta) 'else)))
	   (if (pl1-typify-0lev-parencheck sta)(return (list sta 'random)))
	   (return (list (cdr sta) key))))))


(defun pl1-typify-0lev-parencheck (sta)
       (do ((parnct 0)
	  (x sta (cdr x)))
	 ((or (null x)(eq (car x) SEMI)) nil)
	 (cond ((eq (car x) OPEN-PAREN)
	        (setq parnct (1+ parnct)))
	       ((eq (car x) CLOSE-PAREN)
	        (setq parnct (1- parnct)))
	       ((not (= parnct 0)))
	       ((eq (car x) '/=)(return t)))))


(defun pl1-typify-do-hacker (sta)
       (cond ((stringp (cadr sta))(list sta 'do))
	   ((eq (cadr sta) SEMI)(list sta 'do)) ;redundant
	   (t (list sta 'random))))


(defun pl1-typify-if-hacker (sta parsit)
       (prog ()
	   (and (symbolp (cadr sta))
	        (not (eq (cadr sta) OPEN-PAREN))
	        (not (memq (cadr sta) '(/- /+ /^)))
	        (return (list sta 'random)))
	   (and (eq (cadr sta) '/-)
	        (eq (caddr sta) '/>)
	        (return (list sta 'random)))
	   (return
	     (do ((parnct 0)
		(prev '/=)
		(tsta sta (cdr tsta)))
	         ((or (null tsta)(eq (car tsta) SEMI))
		(list sta 'random))
	         (cond ((eq (car tsta) OPEN-PAREN)
		      (setq parnct (1+ parnct)))
		     ((eq (car tsta) CLOSE-PAREN)
		      (setq parnct (1- parnct)))
		     ((not (= parnct 0)))
		     ((not (stringp (car tsta))))
		     ((not (samepnamep (car tsta) "then")))
		     ((or (stringp prev)(eq prev CLOSE-PAREN)(eq prev '/.))
		      (return (do ((x sta (cdr x)))
			        ((eq x (cdr tsta))
			         (list x 'if))
			        (if parsit (pl1-parse-chk (car x)))))))
	         (setq prev (car tsta))))))


(defun pl1-typify-on-hacker (sta parsit)
       (prog ()
	   (cond ((stringp (cadr sta))
		(and parsit (pl1-parse-chk "on"))
		(and parsit (pl1-parse-chk (cadr sta)))
		(setq sta (cddr sta)))
	         (t (return (list sta 'random))))
	   (do-forever
	     (if (and (eq (car sta) OPEN-PAREN)
		    (stringp (cadr sta))
		    (eq (caddr sta) CLOSE-PAREN))
	         (if (eq (cadddr sta) COLON)(stop-doing)
		   else (if parsit (pl1-parse-chk OPEN-PAREN)
			  (pl1-parse-chk (cadr sta))
			  (pl1-parse-chk CLOSE-PAREN))
		   (setq sta (cdddr sta))))	       
	     (if (and (stringp (cadr sta))
		    (eq (car sta) '/,))
	         (if parsit (pl1-parse-chk (car sta))
		   (pl1-parse-chk (cadr sta)))
	         (setq sta (cddr sta))
	         else (stop-doing)))
	   (and (eq (cadr sta) SEMI)
	        (stringp (car sta))
	        (samepnamep (car sta) "system")
	        (progn (and parsit (pl1-parse-chk "system"))
		     (setq sta (cdr sta))))
	   (if (and (stringp (car sta))
		  (samepnamep (car sta) "snap")
		  (pl1-typify-ridiculous-snap-screw sta))
	       (if parsit (pl1-parse-chk "snap"))
	       (setq sta (cdr sta)))
	   (return (list sta 'on))))


(defun pl1-typify-ridiculous-snap-screw (sta)	;have snap x x x ...
       (cond ((eq (cadr sta) SEMI) t)		;for sure
	   ((null (cdr sta)) t)		;why not
	   ((stringp (cadr sta)) t)		;snap begin; etc.
	   ((not (eq (cadr sta) OPEN-PAREN)) nil)    ;no chance, snap =, snap -> etc.
	   ;;at this point snap (13): is problem, as is snap (fixedov).. so...
	   ((not (eq (pl1-skip-over-labels sta nil) sta)) nil) ;label array
	   ((eq (pl1-skip-over-labels (cdr sta) nil) (cdr sta)) nil)     ;assgt sta
	   (t t)))			;real snappo


(defun pl1-parse-chk (lexeme)
       (let ((parsed (get-pl1-token-forwards)))
	  (cond ((symbolp parsed)
	         (or (eq parsed lexeme)
		   (error "pl1-parse-chk: out of sync 1")))
	        ((not (stringp lexeme))
	         (error "pl1-parse-chk: out of sync 2"))
	        ((not (samepnamep parsed lexeme))
	         (error "pl1-parse-chk: out of sync 3")))))

;;;

;;;
;;;	INDENTATION  (written by Greenberg)
;;;

(defun compute-pl1-indentation ()
       (prog (prevhpos prev-sta incomp-flag)
	   (setq prev-sta (pl1-find-start-prev-sta))
	   (if (pl1-declare-p prev-sta)
	       (save-excursion
	         (do-forever
		 (go-to-mark (car prev-sta))
		 (release-mark (car prev-sta))
		 (setq prev-sta (pl1-find-start-prev-sta))
		 (if (not (pl1-declare-p prev-sta))
		     (return t)))))
	   (if (eq prev-sta 'first-statement)
	       (return pl1-first-column))
	   (release-mark (car prev-sta))
	   (setq prevhpos (cadr prev-sta)
	         incomp-flag (cadddr prev-sta) prev-sta (caddr prev-sta))
	   (if incomp-flag (return (+ pl1-indentation prevhpos)))
	   (do ((levels 0)
	        (s (pl1-typify-statement prev-sta nil)
		 (pl1-typify-statement (pl1-skip-over-labels (car s) nil) nil)))
	       (nil)
	       (cond ((memq (cadr s) '(if else on))(setq levels (1+ levels)))
		   ((memq (cadr s) '(do begin ))   ;no proc for now
		    (setq prevhpos (+ prevhpos (* pl1-indentation (max levels 1))))
		    (return t))
		   ((and (eq (cadr s) 'end)(= pl1-inding-style 2))
		    (setq prevhpos (- prevhpos pl1-indentation))
		    (return t))
		   (t (return nil))))
	   (return prevhpos)))


(defcom  pl1-cret-and-indent
         &numeric-argument (&reject)
        (delete-white-sides)
        (new-line)
        (indent-pl1-statement))

(defcom indent-pl1-statement
        &numeric-argument (&reject)
       (delete-white-sides)
       (whitespace-to-hpos (compute-pl1-indentation)))

(defcom roll-back-pl1-indentation
        &numeric-argument (&repeat)
       (let ((hp (cur-hpos)))
	  (delete-white-sides)
	  (whitespace-to-hpos (- hp pl1-indentation))))

(defcom pl1-tab-one-more-level
        &numeric-argument (&repeat)
       (let ((cur-hpos (cur-hpos)))
	  (delete-white-sides)
	  (whitespace-to-hpos (+ pl1-indentation cur-hpos))))
;;;
;;;
;;;	Reindentation of a Region (written by Dixon in May 80)
;;;

(defcom pl1-reindent-region
        &numeric-argument (&reject)
        (if (point>markp der-wahrer-mark)
	  (exchange-point-and-mark))
        (prog (token)
	    (if numarg
	        else
	        (do-forever
		(if (line-is-blank)
		    (delete-white-sides)
		    else
		    (go-to-beginning-of-line)
		    (skip-over-whitespace-in-line)
		    (if (looking-at comment-prefix-trim)
		        (pl1-comment-current-line)
		        else
		        (if (bolp)
			  (with-mark bol
				   (pl1-forward-statement)
				   (do-forever
				     (setq token (get-pl1-token-backwards))
				     (if (and (eq token COLON)
					    (save-excursion
					      (get-pl1-token-backwards)
					      (pl1-legitimate-label-context)))
				         (delete-char)
				         (electric-pl1-colon)
				         (stop-doing))
				     (if (mark-at-current-point-p bol)
				         (pl1-rindent-pl1-statement))))
			  else
			  (pl1-rindent-pl1-statement))
		        (if (pl1-comment-in-line)
			  (pl1-comment-current-line))))
		(if (mark-on-current-line-p der-wahrer-mark)
		    (go-to-beginning-of-line)
		    (stop-doing))
		(next-line)))))

(defun pl1-rindent-pl1-statement ()
       (indent-pl1-statement)
       (if (and (= pl1-inding-style 1)(forward-search-in-line "end"))
	 ;;do type 2 tomorrow
	 (save-excursion
	   (pl1-forward-statement)
	   (pl1-adjust-for-this-maybe-being-an-end-statement))))

	 
(defun pl1-comment-in-line ()
       (prog ()
loop
	   (if (search-for-first-charset-line pl1-com-quote-lab-charactertbl)
	       (if-at COLON (forward-char) (go loop))
	       (if-at """"
		    (with-mark ind-line
			     (get-pl1-quoted-string-forward)
			     (if (mark-on-current-line-p ind-line)
			         (go-to-mark ind-line)
			         (return nil)))
		    (go loop))
	       (if (looking-at comment-prefix-trim)
		 (return t))
	       (go loop))
	   (return nil)))

;;;
;;;	Indentation of Attributes in dcl Statement
;;;	  (written by Dixon in May 79 as part of pl1dcl rewrite)

(defcom pl1-skip-to-dcl-column
        &numeric-argument (&reject)
       (delete-white-sides)
       (if (> (cur-hpos) (- pl1-dcl-column 2)) (new-line))
       (whitespace-to-hpos (1- pl1-dcl-column)))

(defprop pl1-skip-to-dcl-attributes
"Skips from name of variable in a dcl line to pl1-dcl-column.
This is especially useful when pl1-dcl-style option is set to 2.
The pl1-dcl-column and pl1-dcl-style can be set using
$$extended-command$ opt." documentation)


;;;

;;;
;;;	Last are the user-visible Emacs commands for PL/I mode.
;;;



;;;
;;; 	COMMENT INSERTION
;;;


;;;	  Comment Box
;;;	    (written by Schauble in March 79)
;;;	    (rewritten by Dixon in May 79 to fix bugs)
;;;	    (enhanced by Dixon in May 80)

(defcom pl1-comment-box ()
        &numeric-argument (&reject)
       (prog (fill-to-col)
	   (if (memq 'comment buffer-minor-modes)
					; exit comment mode
	       (if (or (point>markp pl1-box-start)
		     (mark-at-current-point-p pl1-box-start))
		 (do-forever
		   (go-to-beginning-of-line)
		   (if (lastlinep)
		       (go-to-end-of-line)
		       (stop-doing)
		       else
		       (if (or (looking-at (catenate TAB comment-prefix))
			     (looking-at (catenate TAB comment-prefix-trim TAB)))
			 (if pl1-mid-box
			     (go-to-end-of-line)
			     (if (looking-back-at comment-suffix)
			         (prev-line)
			         (go-to-end-of-line)
			         (stop-doing))
			     (go-to-beginning-of-line))
			 (next-line)
			 else
			 (prev-line)
			 (go-to-end-of-line)
			 (stop-doing))))
		 (filloff)
		 (setq fill-prefix "")
		 (pl1-restore-key-bindings-post-fill)
		 (setq fill-to-col (- pl1-line-length 10.
				  (stringlength comment-suffix)))
		 (if pl1-mid-box
		     (next-line)
		     else
		     (pl1-conditional-new-line (catenate TAB comment-prefix))
		     (new-line)		; cursor now on close line
		     (insert-string (catenate TAB comment-prefix))
		     (do-times (// (- pl1-line-length 20.
				  (stringlength comment-prefix)
				  (stringlength comment-suffix)) 3)
			     (insert-string " * "))
		     (whitespace-to-hpos fill-to-col)
		     (insert-string comment-suffix)
		     (new-line)
		     (prev-line))
		 (with-mark box-end
			  (do-forever
			    (prev-line)
			    (go-to-end-of-line)
			    (delete-white-sides)
			    (whitespace-to-hpos fill-to-col)
			    (insert-string comment-suffix)
			    (if (mark-on-current-line-p pl1-box-start)
			        (stop-doing)))
			  (go-to-mark box-end))
		 else		;; cursor is before start of box
		 (go-to-mark pl1-box-start)
		 (display-error "Cursor not at end of comment box"
			      (if pl1-mid-box " insert lines")))
	       (release-mark pl1-box-start)	; clear up the mode
	       (negate-minor-mode 'comment)
	       (if pl1-mid-box
		 else
		 (next-line)
		 (go-to-end-of-line)
		 (pl1-cret-and-indent))
;;;
	       else			;   start of comment box
	       (go-to-beginning-of-line)
	       (if (or (looking-at (catenate TAB comment-prefix))
		     (looking-at (catenate TAB comment-prefix-trim TAB)))
		 (prev-line)		; check for inserting line in middle
					;  of an existing comment box
		 (if (or (looking-at (catenate TAB comment-prefix))
		         (looking-at (catenate TAB comment-prefix-trim TAB)))
		     (setq pl1-mid-box t)
		     else
		     (prev-line)
		     (setq pl1-mid-box nil))
		 else
		 (setq pl1-mid-box nil))
	       (if pl1-mid-box
		 (go-to-end-of-line)
		 (pl1-save-key-bindings-pre-fill)
		 (fillon)
		 (set-fill-column (- pl1-line-length 10.
				 (1+ (stringlength comment-suffix))))
		 (setq fill-prefix (catenate TAB comment-prefix))
		 (new-line)
		 (setq pl1-box-start (set-mark))
		 else
		 (pl1-conditional-new-line "")
		 (new-line)
		 (insert-string  (catenate TAB comment-prefix))
		 (do-times (// (- pl1-line-length 20.
			        (stringlength comment-prefix)
			        (stringlength comment-suffix)) 3)
			 (insert-string " * "))
		 (pl1-save-key-bindings-pre-fill)
		 (fillon)
		 (set-fill-column (- pl1-line-length 10.
				 (1+ (stringlength comment-suffix))))
		 (whitespace-to-hpos
		   (- pl1-line-length 10.
		      (stringlength comment-suffix)))
		 (insert-string comment-suffix) 
		 (setq fill-prefix (catenate TAB comment-prefix))
		 (new-line)
		 (setq pl1-box-start (set-mark))
		 (new-line))
	       (assert-minor-mode 'comment))))

(defprop pl1-comment-box
"Generates a comment box containing text describing the program.  When $$$ is
first given, comment minor mode is entered.  The first two lines of the box
are created and the cursor is positioned at the end of the third line, ready
to begin typing the text.  

Fill mode is enabled, to facilitate typing of textual input (type
""$$extended-command$ describe fillon"" for information on fill mode), but can
be disabled during comment mode by typing:
   $$extended-command$ filloff

After all text is typed in, use $$$ again to turn of comment mode and return
to normal PL/I editing.

To add text to an existing comment box, position the cursor to the line above
which the new text is to be inserted, then type $$$." documentation)



(defun pl1-conditional-new-line (prefix)
       (go-to-beginning-of-line)
       (if (or (line-is-blank)
	     (and (looking-at prefix)
		(= curlinel (1+ (stringlength  prefix)))))
	 (without-saving (kill-to-end-of-line))
	 else
	 (go-to-end-of-line)
	 (let ((fill-prefix "")) (new-line)))
       (if (eq prefix "")
	 else (insert-string prefix)))

(defun pl1-save-key-bindings-pre-fill ()
       (setq pl1-key-bindings-pre-fill
	   (mapcar '(lambda (x)
			(get-key-binding (list 0 (getcharn x 1) nil)))
		 fill-mode-delimiters)))

(defun pl1-restore-key-bindings-post-fill ()
       (mapc 'set-key fill-mode-delimiters pl1-key-bindings-pre-fill))

(defun looking-back-at (string)
       ((lambda (linel sl)
	      (cond ((> sl linel) nil)
		  ((= sl 0) t)
		  (t (prog2 (do-times sl (backward-char))
			   (looking-at string)
			   (do-times sl (forward-char))))))
        curlinel (stringlength string)))
;;;
(defcom pl1-refill-comment-box-region
        &numeric-argument (&reject)
       (if (point>markp der-wahrer-mark)
	 (exchange-point-and-mark))
       (go-to-beginning-of-line)		; region begins at start of
       (setq pl1-box-start (set-mark))		;   line containing the point
       (go-to-mark der-wahrer-mark)
       (go-to-end-of-line)
       (set-the-mark)			; region ends at end of line
       (go-to-mark pl1-box-start)		;   containing the-mark
       (do-forever				; make sure entire region is
         (go-to-beginning-of-line)		;   part of comment box
         (if (or (looking-at (catenate TAB comment-prefix))
	       (looking-at (catenate TAB comment-prefix-trim TAB)))
	   else
	   (display-error "Region lies (totally or partially) outside comment box."))
         (go-to-end-of-line)
         (delete-white-sides)
         (if (looking-back-at comment-suffix)
	   else
	   (display-error "Region lies (totally or partially) outside comment box."))
         (if (mark-on-current-line-p der-wahrer-mark)
	   (stop-doing))
         (next-line))
       (go-to-mark pl1-box-start)		; save the region in case
       (copy-region)			;   filling is disastrous 
					; remove comment prefix,  
					;   comment suffix, &
       (do-forever				;   trailing whitespace from
         (go-to-beginning-of-line)		;   each line of region
         (do-times (1+ (stringlength comment-prefix))
	         (delete-char))
         (go-to-end-of-line)
         (do-times (stringlength comment-suffix) (rubout-char))
         (delete-white-sides)			; Remove suffix & trail
         (if (mark-on-current-line-p der-wahrer-mark)
	   (stop-doing))
         (next-line))
       (set-fill-column (- pl1-line-length
		       10. (stringlength comment-prefix)
		       10. (1+ (stringlength comment-suffix))))
       (setq fill-prefix "")
       (go-to-mark pl1-box-start)		; refill the region
       (exchange-point-and-mark)
       (without-saving (runoff-fill-region))
       (exchange-point-and-mark)		; put back comment prefix
       (do-forever				;   & suffix on each line
         (go-to-beginning-of-line)
         (insert-string (catenate TAB comment-prefix))
         (go-to-end-of-line)
         (delete-white-sides)
         (whitespace-to-hpos (- pl1-line-length 10.
			  (stringlength comment-suffix)))
         (insert-string comment-suffix)
         (if (mark-on-current-line-p der-wahrer-mark)
	   (stop-doing))
         (next-line))
       (go-to-end-of-line)
       (set-the-mark)
       (go-to-mark pl1-box-start)		; Mark text just filled
       (exchange-point-and-mark)
       (release-mark pl1-box-start))

(defprop pl1-refill-comment-box-region
"Refills text inside a comment block between the line containing the-mark and
the line containing the cursor.  Unfilled text is saved on the kill-ring in
case filling produces unexpected results." documentation)

;;;
;;;
;;;	Beginning of Comment (written May 80 by Dixon)
;;;

(defcom pl1-comment-current-line
        &numeric-argument (&reject)
       (go-to-beginning-of-line)
       (if (forward-search-in-line comment-prefix)
	 (do-times (stringlength comment-prefix) (backward-char))
	 (if (and (> pl1-comment-style 1)
		(= (cur-hpos) (1- pl1-comment-column)))
 	     (save-excursion		; for comment on a line by
	       (skip-back-whitespace-in-line)	; itself, see if it should be
 	       (if (bolp)			; put on previous line.  It 
		 (prev-line)		; may have been split from
 		 (if (line-is-blank)	; prev line because line had
		     else			; text in comment column 
					; which may no longer be 
					; there.
 		     (if (or (forward-search-in-line comment-prefix)
			   (forward-search-in-line comment-prefix-trim))
		         else
 		         (go-to-end-of-line)
		         (delete-white-sides)
		         (next-line)
		         (delete-line-indentation))))))
 	 (if (= (cur-hpos) (1- pl1-comment-column))	 
	     else				; comment not positioned ok
	     (delete-white-sides)
	     (if (> (cur-hpos) (1- pl1-comment-column))
					; comment starts beyond 
	         (if (= pl1-comment-style 1)	;   pl1-comment-column
		   (do ((column (1- pl1-comment-column) (+ column pl1-indentation)))
		       ((> column (cur-hpos))
		        (whitespace-to-hpos column)))
		   else
		   (if (= pl1-comment-style 2)
		       (if (< (+ (1- pl1-comment-column) pl1-comment-column-delta)
			    (cur-hpos))	; comment won't fit within
			 (new-line)	;   delta chars of
					;   pl1-comment-column
			 (whitespace-to-hpos (1- pl1-comment-column))
			 else		; comment will fit within
					;   delta chars
			 (do ((column (1- pl1-comment-column) (+ column pl1-indentation)))
			     ((> column (cur-hpos))
			      (whitespace-to-hpos column))))
		       else		; assume pl1-comment-style 3
		       (new-line)		;   put comment on a new line
		       (whitespace-to-hpos (1- pl1-comment-column))))
	         else			; cur-hpos<pl1-comment-column
	         (whitespace-to-hpos (1- pl1-comment-column))))
	 (do-times (stringlength comment-prefix) (forward-char))
	 else				; complete comment-prefix not
					;   found.  Look for prefix
					;   without trailing SPACE
	 (if (forward-search-in-line comment-prefix-trim)
	     (do-times (stringlength comment-prefix-trim)
		     (rubout-char))
	     (insert-string comment-prefix)	; convert nonstandard prefix
	     (pl1-comment-current-line)	;   to standard one
	     else				; no comment prefix on line
	     (go-to-end-of-line)		;   put one at end of line
	     (delete-white-sides)		;   then do stuff above
	     (insert-string comment-prefix)
	     (pl1-comment-current-line))))

(defcom pl1-comment-next-line
        &numeric-argument (&pass)
        &negative-function pl1-comment-prev-line
        (if numarg else (setq numarg 1))
        (do ((count 1 (1+ count)))
	  ((> count numarg))
	  (if (lastlinep)
	      (go-to-end-of-line)
	      (new-line)
	      (insert-string comment-prefix)
	      (setq count numarg)
	      else
	      (next-line)))
       (pl1-comment-current-line))

(defcom pl1-comment-prev-line
        &numeric-argument (&pass)
        &negative-function pl1-comment-next-line
        (if numarg else (setq numarg 1))
        (do ((count 1 (1+ count)))
	  ((> count numarg))
	  (if (firstlinep)
	      (go-to-beginning-of-line)
	      (open-space)
	      (insert-string comment-prefix)
	      (setq count numarg)
	      else
	      (prev-line)))
       (pl1-comment-current-line))

(defprop pl1-comment-current-line
"Searches for this line's comment.  If one is found, it is indented to the
comment column for this line.  If not found, a comment prefix is inserted at 
the comment column.

Use $$extended-command$ opt to set the following comment-related options:
pl1-comment-style, pl1-comment-column, and pl1-comment-column-delta.

pl1-comment-style controls commenting when PL/I statements on the current line
already extend beyond the pl1-comment-column.  It can have the following
values:

1 = begin the comment beyond the end of the statements.

2 = if PL/I statements extend beyond 

	pl1-comment-column + pl1-comment-column-delta

then put comment on a new line below the current line.  Otherwise, put
comment on current line beyond end of the statements.

3 = put comment on a new line below the current line." documentation)

(defprop pl1-comment-prev-line
"Searches for a comment on the previous line.  If one is found, it is indented
to the comment column for this line.  If not found, a comment prefix is 
inserted at the comment column.  Essentially the same as $$prev-line-command$
$$pl1-comment-current-line$.  See $$pl1-comment-current-line$."
documentation)

(defprop pl1-comment-next-line 
"Searches for a comment on the next line.  If one is found, it is indented to
the comment column for this line.  If not found, a comment prefix is inserted 
at the comment column.  Essentially the same as $$next-line-command$
$$pl1-comment-current-line$.  See $$pl1-comment-current-line$." documentation)


;;;
;;;	  End of Comment (written by Dixon in May 79)
;;;

(defcom pl1-comment-end
        &numeric-argument (&reject)
       (if (forward-search-in-line comment-suffix) ; comment already ended
	 (do-times (stringlength comment-suffix) (rubout-char))
	 (delete-white-sides)
	 else (go-to-end-of-line))
       (if (> (cur-hpos) (- pl1-line-length
		        (stringlength comment-suffix)))
	 (insert-string comment-suffix)
	 (display-error
	   (catenate "Comment ends beyond column "
		   (decimal-rep (- pl1-line-length
			         (stringlength comment-suffix)))))
	 else
	 (whitespace-to-hpos (- pl1-line-length
			    (stringlength comment-suffix))))
       (insert-string comment-suffix)
       (if (or (lastlinep)
	     (save-excursion (next-line)
			 (line-is-blank)))
	 (pl1-cret-and-indent)))

(defprop pl1-end-comment
"Skips to end of a comment line and inserts a comment end delimiter
(*/) in the column defined by the pl1-line-length option.  This option
can be set using $$extended-command$ opt." documentation)



;;;	Comment line between program sections (written by Dixon)

(defcom pl1-line-between-procs
        &numeric-argument (&pass)
       (go-to-end-of-line)
       (delete-white-sides)
       (if (line-is-blank) else (new-line))
       (delete-blank-lines)
       (if (looking-at (catenate comment-prefix " *  *  *  *  *"))
	 (pl1-insert-divider)		; Already a divider there.
	 (new-line)			;   Create a page separator
	 (insert-string FF)
	 (new-line)
        else
           (pl1-insert-divider)
	 (new-line)
	 (if numarg			; Force a page separator
	     (insert-string FF)
	     (new-line)
	     (pl1-insert-divider)
	     (new-line))
	 (new-line)
	 (new-line)))

(defun pl1-insert-divider ()			; Insert divider across page
       (insert-string comment-prefix)		;   /*  *  *  ...  *  *  */
       (do-times (// (- pl1-line-length
		    (stringlength comment-prefix)
		    (stringlength comment-suffix)) 3.)
	       (insert-string " * "))
       (whitespace-to-hpos (- pl1-line-length
			(stringlength comment-suffix)))
       (insert-string comment-suffix))

(defprop pl1-line-between-procs
"Inserts comment line which divides the page.  A numeric arg
(eg, ^U$$$) generates a dividing comment, a newpage char, and a
second dividing comment.  This comment divider is used
to separate procedures in PL/I source." documentation) 
;;;
(defcom pl1-include-file-comment-start-end
        &numeric-argument (&reject)
        (if (null fpathname)
	  (display-error "The buffer does not have a pathname."))
        (let ((path-list (namelist fpathname)))
	   (let ((rpath-list (reverse path-list))
	         (entry-name (cadr path-list)))
	        (mapc '(lambda (x) (setq entry-name (catenate entry-name "." x)))
		    (cddr path-list))
	        (if (and (eq 'incl (cadr rpath-list))
		       (eq 'pl1 (car rpath-list)))
		  else (display-error "This file is not an include file."))
	        (save-excursion
		(go-to-beginning-of-buffer)
		(if (or (empty-buffer-p current-buffer)
		        (not (looking-at (catenate comment-prefix " START OF"))))
		    (if (line-is-blank) else (open-space))
		    (insert-string (catenate comment-prefix " START OF:"))
		    (whitespace-to-hpos 20.)
		    (insert-string entry-name)
		    (whitespace-to-hpos 61.)
		    (do-times (// (- pl1-line-length 61. 
				 (stringlength comment-suffix)) 3.)
			    (insert-string " * "))
		    (whitespace-to-hpos
		      (- pl1-line-length
		         (stringlength comment-suffix)))
		    (insert-string comment-suffix)
		    (next-line)
		    (if (lastlinep)
		        else 
		        (if (line-is-blank) else (open-space))))
		(go-to-end-of-buffer)
		(if (bolp) (prev-line)	;buffer ends in newline, go up one
		    else (go-to-beginning-of-line))
		(if (not (looking-at (catenate comment-prefix " END OF:")))
		    (go-to-end-of-line)
		    (if (line-is-blank) else (new-line))
		    (new-line)
		    (insert-string (catenate comment-prefix " END OF:"))
		    (whitespace-to-hpos 20.)
		    (insert-string entry-name)
		    (whitespace-to-hpos 61.)
		    (do-times (// (- pl1-line-length 61.
				 (stringlength comment-suffix)) 3.)
			    (insert-string " * "))
		    (whitespace-to-hpos
		      (- pl1-line-length
		         (stringlength comment-suffix)))
		    (insert-string comment-suffix)
		    (insert-string NL))))))

(defprop pl1-include-file-comment-start-end
"Generates starting and ending comment lines indentifying name of a PL/I
include file at top and bottom of the buffer." documentation)
;;;
;;;
;;;	STATEMENT MOVEMENT (written by Dixon in May 79)
;;;

(defcom pl1-forward-statement 
        &numeric-argument (&repeat &lower-bound 1)
        &negative-function pl1-backward-statement
       (prog (token)
	   (do-forever
	     (setq token (get-pl1-token-forwards))
	     (if (eq token SEMI)
	         (stop-doing))
	     (if (null token)
	         (display-error)))))		; at end of file.

(defcom pl1-backward-statement 
        &numeric-argument (&repeat &lower-bound 1)
        &negative-function pl1-forward-statement
       (prog (token)
	   (with-mark current-loc
	        (setq token (get-pl1-token-backwards))
	        (if (null token)
		  (go-to-mark current-loc)
		  (display-error)))		; at beginning of file.
	   (do-forever
	     (with-mark current-loc
		(setq token (get-pl1-token-backwards))
		(if (eq token SEMI)
		    (do-times 2 (get-pl1-token-forwards))
		    (get-pl1-token-backwards)
		    (stop-doing))
		(if (null token)
		    (go-to-mark current-loc)
		    (stop-doing))))))

(defprop pl1-backward-statement
"Goes to the beginning of the current statement.  If already at the beginning
of a statement, goes to the beginning of the previous statement." 
documentation)

(defprop pl1-forward-statement 
"Goes forward to the end of this statement.  If at the end of a statement, 
goes forward to the end of the next statement." documentation)

;;;

;;;
;;;	AUTO-DECLARATOR (integrated 7/31/78)
;;;	  (written by Greenberg in April 78)
;;;         (rewritten by Dixon in May 79 to use get_entry_point_dcl_)
;;;


(defcom pl1dcl
        &numeric-argument (&reject)
       (prog (the-entry the-error the-dcl the-type)
	   (save-excursion
	     (skip-back-whitespace-in-line)
	     (with-mark here
		      (skip-back-to-whitespace)
		      (setq the-entry
			  (point-mark-to-string here))))
	   (let ((result (entry_point_dcl_ the-entry
				     pl1-dcl-style
				     pl1-line-length)))
	        (setq the-dcl (car result))
	        (setq the-type (cadr result))
	        (setq the-error (caddr result))
	        (cond ((samepnamep the-type 'abbrev)
		     (save-excursion
		       (backward-word)
		       (insert-string the-dcl)
		       (delete-word)))
		    ((> (stringlength the-dcl) 0)
		     (if (not (get (lefthand-char) 'whiteness))
		         (insert-char " "))
		     (insert-string (substr the-dcl 2))))
	        (if (> (stringlength the-error) 0)
		  (if (not (samepnamep the-error 'abbrev))   ;disagree with GDixon here.
		      (minibuffer-print the-error))))))

;;;

;;;
;;;	ELECTRIC MODE Functions (written by Greenberg on 07/31/78)
;;;

(defcom electric-pl1-semicolon
        &numeric-argument (&reject)
       (insert-string ";")
       (pl1-adjust-for-this-maybe-being-an-end-statement)
       (or macro-execution-in-progress (redisplay))    ;for benefit of printing ttys
       (if (or (lastlinep)
	     (save-excursion (next-line)
			 (line-is-blank)))
	 (pl1-cret-and-indent)))

(defun pl1-adjust-for-this-maybe-being-an-end-statement ()
       (if (= pl1-inding-style 1)
	 (with-mark m
		  (go-to-beginning-of-line)
		  (if (forward-search-in-line "end")
		      (go-to-mark m)
		      (let ((s (pl1-find-start-prev-sta)))
			 (if (eq 'end (cadr (pl1-typify-statement  (caddr s) nil)))
			     (go-to-mark (car s))
			     (roll-back-pl1-indentation))
			 (release-mark (car s))))
		  (go-to-mark m))))

(defcom electric-pl1-colon
        &numeric-argument (&reject)
       (insert-string ":")
       (if (pl1-legitimate-label-context)
	 (save-excursion (backward-word)
		       (delete-white-sides)
		       (if (not (bolp)) (new-line)))
	 (indent-pl1-statement)		; IS THIS RIGHT?
	 (or macro-execution-in-progress (redisplay))	;for printing tty's
	 (if-back-at COLON (pl1-cret-and-indent))))


(defun pl1-legitimate-label-context ()
       (save-excursion
         (do-forever
	 (let ((tok (get-pl1-token-backwards)))
	      (if (eq tok SEMI)(return t))	;just after another statement
	      (if (eq tok nil)(return t))	;at beginning of file
	      (if (not (eq tok COLON))(return nil))) ;gotta be a label or condition prefix
	 (let ((tok (get-pl1-token-backwards)))
	      (cond ((stringp tok))		; the preceding label (perhaps), but keep looking
		  ((eq tok CLOSE-PAREN)(return t)) ; subscript or prefix. good enuff for now.
		  (t (return nil)))))))	; no good, give up

;;;
;;;
;;; 	Language dependent part of ERROR SCANNER.
;;;	  (written by Schauble in March 79)


(defun pl1-error-list-builder ()
       (if (= buffer-uid -2)
	 (let ((er-list nil) (line-num))
	      (do-forever
	        (go-to-beginning-of-line)
	        (if (or (looking-at "WARNING")
		      (looking-at "FATAL ERROR")
		      (looking-at "ERROR"))
		  (go-to-end-of-line)
		  (with-mark m
			   (skip-back-to-whitespace)
			   (setq line-num (point-mark-to-string m)))
		  (backward-n-chars (stringlength "LINE "))
		  (setq line-num
		        (if (looking-at "LINE")
			  (let ((z (cv_dec_check_ line-num)))
			       (if (= 0 (car z)) (cadr z)))))

;; line-num is now fixnum of source error line number or nil if message
;;     had no line number

		  (go-to-beginning-of-line)
		  (setq er-list (cons (cons (set-mark) line-num) er-list)))
	        (insert-string "  ")
	        (if (firstlinep) (stop-doing) else (prev-line)))
	   
;;   er-list is now list of (<mark in fout> . <line number in source>)
;;   return it as the defun value.
	 er-list)
	 else
;;  Buffer does not contain a compilation, return error
	 'not-compile))
