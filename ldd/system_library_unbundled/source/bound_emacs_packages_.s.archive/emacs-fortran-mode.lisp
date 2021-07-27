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
;;;
;;; Fortran mode extension for Multics EMACS
;;;   Written by Paul Schauble   DVCP mail station C34  HVN 357-4531
;;;   On January 17, 1979


;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added missing *expr declarations.
;;;                                                      END HISTORY COMMENTS

;;;

(%include e-macros)
(defvar fortran-mode-hook nil)

(declare (special fortran-begin-comment-line fortran-end-comment-line
	        fill-prefix current-buffer-mode compiler compile-options
	        buffer-uid mode-identification error-list-builder
	        buffer-minor-modes error-list e-list))
(declare (*expr conditional-new-line cv_dec_check_ exit-error-scan-mode
	      internedp kill-contents-of-line))

;;;
;;;     Mode initialization
;;;

(defun fortran-mode ()
       (mapc '(lambda (x)
		  (set-key (car x) (cadr x)))
	   '((esc-/;	fortran-comment-line)
	     (^xc		fortran-begin-comment-block)
	     (esc-/:	fortran-label)
	     (^i		fortran-indent-statement)
	     (esc-^m	fortran-continue)
	     (esc-^c	compile-buffer)
	     (/'		fortran-abbrev-expander)
	     (esc-p	undefined-command)	; I don't know what these
	     (esc-n	undefined-command)	; mean to Fortran
	     (^x^d	locate-next-error)
	     (^xt		exit-error-scan-mode)))
       (setq current-buffer-mode 'Fortran)
       (setq fill-prefix "      ")
       (if (line-is-blank)
	 (without-saving (kill-contents-of-line) (insert-string "      ")))
       (register-local-var 'fortran-begin-comment-line) 
       (register-local-var 'fortran-end-comment-line)
       (register-local-var 'compiler)
       (register-local-var 'compile-options)
       (register-local-var 'mode-identification)
       (register-local-var 'error-list-builder)
       (setq fortran-begin-comment-line
	        "c     ========================================"
	   fortran-end-comment-line fortran-begin-comment-line
	   compiler "ft "
	   compile-options " -tb"
	   error-list-builder 'fortran-error-list-builder
	   mode-identification -1)
       (if (boundp 'error-list)	; end error scan mode if needed.
	 (if error-list (exit-error-scan-mode))
	 else
	 (setq error-list nil e-list nil))
       (if fortran-mode-hook (errset (funcall fortran-mode-hook))))

;;;
;;;	One time initialization, done when file is loaded
;;;

(mapc '(lambda (item)		;; initial list of abbrevs
	     (putprop (car item) (cdr item) 'fortran-abbrev))
      '((in integer/    nil nil)	(su subroutine/ 	nil nil)
        (di dimension/  nil nil)	(co continue	t   t)
        (re return      t   t)	(fu function/ 	nil nil)
        (au automatic/  nil nil)	(eq equivalence/ /( nil nil)
        (ex external/   nil nil)	(cn common/ 	nil nil)
        (fo format/ /(  t   nil)	(im implicit/ 	nil nil)
        )  )

;;;
;;;	Basic Fortran formating    1/17/79
;;;

(defun fortran-set-begin-comment ()
       (setq fortran-begin-comment-line
	   (trim-minibuf-response "Begin comment block: " NL )))

(defun fortran-set-end-comment ()
       (setq fortran-end-comment-line
	   (trim-minibuf-response "End comment block: " NL)))

(defun fortran-begin-comment-block ()
       (if (memq 'comment buffer-minor-modes)
	 (conditional-new-line fortran-end-comment-line)
	 (setq fill-prefix "      ")
	 (new-line)
	 (negate-minor-mode 'comment)
	 else
	 (conditional-new-line fortran-begin-comment-line)
	 (setq fill-prefix "c     ")
	 (new-line)
	 (assert-minor-mode 'comment)
	 ))

(defun fortran-continue ()
       (conditional-new-line "     &  "))

(defun fortran-comment-line ()
       (conditional-new-line "c     "))

(defun fortran-label ()
       (save-excursion
         (go-to-beginning-of-line)
         (delete-white-sides))
       (delete-white-sides)
       (if (> (cur-hpos) 5)
	 (display-error "Statement number too long")
	 else
	 (whitespace-to-hpos 6)
	 ))

(defun fortran-indent-statement ()
       (if (< (cur-hpos) 6)
	 (whitespace-to-hpos 6)
	 else
	 (insert-char TAB)))

;;;
;;;  Fortran abbrevs - January 29, 1979
;;;

(defun set-fortran-abbrev n
       (if (< n 2) (display-error "Too few arguments")
	 else
	 (let ((lab nil) (el nil))
	  (do i 3 (1+ i) (> i n)
	    ((lambda (x)
	        (cond
		((eq x 'label) (setq lab t))
		((eq x 'eol)   (setq el t))
		(t (display-error (catenate "Invalid option: " x)))))
	     (arg i)))
	  (putprop (arg 1) (list (arg 2) lab el) 'fortran-abbrev))))
	     
(defun fortran-abbrev-expander ()
       (prog (the-abbr)
	   (with-mark m
		    (backward-char) (backward-char)
		    (setq the-abbr (internedp (point-mark-to-string m)))
		    (setq the-abbr
			(if (symbolp the-abbr)
			    (get the-abbr 'fortran-abbrev)
			    else nil))
		    (if the-abbr
		        (if (cadr the-abbr) (fortran-label))
		        (without-saving (wipe-point-mark m))
		        (insert-string (car the-abbr))
		        (redisplay)
				   
		        (if (caddr the-abbr)
			  (if (or (lastlinep)
				(save-excursion (next-line)
					      (line-is-blank)))
			      (new-line)))
		        else (go-to-mark m)
		             (release-mark m)
		             (display-error "Undefined abbreviation"))
		    )))

;;; Language dependant part of error scanner.

(defun fortran-error-list-builder ()
       (if (= buffer-uid -1)
	 (let ((er-list nil) (line-num))
	      (do-forever
	        (go-to-beginning-of-line)
	        (if (or (looking-at "WARNING")
		      (looking-at "ERROR"))
		  (go-to-end-of-line)
		  (with-mark m
			   (skip-back-to-whitespace)
			   (setq line-num (point-mark-to-string m)))
		  (do-times 5 (backward-char))
		  (setq line-num
		        (if (looking-at "line")
			  (let ((z (cv_dec_check_ line-num)))
			       (if (= 0 (car z)) (cadr z)))))

;; line-num is now fixnum of source error line number or nil if message
;;     had no line number

		  (go-to-beginning-of-line)
		  (setq er-list (cons (cons (set-mark) line-num) er-list)))
	        (insert-string "  ")
	        (if (firstlinep) (stop-doing) else (prev-line)))
	   
;;  er-list is now list of (<mark in file_output> . <line number in source>)
;;   return it as the defun value.
	 er-list)
	 else
;;  Buffer does not contain a compilation, return error
	 'not-compile))
;; 
;; Change History
;; 
;; Original begun 1/17/79
;; 1/29/79 Added fortran-compile and supporting extended commands.
;; 1/30/79 Added fortran-abbrev-expanded and supporting commands.
;; 1/31/79 Modified fortran-abbrev-expanded package to make the 
;;	label and new-line hacks available to user abbrevs.
;; 2/1/79 Modified fortran-compile into compile buffer. Changed
;; 	 supporting commands to accepts args on command line rather
;; 	 than in minibuffer. Changed handling of local variables
;; 	 to be unconditionally set in each buffer. 
;; 2/3/79  Added without-saving to all deletes, so that normal use
;; 	 does not crud up the kill-ring.
;; 2/3/79  Changed abbrev-expanded to simplify use of properties.
;; 	 Added abbrev im for implicit.
;; 2/3/79  Changed fortran-abbrev-expander to not do automatic
;; 	 new-lines if the next line is not empty. Changed
;; 	 fortran-label so that it doesn't lose if done more than
;; 	 once to a line. 
;; 2/11/79 Added first edition of fortran-next-error. Scans backwards.
;; 3/2/79	 Changed compile-buffer and error scanner to be language
;; 	 independant and move them to a common segment. Also changed
;; 	 the error scanner to keep a mark list and to work forwards.

