;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declarations so that it would compile without warnings.
;;;                                                      END HISTORY COMMENTS


;;;
;;; Special key definitions for VT1XX terminals: control, meta, control-meta,
;;;  and meta-control prefix functions

;;; Created:  11 March 1981 by G. Palter

(%include e-macros)

(declare (special last-command-triplet-mpfxk last-command-triplet-1))
(declare (array* (notype (key-bindings 128. 2))))
(declare (*expr execute-command last-command-triplet))

;;; Called as part of function that reads a key name from the minibuffer
(defun key-prompt-1 (metap key prefix)
       (prog (mf1)
	   (and (or prefix (= metap 1))
	        (< key (1+ (CtoI "z")))(> key (1- (CtoI "a")))
	        (setq key (- key 40)))
	   (setq mf1 (cond ((eq (key-bindings key 0) 'control-prefix) 'control-prefix)
		         (prefix (arraycall t (key-bindings prefix 0) key))
		         (t (key-bindings key metap))))
	   (cond ((eq mf1 'escape)
		(minibuffer-print-noclear "esc-")
		(return (key-prompt-1 1 (get-char) nil)))
	         ((eq mf1 'control-prefix)
		((lambda (next-char)
		         (cond ((and (> next-char (1- (CtoI "@"))) (< next-char (1+ (CtoI "_"))))
			      (return (key-prompt-1 metap (- next-char (CtoI "@")) prefix)))
			     ((and (> next-char (1- (CtoI "a"))) (< next-char (1+ (CtoI "z"))))
			      (return (key-prompt-1 metap (- next-char (1- (CtoI "a"))) prefix)))
			     (t (display-error "Bad control character: " (printable next-char)))))
		 (get-char)))		;need to look further
	         ((not (symbolp mf1))
		(minibuffer-print-noclear (printable key)
				      " (prefix char): ")
		(return (key-prompt-1 0 (get-char) key)))
	         (t (minibuffer-print-noclear (printable key))
		  (return (list metap key prefix))))))


;;; Execute a "key" as an Emacs command:  A "key" is the triplet consisting
;;;  of a character, "meta"-bit, and prefix character used to determine the
;;;  exact command to be executed.
(defun execute-key (metap ch prefix)
       (let ((command))			;the command to execute
	  (and (or (= metap 1) prefix)
	       (and (< ch (1+ (CtoI "z")))
		  (> ch (1- (CtoI "a")))
		  (setq ch (- ch 40))))
	  (cond ((not prefix) (setq command (key-bindings ch metap)))
	        (t (setq command (arraycall t (key-bindings prefix 0) ch))))
	  (cond ((symbolp command)		;normal command
	         (setq last-command-triplet-mpfxk (cond ((= metap 1) 'meta)
					        (t prefix))
		     last-command-triplet-1 ch)
	         (execute-command command (last-command-triplet) nil))
	        (t			;a prefix character
		(let ((next-char (get-char)))
		     (cond ((eq (key-bindings next-char 0) 'control-prefix)
			  (let ((the-char (get-char)))     ;controllify next character
			       (cond
			         ((and (> the-char (1- (CtoI "@"))) (< the-char (1+ (CtoI "_"))))
				(execute-key 0 (- the-char (CtoI "@")) ch))
			         ((and (> the-char (1- (CtoI "a"))) (< the-char (1+ (CtoI "z"))))
				(execute-key 0 (- the-char (1- (CtoI "a"))) ch))
			         (t (ring-tty-bell)))))    ;can't be control char
			 (t		;ordinary char after prefix
			   (execute-key 0 next-char ch))))))))


;;; Command that does real work of ESC
(defcom escape-dont-exit-minibuf
        &numeric-argument (&pass)
        (prog (nxcn numf negate)
a 	    (setq nxcn (get-char))
	    (cond ((and (> nxcn (1- (CtoI "0"))) (< nxcn (1+ (CtoI "9"))))	;number
		 (or numarg (setq numarg 0))
		 (setq numarg (+ (- nxcn (CtoI "0")) (* 10. numarg)))
		 (setq numf t)
		 (go a))
		((and (not numf) (= nxcn (CtoI "-")))	;want negative argument
		 (setq negate t numf t) (go a))
		((and (not numf) (= nxcn (CtoI "+")))	;want positive argument
		 (setq numf t) (go a))
		(t (and numf negate		;negative argument (default -1)
		        (setq numarg (- (or numarg 1))))
		   (cond (numf (process-char nxcn))
		         ((eq (key-bindings nxcn 0) 'control-prefix)
			(control-meta-prefix))   ;ESC-^^ -- control-meta
		         (t (execute-key 1 nxcn nil)))))))


;;; Control prefix: reads characters building the numeric argument if fed
;;;  digits; when a non-digit is given, executes the control function of
;;;  said character
(defcom control-prefix
        &numeric-argument (&pass)
        (prog (nxcn numf negate)
a 	    (setq nxcn (get-char))
	    (cond ((and (> nxcn (1- (CtoI "0"))) (< nxcn (1+ (CtoI "9"))))	;number
		 (or numarg (setq numarg 0))
		 (setq numarg (+ (- nxcn (CtoI "0")) (* 10. numarg)))
		 (setq numf t)
		 (go a))
		((and (not numf) (= nxcn (CtoI "-")))	;want negative argument
		 (setq negate t numf t) (go a))
		((and (not numf) (= nxcn (CtoI "+")))	;want positive argument
		 (setq numf t) (go a))
		(t (and numf negate		;negative argument (default -1)
		        (setq numarg (- (or numarg 1))))
		   (cond (numf (process-char nxcn))
		         ((eq (key-bindings nxcn 0) 'escape)
			(control-meta-prefix))   ;^^-ESC: control-meta
		         ((and (> nxcn (1- (CtoI "@"))) (< nxcn (1+ (CtoI "_"))))
			(process-char (- nxcn (CtoI "@"))))
		         ((and (> nxcn (1- (CtoI "a"))) (< nxcn (1+ (CtoI "z"))))
			(process-char (- nxcn (1- (CtoI "a")))))
		         (t (ring-tty-bell))))))
        &documentation
"Used to enter control characters when the terminal or network uses those
characters for its own purposes.  If $$$ is followed by digits or the minus
sign (-) or plus sign (+), a numeric argument is collected just as is done for
$$escape$.  (E.g: Typing $$$123$$go-to-line-number$ will go to line 123).
Typing $$$-S is equivalent to typing ^S; typing $$$-$$escape$-S is equivalent
to typing $$escape$-^S.")


;;; Control-meta prefix: reads characters building the numeric argument if fed
;;;  digits; when a non-digit is given, executes the ESC-control function of
;;;  said character
(defcom control-meta-prefix
        &numeric-argument (&pass)
        (prog (nxcn numf negate)
a 	    (setq nxcn (get-char))
	    (cond ((and (> nxcn (1- (CtoI "0"))) (< nxcn (1+ (CtoI "9"))))	;number
		 (or numarg (setq numarg 0))
		 (setq numarg (+ (- nxcn (CtoI "0")) (* 10. numarg)))
		 (setq numf t)
		 (go a))
		((and (not numf) (= nxcn (CtoI "-")))	;want negative argument
		 (setq negate t numf t) (go a))
		((and (not numf) (= nxcn (CtoI "+")))	;want positive argument
		 (setq numf t) (go a))
		(t (and numf negate		;negative argument (default -1)
		        (setq numarg (- (or numarg 1))))
		   (cond (numf (process-char nxcn))
		         ((and (> nxcn (1- (CtoI "@"))) (< nxcn (1+ (CtoI "_"))))
			(execute-key 1 (- nxcn (CtoI "@")) nil))
		         ((and (> nxcn (1- (CtoI "a"))) (< nxcn (1- (CtoI "z"))))
			(execute-key 1 (- nxcn (1- (CtoI "a"))) nil))
		         (t (ring-tty-bell)))))))

(set-permanent-key '^^ 'control-prefix)

(sstatus uuolinks nil)
