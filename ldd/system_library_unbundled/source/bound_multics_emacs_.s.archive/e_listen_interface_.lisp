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
;;;	EMACS editor interface
;;;	  08/27/78 GMP
;;;	  Reorganized 3/20/79 by BSG to flush the table.
;;;	Moved argument parsing to PL/1, 21 July 1981 RMSoley
;;;       Modified: 26 November 1983 B. Margolin to not call e_pl1_ network entries.
;;;	Modified: 19 December 1983 B. Margolin to properly strip off ".ctl",
;;;		not err in ttp-validate if given a starname, complain
;;;		if -ttp/-query used in video system.
;;;

(setq errlist '((e-listen-interface)))		; set initial function

(declare (special tty-ctl-dir terminal-list next-multics-argno mode-line-herald
	        given-tty-type emacs-name null-pointer network-flag
	        args:paths args:apply-arg tasking-emacs args:force
	        args:ll args:pl args:ns tasking-arg terminal-type ttp-arg))
(declare (*expr absolute_pathname_ e_argument_parse_$get_startup_info 
	      e_argument_parse_$get_ttp_info e_lap_$rtrim
	      e_pl1_$get_real_terminal_type
	      e_pl1_$get_terminal_type e_pl1_$init e_pl1_$set_line_speed_
	      e_pl1_$set_terminal_type
	      e_pl1_$will_supdup_output e_tasking_$get_tasking_flag
	      emacs$get_my_name emacs$set_emacs_return_code error_table_
	      exists-file
	      expand_pathname_ lisp-quit-function list_emacs_ctls$find_ctl
	      list_emacs_ctls$list_emacs_ctls listener-level loadfile
	      nullstringp e_terminal_io_$check_printing))

(defun e-listen-interface ()
       (setq errlist '((lisp-quit-function)))
       (sstatus mulpi (sstatus mulquit t))
       (setq emacs-name (make_atom (e_lap_$rtrim (emacs$get_my_name))))
       (e_pl1_$init)
       (setq network-flag 0)		; no more network
       (and (setq tasking-emacs (not (zerop (e_tasking_$get_tasking_flag))))
	  (setq mode-line-herald (catenate "Tasking " mode-line-herald)))
       (let ((ttp-args (e_argument_parse_$get_ttp_info))
	   (su-args (e_argument_parse_$get_startup_info)))
	  (setq args:ns (zerop (car su-args))
	        tasking-arg (= 1 (cadr su-args))
	        args:paths (caddr su-args)
	        args:pl (cadddr su-args)
	        args:ll (car (cddddr su-args))
	        args:apply-arg (caddr (cddddr su-args)))
	  (let ((linespeed (cadr (cddddr su-args))))
	       (or (< linespeed 0)
		 (e_pl1_$set_line_speed_ (// linespeed 10.))))
	  (setq terminal-type (e_lap_$rtrim (cadr ttp-args))
	        args:force (> (car ttp-args) 99.)
	        ttp-arg (\ (car ttp-args) 100.)))
       (cond ((or (eq emacs-name 'emacs_) (zerop ttp-arg))	;no ttp arg
	    (eli:start))
	   ((= ttp-arg 1)			; -reset
	    (e_pl1_$set_terminal_type "") (e_pl1_$set_line_speed_ 0)
	    (eli:start))
	   ((and (not args:force)
	         (samepnamep (e_pl1_$get_real_terminal_type) "video_system"))
	    (princ (catenate "emacs: "
			 (cond ((= ttp-arg 2) "-query")
			       (t "-terminal_type"))
			 " not valid when using the video system."))
	    (terpri)
	    (lisp-quit-function))
	   ((= ttp-arg 2) (eli:start-internal (eli:query nil)))	; -query
	   (t (eli:start-internal terminal-type))))  ; -ttp FOO

(defun eli:start ()
       (let ((ttp (e_pl1_$get_terminal_type)))
	  (cond ((not (nullstringp ttp))
	         (eli:start-internal ttp))
	        ((and (= network-flag 1)
		    (not (zerop (e_pl1_$will_supdup_output))))
	         (eli:start-internal 'supdup_output))
	        (t (eli:start-internal (e_pl1_$get_real_terminal_type))))))

(defun eli:start-internal (ttp)
       (cond ((samepnamep ttp "ASCII") (setq ttp (eli:query nil)))
	   ((= 1 (e_terminal_io_$check_printing ttp)) (setq ttp "printing")))
       (do ((ans (make_atom (e_lap_$rtrim ttp)) (eli:query t)))
	 ((ttp-validate ans t))
	 (princ "Unknown terminal type.") (terpri)))

(defun eli:query (bothersome)
       (terpri)
       (cond (bothersome (princ "Do you want a list of known types? ")
		     (and (memq (car (explodec (readline))) '(y Y))
			(eli:produce-terminal-list))
		     (princ "Type ""quit"" to abort.")))
       (terpri)
       (princ "What type of terminal do you have? ")
       (do ((in (errset (readlist (explodec (readline))))
	      (errset (readlist (explodec (readline))))))
	 (())
	 (cond ((or (null in)
		  (numberp (setq in (car in)))
		  (nullstringp in)
		  (memq in '(? % *))
		  (not (symbolp in)))
	        (terpri)
	        (princ "Do not understand.  Try again: "))
	       ((eq (lowercase-ttp in) 'quit) (lisp-quit-function))
	       (t (return in)))))

(defun eli:produce-terminal-list ()
       (list_emacs_ctls$list_emacs_ctls "**"))

;;; This function attempts to find the given type terminal controller.
(defun ttp-known-type (type)
       (cond ((eq (get type 'ttyequiv) 'unsupported)
	    (princ
	      "This program requires a full-duplex ASCII terminal.  Sorry.")
	    (terpri) (lisp-quit-function))
	   ((or (not (zerop (index type "<")))
	        (not (zerop (index type ">"))))
	    nil)
	   (t ; Find ctl by emacs_terminal_ctls search path.
	     (let ((s (e_lap_$rtrim (list_emacs_ctls$find_ctl type))))
		(cond ((nullstringp s) nil)
		      (t s))))))

(defun ttp-validate (type set-type-flag)
       (let ((known? (ttp-known-type (lowercase-ttp type))))
	  (cond (known?
		(setq type (lowercase-ttp type))
		(setq given-tty-type (intern (make_atom type)))
		(and set-type-flag (e_pl1_$set_terminal_type type))
		(setq type (or (get type 'ttyequiv) type))
		(load known?)
		(start-emacs))		; NEVER RETURNS
	        ((exists-file type 4)
	         (and set-type-flag
		    (e_pl1_$set_terminal_type
		      (car (absolute_pathname_ type))))
	         (setq given-tty-type
		     (make_atom
		       (let ((result (cadr (expand_pathname_ type))))
			  (let ((ix (index result " ")))
			       (and (zerop ix) (setq ix 33.))
			       (cond ((= ix 1) "????")
				   ((samepnamep ".ctl"
					      (substr result
						    (- ix 4)
						    4))
				    (substr result 1 (- ix 5)))
				   ((samepnamep "ctl"
					      (substr result
						    (- ix 3)
						    3))
				    (substr result 1 (- ix 4)))
				   (t (substr result 1 (1- ix))))))))
	         (loadfile type)
	         (start-emacs))		; NEVER RETURNS
	        ((eq emacs-name 'emacs_)
	         (emacs$set_emacs_return_code
		 (error_table_ 'action_not_performed))
	         (lisp-quit-function))
	        (t nil))))

(defun lowercase-ttp (string)
       (implode (mapcar '(lambda (y)
			   (cond ((and (< y (1+ (CtoI "Z")))
				     (> y (1- (CtoI "A"))))
				(ascii (+ y 40)))
			         (t y)))
		    (exploden string))))

(defun start-emacs () (listener-level) (lisp-quit-function))

;;; Unsupportable terminals
(mapc '(lambda (x) (putprop x 'unsupported 'ttyequiv))
      '(/1050 /2741 ards corr2741 g115 g115_upper ibm2780
	    ibm2780_lower ibm3780 ibm3780_lower))





