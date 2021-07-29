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
;;;	VT132 control package
;;;	 Created:  24 April 1980 by G. Palter from VT132P CTL
;;;	 Modified: 11 March 1981 by G. Palter for new terminal types and to
;;;		    support flow control
;;;	 Modified: 20 August 1982 by B. Margolin to copy CAH's underlining
;;;		    code from vt100 CTL
;;;	 Modified: 12 October 1982 by B. Margolin for slight underline change
;;;

(%include e-macros)

(declare (*expr Rprinc Rtyo DCTL-standard-set-modes))
(declare (special X Y screenheight screenlinelen ospeed given-tty-type))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep
	        DCTL-underline-mask))
(declare (special region-scroll-availablep scroll-region-top scroll-region-bottom DCTL-insert-mode-on))
(declare (special DCTL-oflow-enabled DCTL-have-nonstandard-setmodes))

(declare (defpl1 not_ascii_ "" (char (*) aligned) (return bit (1) aligned)))
(declare (defpl1 vt1xx_ctl_util_$re_enable_oflow ""))


;;; Macro to output escape sequence
(defun vt132-escape macro (form)
       (list 'Rprinc
	   (apply 'catenate
		(cons (ItoC 33)
		      (cons "[" (cdr form))))))

;;; Output n to the terminal in decimal.
(defun DCTL-outdec (n)			;BSG 3/23/79
       (let ((have-output))
	  (do digi '(1000. 100. 10. 1) (cdr digi) (null digi)
	      ((lambda (rem)
		     (cond ((or have-output (> rem 0) (= (car digi) 1))
			  (Rtyo (+ 60 rem))
			  (setq have-output t)))
		     (setq n (\ n (car digi))))
	       (// n (car digi))))))


;;; Output padding, based on n pad characters at 9600-baud
;;;  (Padding is sent only if flow control is disabled and the line speed is
;;;   at least 4800 baud)
(defun DCTL-pad (n)
       (or DCTL-oflow-enabled			;flow control should do it
	 (< ospeed 480.)			;terminal not running hard
	 (do-times (// (* n ospeed) 960.)
		 (Rtyo 0))))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq DCTL-underline-mask t)
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq region-scroll-availablep t)
       (setq screenheight 24.)
       (setq screenlinelen
	   (or (cdr (assq given-tty-type
		        '((vt132 . 131.) (vt132_oflow . 131.)
		          (vt132_80c . 79.) (vt132_80c_oflow . 79.))))
	       131.))			;default to 132 wide
       (setq DCTL-oflow-enabled (memq given-tty-type '(vt132_oflow vt132_80c_oflow)))
       (setq tty-type 'vt132)
       (DCTL-prologue)
       (DCTL-home-cursor)
       (DCTL-clear-rest-of-screen))

;;; Initialization that must also be done after a QUIT
(defun DCTL-prologue ()
       (Rtyo 33) (Rprinc "<") (DCTL-pad 20.)	;set ANSI mode from VT52 mode
       (vt132-escape "?4l")			;reset scroll mode (jump)
       (vt132-escape "?6l")			;reset absolute origin mode
       (vt132-escape "r")			;reset scroll region
       (vt132-escape "4l")			;reset insert mode
       (vt132-escape "20l")			;turn off auto-CRLF
       (cond ((= screenlinelen 131.)		;set proper screen width
	    (vt132-escape "?3h") (DCTL-pad 122.))
	   (t (vt132-escape "?3l") (DCTL-pad 122.)))
       (setq scroll-region-top 0 scroll-region-bottom (1- screenheight))
       (setq DCTL-insert-mode-on nil)
       (and DCTL-oflow-enabled (vt1xx_ctl_util_$re_enable_oflow)))


;;; Restore terminal to outside state
(defun DCTL-epilogue ()
       (vt132-escape "r")			;reset scroll region
       (vt132-escape "4l")			;reset insert mode
       (setq DCTL-insert-mode-on nil))


;;; Move terminal's cursor to desired position.
;;;   Relative cursor movement commands are confined to the current scrolling region.  Absolute movement commands can
;;;   address the entire screen if if Origin Mode is reset.  Missing arguments in the absolute positioning command default
;;;   to one.  Relative commands can be used if the scroll boundaries are examined.
(defun DCTL-position-cursor (x y)
       (let ((deltax (- x X))
	   (deltay (- y Y)))
	  (cond ((= deltay 0)
	         (cond ((= deltax 0) nil)
		     ((> deltax 0)		;move right
		      (vt132-escape)
		      (if (not (= deltax 1)) (DCTL-outdec deltax))
		      (Rprinc "C"))
		     (t (cond ((= x 0) (Rtyo 15) (DCTL-pad (1+ (// X 4))))  ;move left
			    ((< (- deltax) 4)
			     (do-times (- deltax) (Rtyo 10)))
			    (t (vt132-escape)
			       (DCTL-outdec (- deltax))
			       (Rprinc "D"))))))
	        ((= deltax 0)
	         ;;make sure scroll region doesn't screw us.
	         (cond ((or (and (> y scroll-region-bottom)
			     (not (> Y scroll-region-bottom)))
			(and (< y scroll-region-top)
			     (not (< Y scroll-region-top))))
		      (vt132-absolute-position x y))
		     ((> deltay 0)		;move down
		      (cond ((< deltay 4)
			   (do-times deltay (Rtyo 12)))
			  (t (vt132-escape)
			     (DCTL-outdec deltay)
			     (Rprinc "B")))
		      (DCTL-pad (* 2 deltay)))
		     (t (cond ((= deltay -1)	;move up
			     (Rtyo 33) (Rprinc "M"))
			    (t (vt132-escape)
			       (DCTL-outdec (- deltay))
			       (Rprinc "A")))
		        (DCTL-pad (* 2 (1+ (- deltay)))))))
	        (t (vt132-absolute-position x y)))
	  (setq X x Y y)))


;;; Perform absolute cursor positioning
(defun vt132-absolute-position (x y)
       (vt132-escape)
       (if (not (= y 0))
	 (DCTL-outdec (1+ y)))
       (if (not (= x 0))
	 (Rprinc ";")
	 (DCTL-outdec (1+ x)))
       (Rprinc "H")
       (DCTL-pad (* 2 (+ (abs (- Y y)) (// (abs (- X x)) 4) 1))))


;;; Output string.
(defun DCTL-display-char-string (string)
       (let ((strx (stringlength string)))
	  (cond ((= strx 0))		;bug in redisplay calls with no string
	        (t (cond (DCTL-insert-mode-on
		         (setq DCTL-insert-mode-on nil)
		         (vt132-escape "4l") (DCTL-pad 10.)))	;reset insert mode
		 (DCTL-output-underlined-string string)
		 (setq X (+ X strx))))))

(defun DCTL-output-underlined-string (string)
       (cond ((zerop (not_ascii_ string))	;optimize standard string
	    (Rprinc string))
	   (t (let ((un nil))
		 (mapc
		   '(lambda (ch)
			  (cond ((< (CtoI ch) 400)	;normal char
			         (and un
				    (vt132-escape "m"))	;out of underline mode
			         (setq un nil)
			         (Rprinc ch))
			        (t	;underlined char (400-bit set)
				(or un (vt132-escape "4m"))
				(setq un t)
				(Rtyo (- (CtoI ch) 400)))))
		   (explodec string))
		 (and un (vt132-escape "m"))))))

;;; Home cursor to upper left corner.
(defun DCTL-home-cursor ()
       (setq X 0 Y 0)
       (vt132-escape H) (DCTL-pad 10.))

;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (vt132-escape J) (DCTL-pad 60.))


;;; Clear to end of line.
(defun DCTL-kill-line ()
       (vt132-escape K) (DCTL-pad 5))


;;; Define the bounds of the scroll region.  Relative cursor
;;; movement can only be done within this region.
(defun DCTL-define-scroll-region (top bottom)
       (cond ((and (= top scroll-region-top) (= bottom scroll-region-bottom)))
	   (t (setq scroll-region-top top scroll-region-bottom bottom)
	      (Rtyo 33) (Rprinc "7")		;push cursor position
	      (Rtyo 33) (Rprinc "[")		;redefine scroll region (homes)
	      (cond ((not (= top 0))
		   (DCTL-outdec (1+ top))))
	      (cond ((not (= bottom (1- screenheight)))
		   (Rprinc ";")
		   (DCTL-outdec (1+ bottom))))
	      (Rprinc "r")
	      (Rtyo 33) (Rprinc "8")
	      (DCTL-pad 25.))))	;pop cursor position


;;; Insert n lines at the current cursor position
(defun DCTL-insert-lines (n)
       (DCTL-scroll-down-region n (1- screenheight)))


;;; Delete n lines at the current cursor position
(defun DCTL-delete-lines (n)
       (DCTL-scroll-up-region n (1- screenheight)))


;;; Move text in scroll region up n lines (inserts whitespace at bottom)
(defun DCTL-scroll-up-region (nlines bottom)
       (DCTL-define-scroll-region Y bottom)
       (let ((oldy Y))
	  (Rtyo 33) (Rprinc "7")		;save cursor position
	  (DCTL-position-cursor 0 bottom)
	  (do-times nlines
		  (Rtyo 12) (DCTL-pad 45.))
	  (Rtyo 33) (Rprinc "8")
	  (setq Y oldy)))

;;; Move text in scroll region down n lines (inserts whitespace at top)
(defun DCTL-scroll-down-region (nlines bottom)
       (DCTL-define-scroll-region Y bottom)
       (do-times nlines
	       (Rtyo 33) (Rprinc 'M) (DCTL-pad 45.)))


;;; Insert the given text at the cursor
(defun DCTL-insert-char-string (string)
       (cond (DCTL-insert-mode-on)
	   (t
	     (setq DCTL-insert-mode-on t)
	     (vt132-escape "4h")))		;turn on insert mode
       (DCTL-output-underlined-string string)
       (setq X (+ X (stringlength string)))
       (DCTL-pad (max (* 2 (stringlength string)) (- screenlinelen X))))


;;; Delete N characters at the cursor
(defun DCTL-delete-chars (n)
       (vt132-escape)
       (and (> n 1) (DCTL-outdec n))
       (Rprinc "P")
       (DCTL-pad (* 7 n)))


;;; Replacement for e_pl1_$set_emacs_tty_modes that enables oflow if necessary
(or (and (boundp 'DCTL-have-nonstandard-setmodes)
         DCTL-have-nonstandard-setmodes)
    (progn (putprop 'DCTL-standard-set-modes
		(get 'e_pl1_$set_emacs_tty_modes 'subr)
		'subr)
	 (setq DCTL-have-nonstandard-setmodes t)))

(defun e_pl1_$set_emacs_tty_modes ()
       (DCTL-standard-set-modes)
       (and DCTL-oflow-enabled (vt1xx_ctl_util_$re_enable_oflow)))

(setq DCTL-oflow-enabled nil)			;above gets called once before DCTL-init


;;; Load in special key definitions for VT1XX terminals
(cond ((status feature Emacs)			;but only in Emacs
       (load (list (car (namelist (truename infile))) "vt1xx_keys_"))))
