;;; ******************************************************
;;; *                                                    *
;;; * Copyright, (C) Honeywell Bull Inc., 1988           *
;;; *                                                    *
;;; * Copyright (c) 1978 by Massachusetts Institute of   *
;;; * Technology and Honeywell Information Systems, Inc. *
;;; *                                                    *
;;; ******************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-08-15,Coppola), approve(86-08-15,MCR7516),
;;;     audit(86-09-02,GDixon), install(86-09-04,MR12.0-1146):
;;;     Add Emacs ctl for Versaterm (Macintosh VT100 Term.)
;;;  2) change(88-01-16,GDixon), approve(88-09-20,MCR8002),
;;;     audit(88-10-07,Blair), install(88-10-07,MR12.2-1141):
;;;      A) Added support for longer versaterm line and page lengths.  The
;;;         user's current tty_ page and line length modes are used by Emacs.
;;;      B) Changed DCTL-clear-rest-of-screen to reset scroll mode.  This
;;;         allows escape via ^X CR to scroll up the Emacs buffers as nonEmacs
;;;         output appears.
;;;      C) Changed all places which reset scroll mode to use the proper
;;;         terminal size (either 80 char lines or 132 char lines).
;;;      D) Remove all region scrolling functions, as they have never worked
;;;         and were never enabled.  Emacs does not properly maintain X and Y
;;;         variables, which the scrolling software depends upon.
;;;                                                      END HISTORY COMMENTS

;;;
;;;
;;;	VersaTerm control package (Macintosh VT100/102 Terminal Emulator)
;;;	 Created:  20 May 1983 by B. Margolin from VT132 CTL
;;;	 Modified: 2 November 1984 by B. Margolin to remove unexecuted
;;;		 forms from DCTL-clear-rest-of-screen and DCTL-kill-line.
;;;
;;;                  May, 1986 by R. Coppola for VersaTerm (tm). Works only for
;;;		 VersaTerm Rev2.20 and higher.

(%include e-macros)

(declare (*expr Rprinc Rtyo DCTL-standard-set-modes))
(declare (special X Y screenheight screenlinelen ospeed given-tty-type))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep
	        DCTL-underline-mask))
(declare (special region-scroll-availablep scroll-region-top scroll-region-bottom DCTL-insert-mode-on))
(declare (special DCTL-oflow-enabled DCTL-have-nonstandard-setmodes))

(declare (defpl1 not_ascii_ "" (char (*) aligned) (return bit (1) aligned)))
(declare (defpl1 get-screen-size "vt1xx_ctl_util_$get_screen_size" (return fixed bin)
	       (return fixed bin)))
(declare (defpl1 vt1xx_ctl_util_$re_enable_oflow ""))


;;; Macro to output escape sequence
(defun vt102-escape macro (form)
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
(defun DCTL-pad (n)
       (or DCTL-oflow-enabled			;flow control should do it
	 (do-times (// (* n ospeed) 960.)
		 (Rtyo 0))))

;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
   (let ((screensize (get-screen-size)))
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq DCTL-underline-mask t)
       (setq screenheight (cadr screensize)
	   screenlinelen (car screensize))
       (setq idel-lines-availablep t
	   idel-chars-availablep t)
       (setq DCTL-oflow-enabled
	   (memq given-tty-type
	      '(versaterm_oflow versaterm_132c_oflow versaterm220_oflow versaterm220_132c_oflow)))
       (setq tty-type 'versaterm)
       (DCTL-prologue)
       (DCTL-home-cursor)
       (DCTL-clear-rest-of-screen)))

;;; Initialization that must also be done after a QUIT
(defun DCTL-prologue ()
       (Rtyo 33) (Rprinc "<")			;set ANSI mode from VT52 mode
       (vt102-escape "?4l")			;reset scroll mode (jump)
       (vt102-escape "?6l")			;reset absolute origin mode
       (vt102-escape "r")			;reset scroll region
       (vt102-escape "20l")			;turn off auto-CRLF
       (cond ((> screenlinelen 100.)
	    (vt102-escape "?3h"))
	   (t (vt102-escape "?3l")))
       (DCTL-pad 102.)
       (setq DCTL-insert-mode-on nil)
       (and DCTL-oflow-enabled (vt1xx_ctl_util_$re_enable_oflow)))


;;; Restore terminal to outside state
(defun DCTL-epilogue ()
       (vt102-escape "?4l")			;reset scroll mode (jump)
       (vt102-escape "?6l")			;reset absolute origin mode
       (vt102-escape "r")			;reset scroll region
       (DCTL-pad 4)
       (setq DCTL-insert-mode-on nil))


;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (vt102-absolute-position x y)
       (setq X x Y y))


;;; Perform absolute cursor positioning
(defun vt102-absolute-position (x y)
       (vt102-escape)
       (if (not (= y 0))
	 (DCTL-outdec (1+ y)))
       (if (not (= x 0))
	 (Rprinc ";")
	 (DCTL-outdec (1+ x)))
       (Rprinc "H"))


;;; Output string.
(defun DCTL-display-char-string (string)
       (let ((strx (stringlength string)))
	  (cond ((= strx 0))		;bug in redisplay calls with no string
	        (t (cond (DCTL-insert-mode-on
		         (setq DCTL-insert-mode-on nil)))
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
				    (vt102-escape "m"))	;out of underline mode
			         (setq un nil)
			         (Rprinc ch))
			        (t	;underlined char (400-bit set)
				(or un (vt102-escape "4m"))
				(setq un t)
				(Rtyo (- (CtoI ch) 400)))))
		   (explodec string))
		 (and un (vt102-escape "m"))))))

;;; Home cursor to upper left corner.
(defun DCTL-home-cursor ()
       (setq X 0 Y 0)
       (vt102-escape H))

;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (vt102-escape "?4l")			;reset scroll mode (jump)
       (vt102-escape "?6l")			;reset absolute origin mode
       (vt102-escape "r")			;reset scroll region
       (vt102-escape J)
       (cond ((> screenlinelen 100.)		;set proper screen width
	    (vt102-escape "?3h"))
	   (t (vt102-escape "?3l"))))


;;; Clear to end of line.
(defun DCTL-kill-line ()
       (vt102-escape K))

;;; Insert n lines at the current cursor position
(defun DCTL-insert-lines (n)
       (do-times n
       (vt102-escape "L")))


;;; Delete n lines at the current cursor position
(defun DCTL-delete-lines (n)
       (do-times n
       (vt102-escape "M")))


;;; Insert given text at the cursor
(defun DCTL-insert-char-string (string)
       (vt102-escape)
       (DCTL-outdec (stringlength string))
       (Rprinc "@")
       (DCTL-output-underlined-string string)
       (setq X (+ X (stringlength string))))


;;; Delete N characters at the cursor
(defun DCTL-delete-chars (n)
       (vt102-escape)
       (and (> n 1) (DCTL-outdec n))
       (Rprinc "P")))


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


;;; REGION SCROLLING DOESN'T WORK because X and Y variables aren't maintained properly by emacs.
;;; Define the bounds of the scroll region.  Relative cursor
;;; movement can only be done within this region.
;;;(defun DCTL-define-scroll-region (top bottom)
;;;       (cond ((and (= top scroll-region-top) (= bottom scroll-region-bottom)))
;;;	   (t (setq scroll-region-top top scroll-region-bottom bottom)
;;;	      (Rtyo 33) (Rprinc "7")		;push cursor position
;;;	      (Rtyo 33) (Rprinc "[")		;redefine scroll region (homes)
;;;	      (cond ((not (= top 0))
;;;		   (DCTL-outdec (1+ top))))
;;;	      (cond ((not (= bottom (1- screenheight)))
;;;		   (Rprinc ";")
;;;		   (DCTL-outdec (1+ bottom))))
;;;	      (Rprinc "r")
;;;	      (Rtyo 33) (Rprinc "8")	;pop cursor position
;;;	      (DCTL-pad 5.))))

;;; Move text in scroll region up n lines (inserts whitespace at bottom)
;;;(defun DCTL-scroll-up-region (nlines bottom)
;;;       (DCTL-define-scroll-region Y bottom)
;;;       (let ((oldy Y))
;;;	  (Rtyo 33) (Rprinc "7")		;save cursor position
;;;	  (DCTL-position-cursor 0 bottom)
;;;	  (do-times nlines
;;;		  (Rtyo 12) (DCTL-pad 5.))
;;;	  (Rtyo 33) (Rprinc "8")
;;;	  (setq Y oldy)))

;;; Move text in scroll region down n lines (inserts whitespace at top)
;;;(defun DCTL-scroll-down-region (nlines bottom)
;;;       (DCTL-define-scroll-region Y bottom)
;;;       (do-times nlines
;;;	       (Rtyo 33) (Rprinc "M") (DCTL-pad 5.)))
