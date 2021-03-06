;;; ******************************************************
;;; *                                                    *
;;; * Copyright, (C) Honeywell Bull Inc., 1987           *
;;; *                                                    *
;;; * Copyright (c) 1978 by Massachusetts Institute of   *
;;; * Technology and Honeywell Information Systems, Inc. *
;;; *                                                    *
;;; ******************************************************

;;; HISTORY COMMENTS:
;;;  1) change(87-06-24,Coppola), approve(87-06-24,MCR7699),
;;;     audit(87-06-24,LJAdams), install(87-07-17,MR12.1-1042):
;;;     Add Emacs support (via this ctl) for MOWSE users with FANSI-CONSOLE
;;;     installed in their PC's.
;;;                                                      END HISTORY COMMENTS

;;;
;;;
;;;	 FANSI-CONSOLE control package for MOWSE
;;;	 Created:  11 May 1987 from vt102 control package
;;;	           

(%include e-macros)

(declare (*expr Rprinc Rtyo DCTL-standard-set-modes))
(declare (special X Y screenheight screenlinelen ospeed given-tty-type))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep
	        DCTL-underline-mask))
(declare (special region-scroll-availablep scroll-region-top scroll-region-bottom DCTL-insert-mode-on))
(declare (defpl1 not_ascii_ "" (char (*) aligned) (return bit (1) aligned)))



;;; Macro to output escape sequence
(defun fansi-escape macro (form)
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
       (do-times (// (* n ospeed) 960.)
		 (Rtyo 0))))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq DCTL-underline-mask t)
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq region-scroll-availablep nil)
       (setq screenheight 24.)
       (setq screenlinelen 79.)
       (memq given-tty-type
	         '(fansi mowse_fansi mowsef))
       (setq tty-type 'fansi)
       (DCTL-prologue)
       (DCTL-home-cursor)
       (DCTL-clear-rest-of-screen))

;;; Initialization that must also be done after a QUIT
(defun DCTL-prologue ()
       (Rtyo 33) (Rprinc "<")			;set ANSI mode from VT52 mode
       (fansi-escape "r")			;reset scroll region
       (fansi-escape "4l")			;reset insert mode
       (fansi-escape "20l")			;turn off auto-CRLF
       (DCTL-pad 102.)
       (setq scroll-region-top 0 scroll-region-bottom (1- screenheight))
       (setq DCTL-insert-mode-on nil))

;;; Restore terminal to outside state
(defun DCTL-epilogue ()
       (fansi-escape "r")			;reset scroll region
       (fansi-escape "4l")			;reset insert mode
       (fansi-escape "20h")			;turn on auto-CRLF
       (DCTL-pad 4)
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
		      (fansi-escape)
		      (if (not (= deltax 1)) (DCTL-outdec deltax))
		      (Rprinc "C"))
		     (t (cond ((= x 0)        ;move left
			     (fansi-escape)
			     (Rprinc "G"))
			    (t (fansi-escape)
			       (DCTL-outdec (- deltax))
			       (Rprinc "D"))))))
	        ((= deltax 0)
	         ;;make sure scroll region doesn't screw us.
	         (cond ((or (and (> y scroll-region-bottom)
			     (not (> Y scroll-region-bottom)))
			(and (< y scroll-region-top)
			     (not (< Y scroll-region-top))))
		      (fansi-absolute-position x y))
		     ((> deltay 0)		;move down
		      (fansi-escape)
		      (DCTL-outdec deltay)
		      (Rprinc "B"))
		     (t (fansi-escape)        ;move up
		         (DCTL-outdec (- deltay))
		         (Rprinc "A"))))
	        (t (fansi-absolute-position x y)))
	  (setq X x Y y)))


;;; Perform absolute cursor positioning
(defun fansi-absolute-position (x y)
       (fansi-escape)
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
		         (setq DCTL-insert-mode-on nil)
		         (fansi-escape "4l") (DCTL-pad 1.)))	;reset insert mode
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
				    (fansi-escape "m"))	;out of underline mode
			         (setq un nil)
			         (Rprinc ch))
			        (t	;underlined char (400-bit set)
				(or un (fansi-escape "4m"))
				(setq un t)
				(Rtyo (- (CtoI ch) 400)))))
		   (explodec string))
		 (and un (fansi-escape "m"))))))

;;; Home cursor to upper left corner.
(defun DCTL-home-cursor ()
       (setq X 0 Y 0)
       (fansi-escape H))

;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (fansi-escape J))

;;; Clear to end of line.
(defun DCTL-kill-line ()
       (fansi-escape K))

;;; Define the bounds of the scroll region.  Relative cursor
;;; movement can only be done within this region.
(defun DCTL-define-scroll-region (top bottom)
       (cond ((and (= top scroll-region-top) (= bottom scroll-region-bottom)))
	   (t (setq scroll-region-top top scroll-region-bottom bottom)
	      (fansi-escape "s")		;save cursor position
	      (fansi-escape)		;redefine scroll region (homes)
	      (cond ((not (= top 0))
		   (DCTL-outdec (1+ top))))
	      (cond ((not (= bottom (1- screenheight)))
		   (Rprinc ";")
		   (DCTL-outdec (1+ bottom))))
	      (Rprinc "r")
	      (fansi-escape "u")	          ;restore cursor position
	      (DCTL-pad 5.))))


;;; Insert n lines at the current cursor position
(defun DCTL-insert-lines (n)
       (fansi-escape) (DCTL-outdec n) (Rprinc "L"))


;;; Delete n lines at the current cursor position
(defun DCTL-delete-lines (n)
       (fansi-escape) (DCTL-outdec n) (Rprinc "M"))


;;; Move text in scroll region up n lines (inserts whitespace at bottom)
(defun DCTL-scroll-up-region (nlines bottom)
       (DCTL-define-scroll-region Y bottom)
       (let ((oldy Y))
	  (fansi-escape)
	  (DCTL-outdec nlines)
	  (Rprinc "S")
	  (setq Y oldy)))

;;; Move text in scroll region down n lines (inserts whitespace at top)
(defun DCTL-scroll-down-region (nlines bottom)
       (DCTL-define-scroll-region Y bottom)
       (fansi-escape)
       (DCTL-outdec nlines)
       (Rprinc "T"))

;;; Insert given text at the cursor
(defun DCTL-insert-char-string (string)
       (fansi-escape)
       (DCTL-outdec (stringlength string))
       (Rprinc "@")
       (DCTL-output-underlined-string string)
       (setq X (+ X (stringlength string))))


;;; Delete N characters at the cursor
(defun DCTL-delete-chars (n)
       (fansi-escape)
       (and (> n 1) (DCTL-outdec n))
       (Rprinc "P")
       (DCTL-pad n))

   