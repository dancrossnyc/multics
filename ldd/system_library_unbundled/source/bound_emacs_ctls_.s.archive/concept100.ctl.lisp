;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;
;;; -*-LISP-*-

;;;
;;;        Concept 100 control package
;;;        DLW 3/12/79


;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declarations so that it would compile without warnings.
;;;                                                      END HISTORY COMMENTS

;;;


(%include e-macros)

(eval-when (compile) (setq ibase (+ 8 2)))

(declare (special
	X Y screenheight screenlinelen ospeed tty-type
	idel-lines-availablep
	idel-chars-availablep
	overstrike-availablep
	region-scroll-availablep
	c100-magic-constant		; Fudge factor for Concept 100 padding
	vmax
	))

(declare (*expr Rprinc Rtyo))

;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq screenheight 24 screenlinelen 79)
       (setq idel-lines-availablep t idel-chars-availablep nil)
       (setq region-scroll-availablep t)
       (setq tty-type 'c100)
       (setq overstrike-availablep t)		; Underscore!
       (setq c100-magic-constant
	   (//$ 1.0 
	        (-$ 1.0 
		  (+$ .45 (*$ .3 (//$ (float ospeed) 960.0))))
	        1000.0))

       (Rtyo 27) (Rprinc "U")			; Set programmer mode.
       (Rtyo 27) (Rprinc "f")			; Set text mode.
       (Rtyo 27) (Rprinc "7")			; Set character mode.
       (Rtyo 27) (Rprinc "5")			; Set upper/lower case mode.
       (Rtyo 27) (Rprinc "8")			; Set full duplex.
       (Rtyo 27) (Rprinc "l")			; Reset auto-linefeed.
       (Rtyo 27) (Rprinc "N")			; Send set attribute word command.
       (Rtyo 72)				; Word is all 0 except protect = 1
					;     (no protection)
       (Rtyo 27) (Rprinc "o")			; Change EOM to null.
       (Rtyo 38) (Rtyo 0)			; ...
;      (Rtyo 27) (Rprinc "$")			; Reset all function keys.
       ;; Here program the function keys, if we ever want to use them.

       (DCTL-define-full-width-window 0 23)
       (DCTL-clear-screen)			; Clear and home.
       ;; Here we could set the tab stops but there is probably no reason.
       )

;;; Move terminal's cursor to desired position.
;;; This first implementation is really cheapo and only uses
;;;   absolute cursor positioning.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X) (= y Y))
	    nil)
	   ((and (= x 0) (= y 0))
	    ;; Home up.
	    (Rtyo 27) (Rprinc "?"))
	   ((= (+ (abs (- x X))
		(abs (- y Y)))
	       1)
	    ;; We are only one away, use relative positioning.
	    (cond ((= x X)
		 (cond ((< y Y)  (Rtyo 27) (Rprinc ";"))
		       (t        (Rtyo 27) (Rprinc "<"))))
		(t
		 (cond ((< x X)  (Rtyo 27) (Rprinc ">"))
		       (t        (Rtyo 27) (Rprinc "="))))))
	   (t
	    ;; Use absolute positioning.
	    (Rtyo 27) (Rprinc "a")
	    (Rtyo (+ 32 y)) (Rtyo (+ 32 x))))
       (setq X x Y y)
       nil)

;;; Output a string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))

;;; Home up and clear screen.
(defun DCTL-clear-screen ()
       (Rtyo 12)
       (DCTL-c100-pad 12.0)
       (setq Y 0 X 0))

;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (if (and (= Y 0) (= X 0))
	 (DCTL-clear-screen)
        else
           (Rtyo 27) (Rtyo 5)
	 (DCTL-c100-pad (*$ 4.0 (float (- 24 Y))))))

;;; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 27) (Rtyo 21)
       (DCTL-c100-pad 4.0))

;;; Insert lines.
(defun DCTL-insert-lines (n)
       (do-times n
         (Rtyo 27) (Rtyo 18)
         (DCTL-c100-pad (*$ .75 (float (- vmax X))))))

;;; Delete lines.
(defun DCTL-delete-lines (n)
       (do-times n
         (Rtyo 27) (Rtyo 2)
         (DCTL-c100-pad (*$ .75 (float (- vmax X))))))

(defun DCTL-define-full-width-window (top bottom)
       (Rtyo 27)
       (Rprinc "v")
       (Rtyo (+ top 32))
       (Rtyo 32)
       (Rtyo (+ (- bottom top) 32 1))
       (Rtyo (+ 80 32))
       (setq Y top
	   X 0
	   vmax bottom))

;;; Move text in scroll region up n lines (inserts whitespace at bottom)
(defun DCTL-scroll-up-region (nlines bottom)
       (DCTL-define-full-width-window Y bottom)
       (DCTL-delete-lines nlines)
       (DCTL-define-full-width-window 0 23))

;;; Move text in scroll region down n lines (inserts whitespace at top)
(defun DCTL-scroll-down-region (nlines bottom)
       (DCTL-define-full-width-window Y bottom)
       (DCTL-insert-lines nlines)
       (DCTL-define-full-width-window 0 23))

;;; This takes a number of milliseconds, adjusts it by the
;;; magic constant, and sends the right number of pad characters.
(defun DCTL-c100-pad (a)
       (do-times (fix (*$ a c100-magic-constant (float ospeed)))
         (Rtyo 127)))
