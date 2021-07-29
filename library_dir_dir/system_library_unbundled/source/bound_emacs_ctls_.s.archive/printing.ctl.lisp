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
;;;	Printing tty display control
;;;	Redone for new redisplay 7/7/78
;;;	Large redo 8/9/78 for tabs, delays, and no screen knowledge.

(declare (special X Y  screenheight idel-lines-availablep idel-chars-availablep screenlinelen tty-type tty-no-upmotionp overstrike-availablep))
(declare (special DCTLV-vertnl-pad DCTLV-horznl-pad DCTLV-consttab-pad DCTLV-vartab-pad DCTLV-tabs-availablep DCTLV-backspace-pad DCTLV-one-time-bspad))
(declare (defpl1 e_pl1_$get_mcs_tty_info "" (return bit (1))(return float bin)(return fixed bin)
         (return float bin)(return fixed bin)(return fixed bin)(return fixed bin)))


(defun DCTL-init ()
   (setq X -777 Y -777)
   (setq tty-type 'printing)
   (setq screenheight 60.)
   (setq idel-chars-availablep nil idel-lines-availablep nil tty-no-upmotionp t overstrike-availablep t)
   ((lambda (result)
	  (setq DCTLV-tabs-availablep (not (zerop (car result))) result (cdr result))
	  (setq DCTLV-horznl-pad (car result) result (cdr result))
	  (setq DCTLV-vertnl-pad (abs (car result)) result (cdr result))
	  (setq DCTLV-vartab-pad (car result) result (cdr result))
	  (setq DCTLV-consttab-pad (car result) result (cdr result))
	  (setq DCTLV-backspace-pad (car result) screenlinelen (cadr result)))
    (e_pl1_$get_mcs_tty_info))
   (setq screenlinelen (cond ((= screenlinelen 79.) 79.)
		         (t (1- screenlinelen))))
   (setq DCTLV-one-time-bspad
         (cond ((< DCTLV-backspace-pad 0)(prog2 0 (- DCTLV-backspace-pad)(setq DCTLV-backspace-pad 0)))
	     (t 0))))

(defun DCTL-position-cursor (x y)
   (prog ()
    (and (= x X)(= y Y)(return nil))
    (and (< X 0)(DCTL-crlf))		;unrandomize
    (and (= y Y)
         (progn
	(cond ((< X x)			;going forward
	       (cond ((not DCTLV-tabs-availablep)    ;no tabs
		    (do xx X (1+ xx)(= xx x)(Rtyo 40)))
		   (t (DCTL-tab-forward X x))))
	       ((< (- X x) x)
	        (DCTL-delay DCTLV-one-time-bspad)
	        (do xx X (1- xx)(= xx x)(Rtyo 10)
			(DCTL-delay DCTLV-backspace-pad)))
	       (t (DCTL-cret)
		(DCTL-position-cursor x Y)))
	(setq X x)    ;y is right by definition
	(return nil)))

	;; Definitely going to a new line at this point

      (DCTL-nextline)
    (setq Y y)
    (DCTL-position-cursor x y)))

(defun DCTL-tab-forward (here there)
   (prog (targ-stops targ-rem cur-stops)
       (setq targ-stops (// there 10.) targ-rem (\ there 10.))
       (setq cur-stops (// here 10.))
       ;;
       ;;  Figure out the relative costs.
       ;;
       (cond ((and (not (= targ-stops cur-stops)) ;dont even bother
	         (< (+ targ-rem		;spaces to be output
		     (* (- targ-stops cur-stops)   ;number of tabs
		        (+ DCTLV-consttab-pad	;constant padding
			 1		;the actual tab
			 (fix (*$ 10e0 DCTLV-vartab-pad)))))
		  (- there here)))		;normal cost
					;do it
	    (do tabx cur-stops (1+ tabx)(= tabx targ-stops)
		(Rtyo 11)			;tab
		(DCTL-delay (+ DCTLV-consttab-pad (fix (*$ 10e0 DCTLV-vartab-pad)))))
	    (setq here (* targ-stops 10.))))
       (do xx here (1+ xx)(= xx there)(Rtyo 40))))


(defun DCTL-assert-scpos (x y)
       (and x (setq X x))
       (and y (setq Y y)))

(defun DCTL-clear-rest-of-screen ()(DCTL-nextline))

(defun DCTL-nextline ()(Rtyo 12)(DCTL-delay DCTLV-vertnl-pad))

(defun DCTL-kill-line ()(Rtyo 12)(DCTL-delay DCTLV-vertnl-pad))

(defun DCTL-display-char-string (s)
     (Rprinc s)
     (setq X (+ X (stringlength s))))

(defun DCTL-cret ()
     (Rtyo 15)(DCTL-delay (+ 3 (fix (*$ (float X) DCTLV-horznl-pad))))(setq X 0))

(defun DCTL-crlf ()
     (DCTL-cret)(DCTL-nextline))

(defun DCTL-delay (n)
   (do i 1 (1+ i)(> i n)(Rtyo 177)))
