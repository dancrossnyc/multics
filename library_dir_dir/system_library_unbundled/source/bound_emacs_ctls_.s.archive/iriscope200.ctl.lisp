;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;;	Iriscope 200 control package
;;;       Ripped off from vt52ctl BSG 3/9/78
;;;	Ripped off from VIP7200ctl by CAH 17 July 1980
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 16. screenlinelen 80.)
       (setq tty-type 'iriscope200)
       (Rtyo 30) (Rtyo 31)
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and				; near home position?
	      (< (DCTL-distance y 0 16.) (DCTL-distance Y y 16.))
	      (<
	        (+ (DCTL-distance x 0 80.) (DCTL-distance y 0 16.))
	        (+ (DCTL-distance X x 80.) (DCTL-distance Y y 16.))))
	    (Rtyo 31)			; yes: go there first
	    (setq X 0 Y 0)))
       (cond ((< (DCTL-distance x 0 80.) (DCTL-distance X x 80.))
	    (Rtyo 15)			; yes: go there
	    (setq X 0)))
       (cond ((< X x)
	    (cond ((< (- x X) 40.) (DCTL-rpt 25 (- x X)))
		(t (setq Y (1- Y)) (DCTL-rpt 10 (+ 80. (- X x))))))
	   (t
	     (cond ((< (- X x) 40.) (DCTL-rpt 10 (- X x)))
		 (t (setq Y (1+ Y)) (DCTL-rpt 25 (+ 80. (- x X)))))))
       (cond ((< Y y)
	    (cond ((< (- y Y) 8.) (DCTL-rpt 12 (- y Y)))
		(t (DCTL-rpt 32 (+ 16. (- Y y))))))
	   (t
	     (cond ((< (- Y y) 8.) (DCTL-rpt 32 (- Y y)))
		 (t (DCTL-rpt 12 (+ 16. (- y Y)))))))
       (setq X x Y y))


; find modular distance between two points
(defun DCTL-distance (A B Mod)
       (cond ((< (abs (- A B)) (// Mod 2)) (abs (- A B)))
	   ((< A B) (- (+ Mod A) B))
	   ((> A B) (- (+ Mod B) A))))


; send a cursor positioning string
(defun DCTL-rpt (Char Num)
 (do ex 1 (1+ ex) (> ex Num) (Rtyo Char)))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string)
       (cond ((< X 80.) nil)
             (t (setq X (- X 80.)) (setq Y (1+ Y)))))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       ((lambda (x y)
	      (Rtyo 30)
	      (setq X 0 Y 0)
	      (DCTL-position-cursor x y))
        X Y))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 26))

