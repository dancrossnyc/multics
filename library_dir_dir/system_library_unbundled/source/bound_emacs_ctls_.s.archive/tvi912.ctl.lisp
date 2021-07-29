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
;;;            TVI912 Controller - ripped off from TVI920 controller.
;;;               written by R. Jarrell Aug. 1982
;;;

(declare (special X Y screenheight screenlinelen tty-type ospeed))
(declare (special idel-lines-availablep idel-chars-availablep))



;;; initialize terminal and terminal control package.

(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'tvi912)
       
       (Rtyo 36)(Rtyo 33)(Rprinc "Y")
       (setq X 0 Y 0))

;;; prologue and epilogue will go here


;;; Move terminal's cursor to desired position.

(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
	   ((and (= x 0)(= y 0))
	    (Rtyo 36)
	   (setq X 0 Y 0))
	   ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
	    (cond ((< X x)
		 (do ex X (1+ ex)(= ex x)(Rtyo 14)))
		((< x X)
		 (do ex x (1+ ex)(= ex X)(Rtyo 10))))
	    (cond ((< Y y)
		 (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
		((< y Y)
		 (do wy y (1+ wy)(= wy Y)(Rtyo 13))))
	    (setq X x Y y))
;; Direct cursor addressing is best.
              (t (setq X x Y y)
	       (Rtyo 33)(Rprinc "=")
                 (Rtyo (+ 40 y))(Rtyo (+ 40 x)))))


;;; Output string.

(defun DCTL-display-char-string (string)
       (Rprinc string)
       (setq X (+ X (stringlength string))))


;;; clear to end of screen.

(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "Y"))


;;; Clear to end of line.

(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "T"))


