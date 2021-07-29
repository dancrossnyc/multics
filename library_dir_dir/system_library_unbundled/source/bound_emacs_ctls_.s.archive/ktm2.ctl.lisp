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
;;;	Synertek KTM2 control package
;;;
;;;       JRD 9 Sept 79 from vt52ctl, debugged by BSG
;;;	BSG 3/21/78 from DD4000ctl
;;;


(declare (special X Y screenheight screenlinelen))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'ktm2)
       (Rtyo 33)(Rprinc "E")(Rtyo 177)(Rtyo 177)
       (setq X 0 Y 0))


					; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
	    nil)
	   ((and (= x 0)(= y 0))
	    (Rtyo 33)(Rprinc "H")
	    (setq X 0 Y 0))
	   (t
	     (let ((cost-of-rel (+ (abs (- x X))(abs (- y Y))))
		 (cost-of-left (+ 1 x (abs (- y Y))))
		 (cost-of-abs 4))
		(cond
		  ((and (< cost-of-rel cost-of-left)
		        (< cost-of-rel cost-of-abs))
		   (cond
		     ((> X x) (do i X (1- i) (= i x)(Rtyo 10)))
		     ((> x X) (do i X (1+ i) (= i x)(Rtyo 11))))
		   (cond
		     ((> Y y) (do i Y (1- i) (= i y)(Rtyo 13)))
		     ((> y Y) (do i Y (1+ i) (= i y)(Rtyo 12)))))
		  ((and (< cost-of-left cost-of-abs)(not (= X 0)))
		   (Rtyo 15) (setq X 0)
		   (DCTL-position-cursor x y))
		  (t (Rtyo 33)(Rprinc "=") (Rtyo (+ 40 y))(Rtyo (+ 40 x)))))
	     (setq X x Y y))))

; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "J")(Rtyo 177))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))



