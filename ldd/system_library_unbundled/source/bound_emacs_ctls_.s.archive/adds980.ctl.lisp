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
;;;	ADDS980 Kludgorama --- BSG 2/12/79... from...
;;;	HISI VIP7800 control package
;;;       Ripped off from VIP7200ctl  BSG 6/6/78 (!)
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (array* (notype (screen ?))))
(declare (special idel-lines-availablep idel-chars-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'adds980)
       (Rtyo 14)
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
	   ((=  x 0)
	    (Rtyo 13)(Rtyo (+ 100 y))
	    (setq X x Y y))
	   ((not (= y Y))
	    (DCTL-position-cursor 0 y)
	    (DCTL-position-cursor x y))
	   ((> x X)
	    (Rtyo 33)(Rtyo 5)
	    (Rtyo (+ 60 (// (- x X) 10.)))
	    (Rtyo (+ 60 (\ (- x X) 10.)))
	    (setq X x))
	   ((< (- X x) 6)
	    (do i (- X x)(1- i)(= i 0)(Rtyo 10)(setq X (1- X))))
	   (t (DCTL-position-cursor 0 Y)
	      (DCTL-position-cursor x y))))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()		;Dont have eos, do all.
       (Rtyo 14)(setq X 0 Y 0))

; Clear to end of line.
(defun DCTL-kill-line1 ()
       (do X1 X (1+ X1)(not (< X1 (cond ((screen Y)(cddr (screen Y)))
				(t 0))))
	 (Rtyo 40)(setq X (1+ X))))

(defun DCTL-kill-line ()
       ((lambda (ox oy)
	      (cond ((= Y (1- screenheight))
		   (DCTL-kill-line1))
		  ((and (screen Y)(< (- (cddr (screen Y)) X) 7))
		   (DCTL-kill-line1))
		  (t (Rtyo 15)
		     (setq X 0 Y (1+ Y))))
	      (DCTL-position-cursor ox oy))
        X Y))

(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rtyo 16)))

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rtyo 17)))
