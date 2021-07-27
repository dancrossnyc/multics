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
;;;	FOX-1100 control package
;;;	GMP on 08/17/78
;;;

(declare (special X Y screenheight screenlinelen))
(declare (special tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil
	   idel-chars-availablep nil
	   screenheight 24.
	   screenlinelen 79.
	   tty-type 'fox)
       (setq X -1 Y -1)
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X) (= y Y))
              nil)
             ((and (= x 0) (= y 0))
              (Rtyo 33) (Rprinc "H")
              (setq X 0 Y 0))
	   (t (or (= x X)
		(cond ((= x 0)
		       (Rtyo 15))
		      ((< (abs (- x X)) 2)
		       (cond ((< X x)
			    (do ex X (1+ ex) (= ex x)
			        (Rtyo 33) (Rprinc "C")))
			   ((< x X)
			    (do ex x (1+ ex) (= ex X) (Rtyo 010)))))
		      (t (Rtyo 33) (Rprinc "Y") (Rtyo (+ 40 x)))))
	      (or (= y Y)
		(cond ((= y (1+ Y))
		       (Rtyo 12))
		      ((< (abs (- y Y)) 2)
		       (cond ((< Y y)
			    (do wy Y (1+ wy) (= wy y)
			        (Rtyo 33) (Rprinc "B")))
			   ((< y Y)
			    (do wy y (1+ wy) (= wy Y)
			        (Rtyo 33) (Rprinc "A")))))
		      (t (Rtyo 33) (Rprinc "X") (Rtyo (+ 40 y)))))
	      (setq X x Y y))))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33) (Rprinc "J"))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33) (Rprinc "I"))



