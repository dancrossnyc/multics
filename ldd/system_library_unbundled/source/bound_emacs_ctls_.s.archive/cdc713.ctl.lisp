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
;;;	CDC713 control package
;;;       JJL, with help from BSG 08/12/79 from VISTAR
;;;

(declare (special X Y screenheight screenlinelen))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))

(declare (eval (read)))(setsyntax '/^ 'macro 'hatmac)
(declare (eval (read)))(defun hatmac ()(- (tyi) 100))
; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 16. screenlinelen 79.)
       (setq tty-type 'cdc713)
       (setq X 0 Y 0)
       (Rtyo ^Y)(Rtyo ^X))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y)))
	   ((and (= x 0)(= y 0))(Rtyo ^Y))
	   ((and (= x 0)(= y Y))(Rtyo ^M))
	   (t
	     (cond ((and (< x X)(> (- X x) x))
		  (cond ((and (< y Y)(> (- Y y) y))
		         (DCTL-position-cursor 0 0))
		        (t (DCTL-position-cursor 0 Y)))))
	     (cond ((< X x)
		  (do ex X (1+ ex)(= ex x)(Rtyo ^U)))
		 ((< x X)
		  (do ex x (1+ ex)(= ex X)(Rtyo ^H))))
	     (cond ((< Y y)
		  (do wy Y (1+ wy)(= wy y)(Rtyo ^J)))
		 ((< y Y)
		  (do wy y (1+ wy)(= wy Y)(Rtyo ^Z))))))
       (setq X x Y y))

; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen () (Rtyo ^X))


; Clear to end of line.
(defun DCTL-kill-line () (Rtyo ^V))
