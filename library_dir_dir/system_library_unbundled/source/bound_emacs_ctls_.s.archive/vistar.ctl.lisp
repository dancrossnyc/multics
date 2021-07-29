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
;;;	VISTAR control package
;;;	BSG 3/21/78 from DD4000ctl
;;;

(declare (special X Y screenheight screenlinelen))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'vistar)
       (setq X 0 Y 0)
       (Rtyo 14))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 32)
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 31)))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 010))))
              (cond ((< Y y)
		 (do wy Y (1+ wy)(= wy y)(Rtyo 35)))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 34))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 27)(Rtyo x)(Rtyo y))))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen () (Rtyo 14))


; Clear to end of line.
(defun DCTL-kill-line () (Rtyo 13))



