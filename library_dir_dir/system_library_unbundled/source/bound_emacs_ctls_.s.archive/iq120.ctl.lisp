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
;;;	Soroc IQ120 control package
;;;       Ripped off from vt52ctl Paul Schauble 3/24/79
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'iq120)
       (Rtyo 33) (Rprinc "*")
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
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
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
                (Rtyo 33) (Rprinc "=")(Rtyo (+ 40 y))(Rtyo (+ 40 x))
                    )))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "Y"))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "T"))



