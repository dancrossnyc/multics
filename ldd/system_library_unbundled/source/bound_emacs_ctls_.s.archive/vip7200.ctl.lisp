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
;;;	HISI VIP7200 control package
;;;       Ripped off from vt52ctl BSG 3/9/78
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'vip7200)
       (Rtyo 33)(Rprinc "H")(Rtyo 33)(Rprinc "J")
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 33)(Rprinc "H")
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 33)(Rprinc "C")))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 33)(Rprinc "D"))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 33)(Rprinc "B")))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "A"))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 33)(Rprinc "f")(Rtyo (+ 40 x))(Rtyo (+ 40 y))
                    )))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "J")
       (Rtyo 0))  ;needed only at 9.6kb


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))



