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
;;;       DATAMEDIA 1521 control package
;;;       Adapted from vip7200ctl by Richard Q. Kahler 7/10/79
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'dm1521)
       (Rtyo 14)                                  ;clear screen
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)                                ;already there
             ((and (= x 0)(= y 0))
              (Rtyo 31)                           ;cursor home
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 34)))    ;cursor right
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 10))))   ;cursor left
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 12)))    ;cursor down
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 37))))   ;cursor up
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
                (Rtyo 36)(Rtyo (+ 40 x))(Rtyo y)
                    )))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 13))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 35))
