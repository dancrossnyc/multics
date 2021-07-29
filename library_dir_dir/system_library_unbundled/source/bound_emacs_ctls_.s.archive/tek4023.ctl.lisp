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
;;;	TEK4023 pseudokludge
;;;
;;;	BSG 3/21/78 from DD4000ctl
;;;	BSG 2/14/80 for tty-no-cleolp :. no more kludge.
;;;

(declare (special X Y screenheight screenlinelen))
(declare (special idel-lines-availablep idel-chars-availablep tty-type tty-no-cleolp))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep nil idel-chars-availablep nil tty-no-cleolp t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'tek4023)
       (DCTL-clear-rest-of-screen))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 28.)(Rtyo 32.)(Rtyo 32.)
              (setq X 0 Y 0))
              (t (Rtyo 28.)(Rtyo (+ 40 x))(Rtyo (+ 40 y))
	      (setq X x Y y))))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()		;cheat- no eos, just home
       (Rtyo 33)(Rtyo 14)(mapc 'Rtyo '(0 0 0 0))
       (setq X 0 Y 0))


