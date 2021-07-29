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
;;;	Glass tty display control
;;;	From printing tty, BSG 6/29/78
;;;	Redone for new redisplay 7/7/78
;;;	For tty-no-cleolp, bsg 2/14/80

(declare (special X Y  screenheight idel-lines-availablep idel-chars-availablep screenlinelen tty-type tty-no-upmotionp tty-no-cleolp))
(declare (array* (notype (newscreen ?))))

(defun DCTL-init ()
   (setq X -777 Y -777)
   (setq tty-type 'teleray)
   (setq screenheight 24. screenlinelen 79.)
   (setq idel-chars-availablep nil idel-lines-availablep nil tty-no-upmotionp t tty-no-cleolp t))

(defun DCTL-position-cursor (x y)
 (prog ()
    (and (= x X)(= y Y)(return nil))
    (and (< X 0)(DCTL-crlf))
    (and (= y Y)
         (progn
	(cond ((and (= x 0)(> X 4))(DCTL-cret))
	      ((< X x)(DCTL-display-char-string
			(substr (or (cadr (newscreen Y)) "          ") (1+ X) (- x X))))
	       ((< (- X x) x) (do xx X (1- xx)(= xx x)(Rtyo 10)))
	       (t (DCTL-cret)
		(DCTL-position-cursor x Y)))
	(setq X x)    ;y is right by definition
	(return nil)))

	;; Definitely going to a new line at this point

    (DCTL-nextline)
    (setq Y y)
    (DCTL-position-cursor x y)))

(defun DCTL-assert-scpos (x y)
       (and x (setq X x))
       (and y (setq Y y)))

(defun DCTL-clear-rest-of-screen ())

(defun DCTL-nextline ()(Rtyo 12))

(defun DCTL-display-char-string (s)
     (Rprinc s)
     (setq X (+ X (stringlength s))))

(defun DCTL-cret ()
     (Rtyo 15)(setq X 0))

(defun DCTL-crlf ()
     (Rtyo 15)(Rtyo 12)(setq X 0))
