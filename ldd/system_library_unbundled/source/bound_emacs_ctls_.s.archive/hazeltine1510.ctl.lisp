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
;;;	Hazeltine 1510 control package
;;;       Ripped off from VIP7800ctl by CDT, 01/80
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep nil)
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'haz1510)
       (Rtyo 33)(Rtyo 34)
       (setq X 0 Y 0)
       (DCTL-prologue))


;;; Prologue code
(defun DCTL-prologue ()
       (Rtyo 33) (Rtyo 34))

;;; Epilogue code
(defun DCTL-epilogue ()
       (Rtyo 33) (Rtyo 34))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 33)(Rtyo 22)
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 3))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 20)))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 10))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 33)(Rtyo 13)))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rtyo 14))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 33)(Rtyo 21)(Rtyo x)(Rtyo y)
                    )))


;;; Output string.
(defun DCTL-display-char-string (string)
       ((lambda (strx)
	      (cond ((= strx 0))		;bug in redisplay calls with no string
		  (t (Rprinc string)
		     (setq X (+ X strx)))))
        (stringlength string)))
	      

; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rtyo 30))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rtyo 17))


(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rtyo 32)))

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rtyo 23)))
