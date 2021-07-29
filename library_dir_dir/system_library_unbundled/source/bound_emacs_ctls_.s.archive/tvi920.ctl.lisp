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
;;;        TVI920 control ripped off from ADM3A, TELERAY1061
;;;        by CLS         06/20/80
;;;           modified    08/11/80 to  fix insert-chars
;;;           modified    09/05/80 to  add pad control for =>1200 baud
;;;	    modified    09/18/80 by CDT to pad efficiently at all speeds

;;; The TVI920C has a 240-character writebehind buffer that can be used to
;;; good effect by carefully under-padding operations that need padding.
;;; Since there is no way to underpad these things deterministically (since
;;; emacs never lets you know when it has gone blocked for read and therefore
;;; you really don't know when the buffer is likely to have emptied itself out)
;;; we cautiously underpad by only slight amounts.

(declare (special X Y screenheight screenlinelen tty-type ospeed))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special DCTL-writebehind-buf-used))


;;; initialize terminal and terminal control package.

(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'tvi920)
       (DCTL-clear-writebehind-buf)
       (Rtyo 36)(Rtyo 33)(Rprinc "Y")
       (setq X 0 Y 0))

;;; prologue and epilogue will go here


;;; Move terminal's cursor to desired position.

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
;; Direct cursor addressing is best.
              (t (setq X x Y y)
	       (Rtyo 33)(Rprinc "=")
                 (Rtyo (+ 40 y))(Rtyo (+ 40 x)))))


;;; Output string.

(defun DCTL-display-char-string (string)
       (Rprinc string)
       (setq X (+ X (stringlength string))))


;;; clear to end of screen.

(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "Y"))


;;; Clear to end of line.

(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "T"))


;;; Insert lines n blank lines at current position.

(defun DCTL-insert-lines (n)
       (DCTL-clear-writebehind-buf)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rprinc "E")
	     (DCTL-underpad 78.))
       (setq X 0)
       (DCTL-clear-writebehind-buf))


;;; Delete lines.

(defun DCTL-delete-lines (n)
       (DCTL-clear-writebehind-buf)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rprinc "R")
	     (DCTL-underpad 78.))
       (setq X 0)
       (DCTL-clear-writebehind-buf))


;;; Insert Characters

(defun DCTL-insert-char-string (str)
       (DCTL-clear-writebehind-buf)
       (do i (stringlength str) (1- i) (= i 0)
	 (Rtyo 33) (Rprinc "Q")
	 (DCTL-underpad 19.))
       (Rprinc str)
       (DCTL-clear-writebehind-buf)
       (setq X (+ X (stringlength str))))


;;; Delete Characters.

(defun DCTL-delete-chars (n)
       (DCTL-clear-writebehind-buf)
       (do i 0 (1+ i)(= i n)
	 (Rtyo 33)(Rprinc "W")
	 (DCTL-underpad 19.))
       (DCTL-clear-writebehind-buf))


;;; Send pad characters to wait specified number of milliseconds
;;; We underpad to take advantage of the 240-char writebehind buffer in the
;;; terminal.  We underpad by 1/3 the buffer and hope it works.

(defun DCTL-underpad (n)
       (do i (1+ (// (* n ospeed) 1000.)) (1- i) (= i 0)
	 (setq DCTL-writebehind-buf-used (1+ DCTL-writebehind-buf-used))
	 (cond ((> DCTL-writebehind-buf-used 80.)(Rtyo 0)))))


(defun DCTL-clear-writebehind-buf ()
       (setq DCTL-writebehind-buf-used 0))
