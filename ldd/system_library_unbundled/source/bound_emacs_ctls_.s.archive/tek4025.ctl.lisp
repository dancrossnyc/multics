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
;;;	Tektronix 4025 control package
;;;	Snarfed from VIP7800 package,
;;;	In turn ripped off from VIP7200ctl
;;;
;;;	Roy A. Leban, January 15, 1979.
;;;
;;;    Notes on current problems with this implementation:
;;;    1) It is possible to have screens > 33 lines.  This is not done.
;;;    2) User setable command character desirable.

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special ctl-close-necessaryp))

; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq screenheight 33. screenlinelen 79.)
       (setq tty-type 'tek4025)(setq ctl-close-necessaryp t)
       (Rtyo 37)(Rprinc "wor 33")(Rtyo 15) ;Work space of 33 lines
       (Rtyo 37)(Rprinc "wor")(Rtyo 15)     ;Go to top of work space.
       (setq X 0 Y 33))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
	    (Rtyo 37)(Rprinc "jum")(Rtyo 15)
              (setq X 0 Y 0))
	   ((= x 0)                 ; column 1 of a line.
	    (Rtyo 37)(Rprinc "jum ")
	    (DCTL-4025-outnum (+ y 1))
	    (Rtyo 15)
	    (setq X x Y y))
	   ((and (= x X)(> Y y))    ; same column- up.
	    (Rtyo 37)(Rprinc "up ")
	    (DCTL-4025-outnum (- Y y))
	    (Rtyo 15)
	    (setq Y y))
	   ((and (= x X)(< Y y))       ; same column- down.
	    (Rtyo 37)(Rprinc "dow ")
	    (DCTL-4025-outnum (- y Y))
	    (Rtyo 15)
	    (setq Y y))
	   ((and (= y Y)(> X x))       ; same line- left.
	    (Rtyo 37)(Rprinc "lef ")
	    (DCTL-4025-outnum (- X x))
	    (Rtyo 15)
	    (setq X x))
	   ((and (= y Y)(< X x))       ; same line- right.
	    (Rtyo 37)(Rprinc "rig ")
	    (DCTL-4025-outnum (- x X))
	    (Rtyo 15)
	    (setq X x))
;;else do a jump with both line and column.
             (t (setq X x Y y)
	    (Rtyo 37)(Rprinc "jum ")
	    (DCTL-4025-outnum (+ y 1))
	    (Rprinc ",")
	    (DCTL-4025-outnum (+ x 1))
	    (Rtyo 15))))

; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 37)(Rprinc "dli 33")   ; max of 33 lines left in buffer.
       (Rtyo 15))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 37)(Rprinc "dch 80")   ; max of 80 chars left in line.
       (Rtyo 15))


(defun DCTL-insert-lines (n)
       (Rtyo 37)(Rprinc "ili ")   ; Note the inherent problem with this.
       (DCTL-4025-outnum n)       ; ili causes lines at the bottom to
       (Rtyo 15))                 ; roll of the screen but not out of
			    ; the terminal.

(defun DCTL-delete-lines (n)
       (Rtyo 37)(Rprinc "dli ")	; first delete the lines,
       (DCTL-4025-outnum n)
       (Rtyo 15)
       (DCTL-insert-lines n))		; then rejustify with inserts.


(defun DCTL-insert-char-string (str)
       (Rtyo 37)(Rprinc "ich")(Rtyo 15)
       (Rprinc str)
	     ; should automatically revert out. if not "!wor"
       (setq X (+ X (stringlength str))))
       

(defun DCTL-delete-chars (n)
       (Rtyo 37)(Rprinc "dch ")
       (DCTL-4025-outnum n)
       (Rtyo 15))

(defun DCTL-4025-outnum (n)
       (cond ((> n 9.)
	 (Rtyo (+ 60 (// n 10.)))
	 (setq n (- n (* 10. (// n 10.))))))
       (Rtyo (+ 60 n)))

(defun DCTL-close-screen ()
       (Rtyo 37)(Rprinc "wor 0")(Rtyo 15))
