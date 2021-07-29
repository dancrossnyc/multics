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
;;;	DATAMEDIA 3000 control package
;;;	 WOS, 11/08/78 from TELERAY1061 package
;;;

(declare (special X Y screenheight screenlinelen ospeed))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t
             screenheight 24. screenlinelen 79.
             tty-type 'dm3000
             X -777 Y -777)
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))


;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X) (= y Y))
	    nil)
	   ((and (= x 0) (= y 0))
	    (Rtyo 33) (Rprinc "H")
	    (setq X 0 Y 0))
	   ((and (< (+ (abs (- X x)) (abs (- Y y))) 4))
	    (cond ((< X x)
		 (do ex X (1+ ex) (= ex x) (Rtyo 33) (Rprinc "C")))
		((< x X)
		 (do ex x (1+ ex) (= ex X) (Rtyo 010))))
	    (cond ((< Y y)
		 (do wy Y (1+ wy) (= wy y) (Rtyo 33) (Rprinc "B")))
		((< y Y)
		 (do wy y (1+ wy) (= wy Y) (Rtyo 33) (Rprinc "A"))))
	    (setq X x Y y))
	   ;; Direct Cursor Addressing is best.
	   (t (setq X x Y y)
	      (Rtyo 33) (Rprinc "Y") (Rtyo (+ 40 y)) (Rtyo (+ 40 x)))))


;;; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33) (Rprinc "J"))


;;; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33) (Rprinc "K"))


;;; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (Rtyo 33) (Rprinc "P")
       (Rprinc str)
       (Rtyo 33) (Rprinc "Q")
       (setq X (+ X (stringlength str))))


;;; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (Rtyo 33) (Rprinc "P")
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "D"))
       (Rtyo 33) (Rprinc "Q"))


;;; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (Rtyo 33) (Rprinc "P")
       (do i 1 (1+ i) (> i n)
           (Rtyo 33) (Rprinc "B") (DCTL-pad 130.))
       (Rtyo 33) (Rprinc "Q")
       (setq X 0))


;;; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (Rtyo 33) (Rprinc "P")
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "A") (DCTL-pad 130.))
       (Rtyo 33) (Rprinc "Q")
       (setq X 0))


;;; Send pad characters to wait specified number of milliseconds
(defun DCTL-pad (n)
       (do i (1+ (// (* n ospeed) 1000.)) (1- i) (= i 0)
           (Rtyo 177)))
