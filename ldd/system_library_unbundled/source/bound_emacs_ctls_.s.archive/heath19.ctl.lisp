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
;;;	H19 control package
;;;	BSG 3/21/78 from DD4000ctl
;;;	CAH 7/18/79 from vt52ctl
;;;	WMY 8/27/80 to add insert-mode stuff
;;;       AEB 9/17/80 Added delays to delete/insert lines and delete chars
;;;

(declare (special X Y ospeed screenheight screenlinelen))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep
	        DCTL-insert-mode-on))

; Initialize terminal and terminal control package.

(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'h19)
       (Rtyo 33)(Rprinc "H")(Rtyo 33)(Rprinc "J")
       (setq X 0 Y 0)
       (DCTL-prologue))

;;; Prologue
(defun DCTL-prologue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 33) (Rprinc "O"))	; turn off insert-mode

;;; Epilogue
(defun DCTL-epilogue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 33) (Rprinc "O"))

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
                     (do ex x (1+ ex)(= ex X)(Rtyo 010))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 33)(Rprinc "B")))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "A"))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 33)(Rprinc "Y")(Rtyo (+ 40 y))(Rtyo (+ 40 x))
                    )))


; Output string.

(defun DCTL-display-char-string (string)
       (cond (DCTL-insert-mode-on
	     (setq DCTL-insert-mode-on nil)
	     (Rtyo 33) (Rprinc "O")))
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.

(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "J"))


; Clear to end of line.

(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))

; Insert character string at current position.

(defun DCTL-insert-char-string (str)
       (cond ((not DCTL-insert-mode-on)
	    (setq DCTL-insert-mode-on t)
	    (Rtyo 33)(Rprinc "@")))
       (Rprinc str)
       (let ((len (stringlength str)))
	  (DCTL-pad (* len 1050.))
	  (setq X (+ X len))))

;;; Delete characters from current position in line.

(defun DCTL-delete-chars (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "N"))
	 (DCTL-pad (* n 2900.)))

;;; Insert n blank lines at current position.

(defun DCTL-insert-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "L") (DCTL-pad 24000.))
       (setq X 0))

;;; Delete n lines at current position.

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "M") (DCTL-pad 24000.))
       (setq X 0))

; Send pad characters

(defun DCTL-pad (n)
       (do i (// (* n ospeed) 1000000.) (1- i) (= i 0)
	 (Rtyo 0)))
