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
;;;       HP2645 control package
;;;       EAK 3/18/78
;;;

(declare (special xconses yconses escfxconsesyconses X Y screenheight ospeed tty-type))
(declare (special idel-lines-availablep idel-chars-availablep screenlinelen))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq xconses (list nil nil))
       (setq yconses (list nil nil))
       (setq escfxconsesyconses (nconc (list (ascii 33) '& 'a)
                                       xconses (list 'c)
                                       yconses (list 'R)))
       (setq screenheight 24.)                             ; 20 lines for editing
       (setq screenlinelen 79.)
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq tty-type 'hp2645)
       (Rtyo 33) (Rprinc "H")                           ; clear screen: home,
       (Rtyo 33) (Rprinc "J")			; and erase to end
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (prog (ycost				; cost of y and x relative
	      xcost				; movement
	      what				; which movement is best
	      cost)				; cost of that movement
	     (and (= x X)(= y Y)		; return right away if already
		  (return nil))			; at desired position
	     (setq what 1			; 1: "home and relative move"
		   cost (+ 2 y x x))		; cost is V + 2H + 2
	     (and (> cost 9)			; direct cursor address better?
		  (setq what 0			; 0: "direct cursor address"
			cost 9))		; cost is 9 characters
	     (setq ycost (- y Y))
	     (and (< ycost 0)
		  (setq ycost (* (- ycost) 2)))
	     (setq xcost (- X x))
	     (and (< xcost 0)
		  (setq xcost (* (- xcost) 2)))
	     (and (< (+ ycost xcost) cost)
		  (setq what 3			; 3: "relative move"
			cost (+ ycost xcost)))
	     (and (< (+ 1 ycost x x) cost)
		  (setq what 2))		; 2: "CR and relative move"
	     (cond ((= what 0)

; Direct Cursor Address

		    (rplaca xconses (+ 60 (// x 10.)))
		    (rplaca (cdr xconses) (+ 60 (\ x 10.)))

		    (rplaca yconses (+ 60 (// y 10.)))
		    (rplaca (cdr yconses) (+ 60 (\ y 10.)))

		    (Rprinc (implode escfxconsesyconses))
		    (setq X x Y y)
		    (return nil))

		   ((= what 1)			; home and relative move?
		    (Rtyo 33)(Rprinc "H")	; home
		    (setq X 0 Y 0))		; keep track of cursor
						; fall through to relative move

		   ((= what 2)			; CR and relative move?
		    (Rtyo 15)			; CR
		    (setq X 0)))		; keep track of cursor
						; fall through to relative move

; Relative Move

	     (cond ((< X x)
		    (do ex X (1+ ex)(= ex x)(Rtyo 33)(Rprinc "C")))
		   ((< x X)
		    (do ex x (1+ ex)(= ex X)(Rtyo 10))))
	     (cond ((< Y y)
		    (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
		   ((< y Y)
		    (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "A"))))
	     (setq X x Y y)
	     (return nil)))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "J"))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))


; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (Rtyo 33)(Rprinc "Q")
       (Rprinc str)
       (Rtyo 33)(Rprinc "R")
       (setq X (+ X (stringlength str))))


; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "P")(DCTL-pad 7000.)))


; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "L")))


; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "M")))


; Send pad characters to wait specified no. of microseconds.
(defun DCTL-pad (n)
       (do i (// (* n ospeed) 1000000.) (1- i) (= i 0)
           (Rtyo 0)))
