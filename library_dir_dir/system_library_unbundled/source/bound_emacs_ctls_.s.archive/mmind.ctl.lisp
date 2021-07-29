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
;;;       Micromind Ctl
;;;	From DD4000 5/18/78 BSG
;;;	Modified 3/23/79 JSL
;;;

(declare (special X Y screenheight screenlinelen ospeed rdis-whitespace-optimize))
(declare (special idel-lines-availablep idel-chars-availablep tty-type overstrike-availablep))
(declare (defpl1 e_pl1_$get_mcs_tty_info "" (return bit (1))(return float bin)(return fixed bin)
	(return float bin)(return fixed bin)(return fixed bin)(return fixed bin)))

; Initialize terminal and terminal control package.

(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t overstrike-availablep t)
       (setq rdis-whitespace-optimize nil)
       (setq tty-type 'micromind)
       (setq screenlinelen (1- (caddr (cddddr (e_pl1_$get_mcs_tty_info)))) screenheight 34.)
       (setq X -777 Y -777)   ; N.B. ^L does this so we should be able to handle it.
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))

; Move terminal's cursor to desired position.

(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y)) nil)		; we're there...do nothing
	   ((and (= x 0)(= y Y))(Rtyo 15))	; just go to beginning of current line
	   ((and (= x 0)(= y 0))(Rtyo 33)(Rprinc "H")(DCTL-pad (* 25000. (min (abs Y) screenheight)))) ; go home.
	   ((and (< (+ (max (min (- X x)(1+ (* 2 x)))(* 2 (- x X)))(max (- y Y)(* 2 (- Y y)))) (cond ((< X 95.) 4)(t 8))))
	    (and (< (1+ (* 2 x)) (- X x)) (setq X 0) (Rtyo 15))	; yes. do carriage return if faster.
	    (cond ((< X x)
		 (do ex X (1+ ex)(= ex x)(Rtyo 33)(Rprinc "C")))	; move right
		((< x X)
		 (do ex x (1+ ex)(= ex X)(Rtyo 10))))	; move left
	    (cond ((< Y y)
		 (do wy Y (1+ wy)(= wy y)(Rtyo 12)(DCTL-pad 25000.)))  ; move down
		((< y Y)
		 (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "A")(DCTL-pad 25000.)))))	; move up
	   ((< x 95.)(Rtyo 33)(Rprinc "Y")	; use abs. cursor address. short form?
		   (Rtyo (+ y 41))
		   (Rtyo (+ x 40))
		   (DCTL-pad (* 25000. (min (abs (- y Y)) screenheight))))
	   (t (Rtyo 33) (Rprinc "F")		; no. use long form.
	      (Rtyo (+ 60 (// x 100.)))
	      (Rtyo (+ 60 (\ (// x 10.) 10.)))
	      (Rtyo (+ 60 (\ x 10.)))
	      (Rtyo (+ 60 (// y 100.)))
	      (Rtyo (+ 60 (\ (// y 10.) 10.)))
	      (Rtyo (+ 60 (\ y 10.)))
	      (DCTL-pad (* 25000. (min (abs (- y Y)) screenheight)))))
       (setq X x Y y))

; Output string.

(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))

; Clear to end of screen.

(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "J")
       (DCTL-pad (* 50000. (- screenheight Y))))

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
           (Rtyo 33)(Rprinc "P")))

; Insert n blank lines at current position.

(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "L")(DCTL-pad 50000.)))

; Delete n lines at current position.

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "M")(DCTL-pad 50000.)))

; Send pad characters to wait specified no. of microseconds.

(defun DCTL-pad (n)
       (do i (// (* n ospeed) 1000000.) (1- i) (= i 0)
	 (Rtyo 0)))
