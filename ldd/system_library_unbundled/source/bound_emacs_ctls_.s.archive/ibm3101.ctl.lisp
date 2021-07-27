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
;;;	IBM 3101 control package
;;;	 Coded:  29 October 1979 by GMP
;;;

(declare (special given-tty-type tty-type ospeed idel-lines-availablep idel-chars-availablep
	        screenheight screenlinelen X Y))

;;; Initialize terminal and terminal control package
(defun DCTL-init ()
       (setq screenheight 24. screenlinelen 79.)
       (cond ((eq given-tty-type 'ibm3101_2x)	;has insert/delete line/character
	    (setq idel-lines-availablep t idel-chars-availablep t))
	   (t				;assume it doesn't have them
	    (setq idel-lines-availablep nil idel-chars-availablep nil)))
       (setq tty-type 'ibm3101)
       (Rtyo 33) (Rprinc "H") (Rtyo 33) (Rprinc "J")   ;home and clear screen
       (setq X 0 Y 0))

;;; Position terminal's cursor to desired position
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X) (= y Y)) nil)	;already in correct position
             ((and (= x 0) (= y 0))		;wants to home the cursor
              (Rtyo 33) (Rprinc "H")
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x)) (abs (- Y y))) 4))	;can use relative motion
              (cond ((< X x)
                     (do ex X (1+ ex) (= ex x) (Rtyo 33) (Rprinc "C")))
                    ((< x X)
                     (do ex x (1+ ex) (= ex X) (Rtyo 010))))
              (cond ((< Y y)
                     (do wy Y (1+ wy) (= wy y) (Rtyo 33) (Rprinc "B")))
                    ((< y Y)
                     (do wy y (1+ wy) (= wy Y) (Rtyo 33) (Rprinc "A"))))
              (setq X x Y y))
             (t (setq X x Y y)		;direct cursor addressing is the right thing
	      (Rtyo 33) (Rprinc "Y") (Rtyo (+ 40 y)) (Rtyo (+ 40 x)))))


;;; Output the given string
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


;;; Clear to end of screen
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33) (Rprinc "J"))


;;; Clear to end of line
(defun DCTL-kill-line ()
       (Rtyo 33) (Rprinc "I"))


;;; Insert character string in line at current position
(defun DCTL-insert-char-string (string)
       (do i 1 (1+ i) (> i (stringlength string))
	 (Rtyo 33) (Rprinc "P") (Rprinc (substr string i 1))
	 (DCTL-pad 100))
       (setq X (+ X (stringlength string))))


;;; Delete characters from the current position in the line
(defun DCTL-delete-chars (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "Q") (DCTL-pad 100)))


;; Insert blank lines at the current position
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "N") (DCTL-pad 100))
       (setq X 0))


;;; Delete lines at current position
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33) (Rprinc "O") (DCTL-pad 100))
       (setq X 0))


;;; Pad for specified number of milliseconds
(defun DCTL-pad (n)
       (do i (1+ (// (* n ospeed) 1000.)) (1- i) (= i 0)
	 (Rtyo 177)))
