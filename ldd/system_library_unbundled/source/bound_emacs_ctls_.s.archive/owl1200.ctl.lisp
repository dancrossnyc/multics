;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1981 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;;
;;;	OWL 1200 control pachage
;;;	Richard Lamson, 05/13/81 from TELERAY 1061 control package
;;;				GMP, 08/14/78 from VT52 package
;;;

(eval-when (compile eval) (setsyntax '/# 'macro 'sharp-macro)

(defun sharp-macro ()
       (let ((ch (tyi)))
	  (or (= ch 57)			; #/ is the only # macro here
	      (error "Unknown # character: " (ItoC ch) 'fail-act))
	  (tyi)))				; return character number

)

(declare (special X Y screenheight screenlinelen ospeed %DCTL-escape-char))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t
             screenheight 24. screenlinelen 79.
             tty-type 'teleray1061
             X -777 Y -777
	   %DCTL-escape-char 33)
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))


;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X) (= y Y))
	    nil)
	   ((and (= x 0) (= y 0))
	    (Rtyo %DCTL-escape-char) (Rtyo #/H)
	    (setq X 0 Y 0))
	   (t				; must actually set X and Y
	       (cond ((= x (1- X)) (Rtyo 10))
		   ((= (1+ x) (1- X)) (Rtyo 10) (Rtyo 10))
		   ((= X (1- x)) (Rtyo %DCTL-escape-char) (Rtyo #/C))
		   (t (Rtyo %DCTL-escape-char) (Rtyo #/Y) (Rtyo y)))
	       (cond ((= y (1- Y)) (Rtyo %DCTL-escape-char) (Rtyo #/A))
		   ((= Y (1- y)) (Rtyo %DCTL-escape-char) (Rtyo #/B))
		   (t (Rtyo %DCTL-escape-char) (Rtyo #/X) (Rtyo y)))
	       (setq X x Y x))))


;;; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo %DCTL-escape-char) (Rtyo #/J) (DCTL-pad 132.))


;;; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo %DCTL-escape-char) (Rtyo #/K) (DCTL-pad 6.))


;;; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (let ((stringlength (stringlength str)))
	  (cond ((= 0 stringlength))
	        (t
		  (do i 1 (1+ i) (= i stringlength)
		      (Rtyo %DCTL-escape-char) (Rtyo #/N) (Rprinc (substr str i 1)))
		  (setq X (+ X stringlength))))))


;;; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo %DCTL-escape-char) (Rtyo #/O)))


;;; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo %DCTL-escape-char) (Rtyo #/L))
       (DCTL-pad (* 6. n))
       (setq X 0))


;;; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo %DCTL-escape-char) (Rtyo #/M))
       (DCTL-pad (* 6. n))
       (setq X 0))


;;; Send pad characters to wait specified number of milliseconds
(defun DCTL-pad (n)
       (do i (1+ (// (* n ospeed) 1000.)) (1- i) (= i 0)
           (Rtyo 0)))
