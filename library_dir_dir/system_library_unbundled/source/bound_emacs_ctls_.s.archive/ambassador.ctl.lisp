;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Bull Inc., 1988                *
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;               AnnArbor Ambassador Controller


;;; HISTORY COMMENTS:
;;;  1) change(88-02-02,Schroth), approve(), audit(), install():
;;;     Pre-hcom journal.
;;;               AnnArbor Ambassador pkg --- BSG 1/26/81
;;;               Tavares' Redisplay hacking features of 1/6/81 added, too.
;;;  2) change(88-02-02,Schroth), approve(88-02-29,MCR7852),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Updated to make set-screen-size compatible with split screen.
;;;                                                      END HISTORY COMMENTS


(%include emacs-rdis-dcls)

(declare (special X Y screenheight screenlinelen ospeed DCTL-csistring))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep))
(declare (special given-tty-type DCTL-dcl-screen-height))
(declare (special modelwindow))
(declare (*expr DCTL-standard-set-screen-size Rprinc Rtyo
	      reset-minibuffer-size wman-init))

;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t
             screenlinelen 79. tty-type 'ambassador
	   DCTL-prologue-availablep t DCTL-epilogue-availablep t
             X -777 Y -777)
       (setq DCTL-dcl-screen-height
	   (or (cdr (assq given-tty-type
		        '((ambassador . 30.) (ambassador_24l . 24.)
		          (ambassador_30l . 30.) (ambassador_48l . 48.)
			(ambassador_60l . 60.))))
	       30.))
       (setq screenheight DCTL-dcl-screen-height)
       (setq DCTL-csistring (catenate (ascii 33) "["))
       (DCTL-prologue)
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))



(defun DCTL-prologue ()
       (DCTL-csi1 49. "Q")			;Make ICH/DCH win
       (DCTL-csiprinc ">30l")			;RM ZDBM, make ^H win
       (DCTL-set-hw-screen-size screenheight))

(defun DCTL-epilogue ()
       (DCTL-set-hw-screen-size DCTL-dcl-screen-height))

;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X) (= y Y)) nil)	;gudenov
	   ((and (= x 0) (= y 0))
	    (DCTL-csiprinc "H")
	    (setq X 0 Y 0))
	   ((or (< X 0)(< Y 0))
	    (DCTL-position-cursor 0 0)
	    (DCTL-position-cursor x y))
	   ((= y Y)			;get away with "HPA"!
	    (let ((d (- x X)))
	         (cond ((= x 0)(Rtyo 15))		;CR
		     ((and (> d 0)(< d 10.))	;HPR will do
		      (DCTL-csi1 d "a"))	;HPR
		     ((and (< d 0)(< (- d) 10.))
		      (setq d (- d))
		      (cond ((< d 5)
			   (do i X (1- i)(= i x)(Rtyo 10)))	;^H
			  (t (DCTL-csi1 d "D"))))
		     (t (DCTL-csi1 (1+ x) "`"))))	;HPA
	    (setq X x))
	   ((= x X)			;get away with "VPA"!
	    (let ((d (- y Y)))
	         (cond ((and (> d 0)(< d 10.))	;VPR will do
		      (cond ((< d 5)
			   (do i Y (1+ i)(= i y)(Rtyo 12)))     ;LF
			  (t (DCTL-csi1 d "e"))))	;VPR
		     ((and (< d 0)(< (- d) 10.))   ;CUU
		      (setq d (- d))
		      (cond ((= d 1)(Rtyo 33)(Rprinc "M"))
			  (t (DCTL-csi1 d "A"))))
		     (t (DCTL-csi1 (1+ y) "d"))))	;VPA
	    (setq Y y))
	   ((= x 0)			;CNL/CPL
	    (DCTL-csi1 (abs (- y Y))
		     (cond ((> y Y) "E")	;CNL
			 (t "F")))	;CPL
	    (setq Y y X 0))
	   ((and (< (+ (abs (- X x)) (abs (- Y y))) 3))
	    (DCTL-position-cursor x Y)
	    (DCTL-position-cursor x y))
	   ;; Direct Cursor Addressing is best.
	   (t (setq X x Y y)
	      (DCTL-csi1 (1+ y) ";")
	      (DCTL-outANSIdec (1+ x))
	      (Rprinc "H"))))

;;; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (DCTL-csi1 0 "J"))

;;; Clear to end of line.
(defun DCTL-kill-line ()
       (DCTL-csi1 0 "K"))


;;; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (DCTL-csi1 (stringlength str) "@")
       (Rprinc str)
       (setq X (+ X (stringlength str))))


;;; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (DCTL-csi1 n "P"))


;;; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (DCTL-csi1 n "L")
       (setq X 0))


;;; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (DCTL-csi1 n "M")
       (setq X 0))


;;; Encoding cruft 1/26/81

(defun DCTL-csi1 (n stuff)
       (Rprinc DCTL-csistring)
       (DCTL-outANSIdec n)
       (Rprinc stuff))

(defun DCTL-outANSIdec (n)
       (cond ((or (= n 0)(= n 1)))
	   (t (DCTL-outANSIdec-recurse n))))

(defun DCTL-outANSIdec-recurse (n)
       (cond ((> n 9)(DCTL-outANSIdec-recurse (// n 10.))))
       (Rtyo (+ (CtoI "0") (\ n 10.))))

(defun DCTL-csiprinc (stuff)
       (Rprinc DCTL-csistring)
       (Rprinc stuff))



;;; Send pad characters to wait specified number of milliseconds
(defun DCTL-pad (n)
       (do i (1+ (// (* n ospeed) 1000.)) (1- i) (= i 0)
           (Rtyo 0)))

;;;; Tavares' hack


;;; the following hack enables the terminal to change its ACTUAL hardware
;;; screen size in response to the set-screen-size command.
;;; (Should work in supdup or emacs, nobody will call set-screen-size,
;;; fixed not to use decimal-rep -BSG 1/26/81)
;;; first, copy the REAL set-screen-size somewhere safe.

(putprop 'DCTL-standard-set-screen-size (get 'set-screen-size 'subr) 'subr)

(defun set-screen-size (screen-size)

;;; find the proper hardware screen size

       (setq screenheight
	   (cond ((> screen-size 48.) 60.)
	         ((> screen-size 40.) 48.)
	         ((> screen-size 36.) 40.)
	         ((> screen-size 30.) 36.)
	         ((> screen-size 28.) 30.)
	         ((> screen-size 26.) 28.)
	         ((> screen-size 24.) 26.)
 	         ((> screen-size 20.) 24.)
	         ((> screen-size 18.) 20.)
	         (t 18.)))

        (DCTL-set-hw-screen-size screenheight)

;;; wipe out the dregs of the obsolete mode line and minibuffer, etc.

        (DCTL-position-cursor 0 (startline modelwindow))
        (DCTL-clear-rest-of-screen)

;;; if the new screenheight is higher than ever, grow emacs' arrays.

        (cond ((> screenheight (cadr (arraydims screen)))
	     ;; The following 'strange' *rearray is used as we must keep both
	     ;; an array pointer and a named array reference up-to-date.
	     (setq screen      (*rearray 'screen t screenheight))
	     (setq eline-conts (*rearray 'eline-conts t screenheight))
	     (setq newscreen   (*rearray 'newscreen t screenheight))))

;;; perform the standard emacs action on set-screen-size

        (DCTL-standard-set-screen-size screen-size)

;;; make emacs recompute where to put the mode line, etc.

        (wman-init)
        (reset-minibuffer-size)

;;; force terminal to return screen to top of page

        (setq X -777 Y -777)			;randomize, force output
        (DCTL-position-cursor 0 0)))



(defun DCTL-set-hw-screen-size (size)
       (DCTL-csiprinc "60;;;")
       (DCTL-outANSIdec size)
       (Rprinc "p")
       (setq X 0 Y 0))			; hw also homes cursor
