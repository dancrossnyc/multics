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
;;;	I100 control package
;;;	CWH 3/5/79 from VT52ctl
;;;	BSG 3/21/78 from DD4000ctl
;;;

;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declarations so that it would compile without warnings.
;;;                                                      END HISTORY COMMENTS


(%include e-macros)
(declare (special X Y screenheight screenlinelen ospeed))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (*expr Rprinc Rtyo))

; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'infoton100)
       (DCTL-home-cursor)
       (DCTL-clear-rest-of-screen)
       (setq X 0 Y 0))

; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
	    (DCTL-home-cursor)
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
	      (Rtyo 33)(Rprinc "f")(Rtyo (+ 40 x))(Rtyo (+ 40 y))
	      )))

; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))

; Home cursor to upper left corner.
(defun DCTL-home-cursor ()
       (Rtyo 33) (Rprinc "H"))

; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33) (Rprinc "J"))

; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))

; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33) (Rprinc "L")
	 (DCTL-pad 100000.))) 

; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33) (Rprinc "M")
	 (DCTL-pad 100000.)))

; Send pad characters
(defun DCTL-pad (n)
       (do i (// (* n ospeed) 1000000.) (1- i) (= i 0)
	 (Rtyo 0)))

; Delete characters from current position in line.
; This won't work unless the terminal has the block mode option.
; Create a separate terminal type?
;(defun DCTL-delete-chars (n)
;       (do i 1 (1+ i)(> i n)
;           (Rtyo 33) (Rprinc "P")))

; Insert character string in line at current position.
; This won't work unless terminal has block mode option.
;(defun DCTL-insert-char-string (str)
;       (do i (stringlength str) (1- i) (= i 0)
;	 (Rtyo 33) (Rprinc "@"))
;      (Rprinc str)
;      (setq X (+ X (stringlength str))))

