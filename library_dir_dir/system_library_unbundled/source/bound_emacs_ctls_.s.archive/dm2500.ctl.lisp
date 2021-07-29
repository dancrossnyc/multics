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
;;;       Data Media 2500 control package
;;;       EAK 3/27/78
;;;

(declare (special dcaconses X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t
             idel-chars-availablep t)
       (setq dcaconses (list (ascii 14) nil nil))
       (setq screenheight 24.                              ; 20 lines for editing
             screenlinelen 79.)
       (setq tty-type 'dm2500)
       (DCTL-clear-screen))                             ; clear whole screen


;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 2)
              (setq X 0 Y 0))
;;; Direct Cursor Addressing is best.
             (t (rplaca (cdr dcaconses) (boole 6 x 140))
                (rplaca (cddr dcaconses) (boole 6 y 140))
                (Rprinc (implode dcaconses))
                (setq X x Y y))))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear whole screen.
(defun DCTL-clear-screen ()
       (Rtyo 36)
       (setq X 0 Y 0))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
   ((lambda (x y)
       (do i Y (1+ i) (= i (1- screenheight))
           (Rprinc (catenate (ascii 27) (ascii 15) (ascii 12)))
	 (setq X 0 Y (1+ Y)))
       (Rtyo 27)
       (DCTL-position-cursor x y))
    X Y))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 27))


; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (Rtyo 20)
       (do i (stringlength str) (1- i) (not (> i 0))
           (Rtyo 34))
       (Rtyo 30)
       (Rprinc str)
       (setq X (+ X (stringlength str))))


; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (Rtyo 20)
       (do i 1 (1+ i)(> i n)
           (Rtyo 10))
       (Rtyo 30))


; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (Rtyo 20)
       (do i 1 (1+ i)(> i n)
           (Rtyo 12))
       (Rtyo 30))


; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (Rtyo 20)
       (do i 1 (1+ i)(> i n)
           (Rtyo 32))
       (Rtyo 30))
