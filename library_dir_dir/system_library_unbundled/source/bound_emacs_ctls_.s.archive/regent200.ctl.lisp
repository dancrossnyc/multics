;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; ***********************************************************
;;; -*-LISP-*-

;;;
;;;	ADDS Regent 200 ctl
;;;       Ripped off from VIP7800 ctl 02/15/80 by CDT
;;;

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep DCTL-insert-mode-on))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'regent200)
       (Rtyo 33)(Rprinc "s")(Rtyo 14)
       (setq X 0 Y 0)
       (DCTL-prologue))


;;; Prologue code
(defun DCTL-prologue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 14)
       (setq X 0 Y 0))

;;; Epilogue code
(defun DCTL-epilogue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 33)(Rprinc "s")(Rtyo 14))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 6)))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 25))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 32))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 33)(Rprinc "Y")(Rtyo (+ 37 y))(Rtyo (+ 37 x))
                    )))


;;; Output string.
(defun DCTL-display-char-string (string)
       ((lambda (strx)
	      (cond ((= strx 0))		;bug in redisplay calls with no string
		  (t (cond (DCTL-insert-mode-on
			   (setq DCTL-insert-mode-on nil)
			   (Rtyo 33) (Rprinc "F")))
		     (Rprinc string)
		     (setq X (+ X strx)))))
        (stringlength string)))
	      

; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "k"))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))

(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rprinc "M")))

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rprinc  "l")))

(defun DCTL-insert-char-string (str)
       (cond (DCTL-insert-mode-on)
	   (t
	     (setq DCTL-insert-mode-on t)
	     (Rtyo 33) (Rprinc "F")))
       (Rprinc str)
       (setq X (+ X (stringlength str))))
       

(defun DCTL-delete-chars (n)
       (do i 0 (1+ i)(= i n)
	 (Rtyo 33)(Rprinc "E")))
