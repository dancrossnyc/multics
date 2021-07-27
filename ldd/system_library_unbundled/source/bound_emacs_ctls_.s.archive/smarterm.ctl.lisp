;;; *****************************************************
;;; *                                                   *
;;; * Copyright (C) 1983 by Massachusetts Institute of  *
;;; * Technology and Honeywell Information Systems Inc. *
;;; *                                                   *
;;; *****************************************************
;;;
;;; -*-LISP-*-

;;;
;;;	Netronics Smarterm-80 control package
;;;       Ripped off from iq120.ctl.lisp by Alberto Magnani 12/3/83.
;;;       
 
(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (*expr Rprinc Rtyo))

; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'smarterm)
       (Rtyo 14)
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
	    (Rtyo 32)                         
              (setq X 0 Y 0))
             ((and (= x 0)(= y Y))
              (Rtyo 15)                     
              (setq X 0 Y y))
             ((and (= x 0)(< (abs(- Y y)) 3))
              (Rtyo 15)
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 13))))
              (setq X 0 Y y))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 1)))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 10))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 13))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
                (Rtyo 33) (Rprinc "=")(Rtyo (+ 40 y))(Rtyo (+ 40 x))
                    )))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "Y"))


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "T"))


; Inserting/deleteing lines
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "E")))

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "R")))


; Inserting/deleteing characters
(defun DCTL-insert-char-string (str)
       (Rtyo 33)(Rprinc "N")
       (Rprinc str)
       (Rtyo 33)(Rprinc "M")
       (setq X (+ X (stringlength str))))

(defun DCTL-delete-chars (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "W")))

; That's it guys.
