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
;;;       Delta Data 4000 control package
;;;       BSG 3/78
;;;       Modified by EAK 3/18/78
;;;	Consing removed in recognition of output buffering, BSG 8/31/78

(declare (special xconses yconses DCTLV-escf X Y screenheight ospeed screenlinelen tty-eolch-lossp))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))

;;;
;;;	Whoever invented the **** eol frobs that caused me to have to
;;;	propagate tty-eolch-lossp throughout n levels of hair,
;;;	him, his fingers should be cut off.

; Initialize terminal and terminal control package.
(defun DCTL-init ()
;      (setq idel-lines-availablep (= ospeed 1200.))
;      (setq idel-chars-availablep (< ospeed 1200.))
       (setq idel-lines-availablep t idel-chars-availablep nil)
			; This seems to be the most popular menu of poisons.
       (setq tty-eolch-lossp idel-lines-availablep)
       (setq DCTLV-escf (catenate (ascii 33) 'F))
       (setq screenheight 25. screenlinelen 79.)
       (setq tty-type 'dd4000)
       (Rtyo 33) (Rprinc "E")
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 33)(Rprinc "H")
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 33)(Rprinc "C")(DCTL-pad 2500.)))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 33)(Rprinc "D")(DCTL-pad 2500.))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 33)(Rprinc "B")(DCTL-pad 2500.)))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "A")(DCTL-pad 2500.))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (DCTL-pad 3000.)
	      (Rprinc DCTLV-escf)
                (Rtyo (+ 60 (// x 100.)))(setq x (\ x 100.))
                (Rtyo (+ 60 (// x 10.)))(setq x (\ x 10.))
                (Rtyo (+ 60 x))

                (Rtyo (+ 60 (// y 100.)))(setq y (\ y 100.))
                (Rtyo (+ 60 (// y 10.)))(setq y (\ y 10.))
                (Rtyo (+ 60 y))

	      (DCTL-pad 5000.)
                    )))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (DCTL-pad 9000.)
       (Rtyo 33)(Rprinc "J")(DCTL-pad 15000.))


; Clear to end of line.
(defun DCTL-kill-line ()
       (DCTL-pad 10000.)
       (Rtyo 33)(Rprinc "K")
       (DCTL-pad 7500.))


; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (Rtyo 33)(Rprinc "Q")
       (Rprinc str)
       (Rtyo 33)(Rprinc "R")
       (setq X (+ X (stringlength str))))


; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "P")(DCTL-pad 2500.)))


; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "L")(DCTL-pad 2500.)))


; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "M")(DCTL-pad 100000.)))


; Send pad characters to wait specified no. of microseconds.
(defun DCTL-pad (n)
       (do i (// (* n ospeed) 1000000.) (1- i) (= i 0)
           (Rtyo 0)))

; Random underscore to turn off losing DD features at high speeds.

(defun idel-off ()
       (setq idel-lines-availablep nil idel-chars-availablep nil))
