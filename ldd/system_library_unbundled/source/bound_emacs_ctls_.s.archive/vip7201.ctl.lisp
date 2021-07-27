;;; -*-LISP-*-

;;;
;;;	HISI VIP7201 control package
;;;       Ripped off from VIP7800ctl  MBA 82-6-3 (!)
;;;

(%include e-macros)

(declare (special X Y ospeed screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep DCTL-insert-mode-on))
(declare (*expr Rprinc Rtyo))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-chars-availablep t
	   idel-lines-availablep (not (> ospeed 240.))) ; painfully slow idel-lines
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'vip7201)
       (setq X 0 Y 0)
       (DCTL-prologue)
	)


;;; Prologue code
(defun DCTL-prologue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 33) (Rprinc "`"))


;;; Epilogue code
(defun DCTL-epilogue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 33) (Rprinc "`"))

; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 33)(Rprinc "H")
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 33)(Rprinc "C")))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 33)(Rprinc "D"))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 33)(Rprinc "B")))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "A"))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 33)(Rprinc "f")(Rtyo (+ 40 x))(Rtyo (+ 40 y))
                    )))


;;; Output string.
(defun DCTL-display-char-string (string)
       ((lambda (strx)
	      (cond ((= strx 0))		;bug in redisplay calls with no string
		  (t (cond (DCTL-insert-mode-on
			   (setq DCTL-insert-mode-on nil)
			   (Rtyo 33) (Rprinc "[J")))
		     (Rprinc string)
		     (setq X (+ X strx)))))
        (stringlength string)))
	      

; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "J")
       (Rtyo 0))  ;needed only at 9.6kb


; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "K"))


(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rprinc "[L")))

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
	     (Rtyo 33)(Rprinc  "[M")))

(defun DCTL-insert-char-string (str)
       (cond (DCTL-insert-mode-on)
	   (t
	     (setq DCTL-insert-mode-on t)
	     (Rtyo 33) (Rprinc "[I")))
       (Rprinc str)
       (setq X (+ X (stringlength str))))
       

(defun DCTL-delete-chars (n)
       (do i 0 (1+ i)(= i n)
	 (Rtyo 33)(Rprinc "[P")))
