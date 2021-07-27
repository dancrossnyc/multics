;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1983 *
;;; *                                                         *
;;; ***********************************************************
;;; -*-LISP-*-

;;;
;;;	Bull DKU7102 CTL package
;;;	6 May 83 by G.Sauvagnat for DKU7102 (SDP mode).
;;;

;;; Include
(%include e-macros)

;;; Declarations
(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep))
(declare (special DCTL-insert-mode-on))
(declare (*expr Rprinc Rtyo))

;;; Output n to the terminal in decimal.
(defun DCTL-outdec (n)			;BSG 3/23/79
       ((lambda (have-output)
	      (do digi '(1000. 100. 10. 1) (cdr digi) (null digi)
		((lambda (rem)
		         (cond ((or have-output (> rem 0) (= (car digi) 1))
			      (Rtyo (+ 60 rem))
			      (setq have-output t)))
		         (setq n (\ n (car digi))))
		 (// n (car digi)))))
        nil))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'dku7102)
       (DCTL-prologue)
       (Rtyo 33)(Rprinc "[2J")		; Effacement de l'ecran
       (Rtyo 33)(Rprinc "[H")			; Positionnement C1 L1
       (setq X 0 Y 0))



;;; Prologue code
(defun DCTL-prologue ()
       (Rtyo 33) (Rprinc "[?=h")		; Passage en mode SDP
       (DCTL-set-insert-mode nil)
       (Rtyo 33) (Rprinc "[=l")		; Passage en mode PAGE


;;; Epilogue code
(defun DCTL-epilogue ()
       (setq DCTL-insert-mode-on nil)
       (Rtyo 33) (Rprinc "c"))		; Reset Initial State (RIS)



;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
					;(redf y)
       (cond ((and (= x X)(= y Y))
              nil)
             ((and (= x 0)(= y 0))
              (Rtyo 33)(Rprinc "[H")
              (setq X 0 Y 0))
             ((and (< (+ (abs (- X x))(abs (- Y y))) 4))
              (cond ((< X x)
                     (do ex X (1+ ex)(= ex x)(Rtyo 33)(Rprinc "[C")))
                    ((< x X)
                     (do ex x (1+ ex)(= ex X)(Rtyo 33)(Rprinc "[D"))))
              (cond ((< Y y)
                     (do wy Y (1+ wy)(= wy y)(Rtyo 33)(Rprinc "[B")))
                    ((< y Y)
                     (do wy y (1+ wy)(= wy Y)(Rtyo 33)(Rprinc "[A"))))
              (setq X x Y y))
;; Direct Cursor Addressing is best.
             (t (setq X x Y y)
	      (Rtyo 33)(Rprinc "[")(DCTL-outdec (1+ y))(Rprinc ";")(DCTL-outdec (1+ x))(Rprinc "f")
                    )))



;;; Output string.
(defun DCTL-display-char-string (string)
       ((lambda (strx)
	      (cond ((= strx 0))		;bug in redisplay calls with no string
		  (t (DCTL-set-insert-mode nil)
;		     (cond ((< 19. Y) (Rtyo 33)(Rprinc "[2;7m")))
		     (Rprinc string)
		     (setq X (+ X strx)))))
        (stringlength string)))


;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "[0J"))


;;; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "[0K"))



;;; Insert lines
(defun DCTL-insert-lines (n)
	 (Rtyo 33)(Rprinc "[")(DCTL-outdec n)(Rprinc "L")))


;;; Delete lines
(defun DCTL-delete-lines (n)
	 (Rtyo 33)(Rprinc "[")(DCTL-outdec n)(Rprinc "M")))


;;; Insert character string
(defun DCTL-insert-char-string (str)
       (DCTL-set-insert-mode t)
       (Rprinc str)
       (setq X (+ X (stringlength str))))


;;; Delete characters
(defun DCTL-delete-chars (n)
	 (Rtyo 33)(Rprinc "[")(DCTL-outdec n)(Rprinc "P"))


;;; Mode insertion
(defun DCTL-set-insert-mode (bit)
       (if bit				; on le veut on
	 (if DCTL-insert-mode-on		; ne rien faire
	     else
	     (setq DCTL-insert-mode-on t)
	     (Rtyo 33) (Rprinc "[4h"))
	 else
	 (if (not DCTL-insert-mode-on)
	     else
	     (setq DCTL-insert-mode-on nil)
	     (Rtyo 33) (Rprinc "[4l"))))


;;; Inverse video?
;(defun redf (y)
;       (cond ((< 19. Y) nil)
;	   ((and (> 20. Y)(> 20. y)) nil)
;	   (t (Rtyo 33)(Rprinc "[0m"))))
