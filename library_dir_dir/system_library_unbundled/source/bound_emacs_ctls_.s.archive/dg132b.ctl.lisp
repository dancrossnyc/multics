;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        DatagraphiX 132B emacs control package               ;;
;;        created 22 February 1979 by Lee A. Newcomb, HIS, FSO ;;
;;        modified VIP 7800 controller.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declarations so that it would compile without warnings.
;;;                                                      END HISTORY COMMENTS


(%include e-macros)

(declare (special X Y screenheight screenlinelen tty-type))
(declare (special idel-lines-availablep idel-chars-availablep))
(declare (special given-tty-type))
(declare (array* (notype (dg132b-posit ?))))

(declare (*expr Rprinc Rtyo))

; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq screenlinelen 131.)
       (setq screenheight
	   (cond ((eq given-tty-type 'dg132b) 30.)	;30 lines for the screen
	         ((eq given-tty-type 'dg132b120) 120.)     ; user has the full terminal memory option
	         ((eq given-tty-type 'dg132b60) 60.)))     ; user has the default terminal memory
       (setq tty-type 'dg132b)
       (Rtyo 33)(Rprinc "H")			;clear/home cursor
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y))
	    nil)				;cursor already at <x,y>
	   ((and (= x 0)(= y 0))
	    (Rtyo 33)(Rprinc "T")
	    (setq X 0 Y 0))			;go home
	   ((and (< (+ (cond ((< x X) (- X x))
			 (t (* 2 (- x X))))
		     (cond ((< Y y) (- y Y))
			 (t (* 2 (- Y y))))) 8))
	    (cond ((< X x)
		 (do ex X (1+ ex)(= ex x) (Rtyo 33) (Rprinc "L")))
		((< x X)
		 (do ex x (1+ ex)(= ex X) (Rtyo 10))))
	    (cond ((< Y y)
		 (do wy Y (1+ wy)(= wy y) (Rtyo 12)))
		((< y Y)
		 (do wy y (1+ wy) (= wy Y)(Rtyo 33)(Rprinc "K"))))
	    (setq X x Y y))
					;; Direct Cursor Addressing is best.
	   (t (setq X x Y y)
	      (Rtyo 33)(Rprinc "8")(Rprinc (dg132b-posit Y))(Rprinc (dg132b-posit X))
       )))

; Output string.
(defun DCTL-display-char-string (string)
       (Rprinc string)
       (setq X (+ X (stringlength string))))

; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (Rtyo 33)(Rprinc "I"))

; Clear to end of line.
(defun DCTL-kill-line ()
       (Rtyo 33)(Rprinc "O"))

(defun DCTL-insert-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33)(Rprinc "3")))

(defun DCTL-delete-lines (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 33)(Rprinc "4")))

(defun DCTL-insert-char-string (str)
       (Rtyo 33)(Rprinc "0")
       (Rprinc  str)
       (Rtyo 33) (Rprinc "5")
       (setq X (+ X (stringlength str))))

(defun DCTL-delete-chars (n)
       (do i 0 (1+ i)(= i n)
	 (Rtyo 33)(Rprinc "6")))

(array dg132b-posit t 132.)
(fillarray 'dg132b-posit '("001" "002" "003" "004" "005" "006" "007" "008"
			        "009" "010" "011" "012" "013" "014" "015"
			        "016" "017" "018" "019" "020" "021" "022"
			        "023" "024" "025" "026" "027" "028" "029"
			        "030" "031" "032" "033" "034" "035" "036"
			        "037" "038" "039" "040" "041" "042" "043"
			        "044" "045" "046" "047" "048" "049" "050"
			        "051" "052" "053" "054" "055" "056" "057"
			        "058" "059" "060" "061" "062" "063" "064"
			        "065" "066" "067" "068" "069" "070" "071"
			        "072" "073" "074" "075" "076" "077" "078"
			        "079" "080" "081" "082" "083" "084" "085"
			        "086" "087" "088" "089" "090" "091" "092"
			        "093" "094" "095" "096" "097" "098" "099"
			        "100" "101" "102" "103" "104" "105" "106"
			        "107" "108" "109" "110" "111" "112" "113"
			        "114" "115" "116" "117" "118" "119" "120"
			        "121" "122" "123" "124" "125" "126" "127"
			        "128" "129" "130" "131" "132"))
