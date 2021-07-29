;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; ***********************************************************
;;;--------------------------------------------------------------------
;;;
;;;	This is the source to the VISUAL-200 terminal controller.
;;;	The suggested name for it is vis200.ctl.lisp.  Do what you
;;;	wish with it.
;;;
;;;--------------------------------------------------------------------
;;;
;;;       Visual 200 control package
;;;	14 July 1982
;;;	Ripped off from various places by David M. Warme (Warme.FSOEP)
;;;

(declare (special X Y screenheight tty-type ospeed))
(declare (special screenlinelen))
(declare (special idel-chars-availablep idel-lines-availablep tty-no-cleolp))

; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq screenheight 24.)              ; 20 lines for editing
       (setq screenlinelen 79.)
       (setq tty-type 'vis200)
       (setq idel-lines-availablep t idel-chars-availablep t tty-no-cleolp nil)
       (Rtyo 27.) (Rprinc "v")		; clear screen
       (setq X 0 Y 0))


; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (prog (ycost                             ; cost of y and x relative
              xcost                             ; movement
              what                              ; which movement is best
              cost)                             ; cost of that movement
             (and (= x X)(= y Y)                ; return right away if already
                  (return nil))                 ; at desired position
             (setq what 1                       ; 1: "home and relative move"
                   cost (+ 2 y x x))            ; cost is V + 2*H + 2
             (and (> cost 4)                    ; direct cursor address better?
                  (setq what 0                  ; 0: "direct cursor address"
                        cost 4))                ; cost is 4 characters
	   (setq ycost (cond ((< y Y) (- Y y))
			 (t (lsh (- y Y) 1))))
	   (setq xcost (cond ((> x X) (- x X))
			 (t (lsh (- X x) 1))))
             (and (< (+ ycost xcost) cost)
                  (setq what 3                  ; 3: "relative move"
                        cost (+ ycost xcost)))
             (and (< (+ 1 ycost x) cost)
                  (setq what 2))                ; 2: "CR and relative move"
             (cond ((= what 0)

; Direct Cursor Address

		(Rtyo 27.)
		(Rprinc "Y")
                    (Rtyo (+ 40 y))
		(Rtyo (+ 40 x))
                    (setq X x Y y)
                    (return nil))

                   ((= what 1)                  ; home and relative move?
		(Rtyo 27.) (Rprinc "H")     ; home
                    (setq X 0 Y 0))             ; keep track of cursor
                                                ; fall through to relative move

                   ((= what 2)                  ; CR and relative move?
                    (Rtyo 15)                   ; CR
                    (setq X 0)))                ; keep track of cursor
                                                ; fall through to relative move

; Relative Move

             (cond ((< X x)
                    (do ex X (1+ ex)(= ex x)(Rtyo 27.)(Rprinc "C")))
                   ((< x X)
                    (do ex x (1+ ex)(= ex X)(Rtyo 10))))
             (cond ((< Y y)
                    (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
                   ((< y Y)
                    (do wy y (1+ wy)(= wy Y)(Rtyo 27.)(Rprinc "A"))))
             (setq X x Y y)
             (return nil)))


; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
    (Rtyo 27.)(Rprinc "y")(setq X 0 Y 0))

; Insert chars.
(defun DCTL-insert-char-string (str)
       (Rtyo 27.)(Rprinc "i")
       (Rprinc str)
       (Rtyo 27.)(Rprinc "j")
       (setq X (+ X (stringlength str))))

; delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (do i 1 (1+ i) (> i n)
	 (Rtyo 27.)(Rprinc "O")))

; Insert blank lines at current position.
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
	 (Rtyo 27.)(Rprinc "L")))

; Delete lines at current position.
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
	 (Rtyo 33)(Rprinc "M")))

; Send pad characters to wait specified no. of msecs.
(defun DCTL-pad (n)
       (do i (// (* n ospeed) 100000.) (1- i) (= i 0) (Rtyo 0)))
; Clear to end of line.
(defun DCTL-kill-line()
       (Rtyo 27.)(Rprinc "x"))
