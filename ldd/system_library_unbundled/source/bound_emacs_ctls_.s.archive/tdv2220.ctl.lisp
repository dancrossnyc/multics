;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declarations to prevent compiler warnings, and removed the
;;;     CR characters before newlines.
;;;                                                      END HISTORY COMMENTS


;;;
;;;       TDV2220 control package
;;;       bb 1981-08-19

(%include e-macros)

(declare (special X Y screenheight screenlinelen ospeed))
(declare (special idel-lines-availablep idel-chars-availablep tty-type))
(declare (special DCTL-prologue-availablep DCTL-epilogue-availablep))
(declare (special region-scroll-availablep))
(declare (*expr Rprinc Rtyo))

;;; Macro to output escape sequence
(defun tdv2220-escape ()
       (Rtyo 33) (Rprinc "["))

;;; Output n to the terminal in decimal.
(defun DCTL-outdec (n)                            ;BSG 3/23/79
       ((lambda (have-output)
                (do digi '(1000. 100. 10. 1) (cdr digi) (null digi)
                    ((lambda (rem)
                             (cond ((or have-output (> rem 0) (= (car digi) 1))
                                    (Rtyo (+ 60 rem))
                                    (setq have-output t)))
                             (setq n (\ n (car digi))))
                     (// n (car digi)))))
        nil))


;;; Output padding, based on n pad characters at 9600-baud
(defun DCTL-pad (n)
       (do-times (// (* n ospeed) 960.)
                 (Rtyo 0)))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq DCTL-prologue-availablep t DCTL-epilogue-availablep t)
       (setq idel-lines-availablep t idel-chars-availablep t)
       (setq region-scroll-availablep nil)
       (setq screenheight 24. screenlinelen 79.)
       (setq tty-type 'tdv2220)
       (DCTL-prologue)
       (DCTL-home-cursor)
       (DCTL-clear-rest-of-screen))

;;; Initialization that must also be done after a QUIT
(defun DCTL-prologue ()
       (tdv2220-escape) (Rprinc "36l"))


;;; Restore terminal to outside state
(defun DCTL-epilogue ()
       (tdv2220-escape) (Rprinc "36h"))


;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (let ((deltax (- x X))
             (deltay (- y Y)))
       (cond ((= deltay 0)
              (cond ((= deltax 0) nil)
                    ((> deltax 0) (tdv2220-escape) (DCTL-outdec deltax)
                                  (Rprinc "C"))
                    ((= x 0) (Rtyo 15))  ;move left
                    (t (tdv2220-escape) (DCTL-outdec (- deltax)) (Rprinc "D"))))
             ((= deltax 0)
              (cond ((> deltay 0) (tdv2220-escape) (DCTL-outdec deltay)
                                  (Rprinc "B"))
                    (t (tdv2220-escape) (DCTL-outdec (- deltay)) (Rprinc "A"))))
             (t (tdv2220-absolute-position x y)))
       (setq X x Y y)))


;;; Perform absolute cursor positioning
(defun tdv2220-absolute-position (x y)
       (tdv2220-escape)
       (DCTL-outdec (1+ y))             ;both arguments plus
       (Rprinc ";")                           ;semicolon must be present
       (DCTL-outdec (1+ x))
       (Rprinc "H"))


;;; Output string.
(defun DCTL-display-char-string (string)
       (setq X (+ X (stringlength string)))
       (Rprinc string))


;;; Home cursor to upper left corner.
(defun DCTL-home-cursor ()
       (setq X 0 Y 0)
       (tdv2220-escape) (Rprinc "H"))             ;direct cursor address
;without args.

;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (tdv2220-escape) (Rprinc "J"))


;;; Clear to end of line.
(defun DCTL-kill-line ()
       (tdv2220-escape) (Rprinc "K"))


;;; Insert n lines at the current cursor position
(defun DCTL-insert-lines (n)
       (tdv2220-escape) (DCTL-outdec n) (Rprinc "L")
       (DCTL-pad (* n 10.)))


;;; Delete n lines at the current cursor position
(defun DCTL-delete-lines (n)
       (tdv2220-escape) (DCTL-outdec n) (Rprinc "M")
       (DCTL-pad (* n 10.)))


;;; Insert string at the current cursor position
(defun DCTL-insert-char-string (string)
       (tdv2220-escape) (DCTL-outdec (stringlength string))
       (Rprinc "@")                               ;Insert blanks
       (DCTL-display-char-string string))         ;and print the string.

(defun DCTL-delete-chars (n)
       (tdv2220-escape) (DCTL-outdec n)
       (Rprinc "P"))
