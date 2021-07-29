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
;;;       ADM2 control package
;;;       EAK 3/31/78
;;;

(declare (special dcaconses X Y screenheight tty-type))
(declare (special idel-lines-availablep idel-chars-availablep screenlinelen))


; Initialize terminal and terminal control package.
(defun DCTL-init ()
       (setq dcaconses (list (ascii 33) '= nil nil))
       (setq screenheight 24.)                             ; 20 lines for editing
       (setq screenlinelen 79.)
       (setq idel-lines-availablep t
             idel-chars-availablep t)
       (setq tty-type 'adm2)
       (Rtyo 33)(Rprinc "*")                            ; clear screen
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
                   cost (+ 1 y x))              ; cost is V + H + 1
             (and (> cost 4)                    ; direct cursor address better?
                  (setq what 0                  ; 0: "direct cursor address"
                        cost 4))                ; cost is 4 characters
             (setq ycost (abs (- y Y)))
             (setq xcost (abs (- x X)))
             (and (< (+ ycost xcost) cost)
                  (setq what 3                  ; 3: "relative move"
                        cost (+ ycost xcost)))
             (and (< (+ 1 ycost x) cost)
                  (setq what 2))                ; 2: "CR and relative move"
             (cond ((= what 0)

; Direct Cursor Address

                    (rplaca (cddr dcaconses) (+ 40 y))
                    (rplaca (cdddr dcaconses) (+ 40 x))

                    (Rprinc (implode dcaconses))
                    (setq X x Y y)
                    (return nil))

                   ((= what 1)                  ; home and relative move?
                    (Rtyo 36)                   ; home
                    (setq X 0 Y 0))             ; keep track of cursor
                                                ; fall through to relative move

                   ((= what 2)                  ; CR and relative move?
                    (Rtyo 15)                   ; CR
                    (setq X 0)))                ; keep track of cursor
                                                ; fall through to relative move

; Relative Move

             (cond ((< X x)
                    (do ex X (1+ ex)(= ex x)(Rtyo 14)))
                   ((< x X)
                    (do ex x (1+ ex)(= ex X)(Rtyo 10))))
             (cond ((< Y y)
                    (do wy Y (1+ wy)(= wy y)(Rtyo 12)))
                   ((< y Y)
                    (do wy y (1+ wy)(= wy Y)(Rtyo 13))))
             (setq X x Y y)
             (return nil)))


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


; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       (do i (stringlength str) (1- i) (= i 0)
           (Rtyo 33)(Rprinc "Q"))
       (Rprinc str)
       (setq X (+ X (stringlength str))))


; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "W")))


; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "E")))


; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (do i 1 (1+ i)(> i n)
           (Rtyo 33)(Rprinc "R")))
