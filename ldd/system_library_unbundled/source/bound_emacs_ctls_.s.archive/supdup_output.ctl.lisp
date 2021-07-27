;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;;
;;;	supdup_outputctl -- from PYZctl BSG 10/1/78
;;;	PYZctl - Protocols of the Youngers of Zion
;;;	Which BSG and DLW scratched out on notes during an otherwise
;;;	rowdy SIPB meeting.
;;;       From old aitvctl, from dm2500ctl.
;;;       BSG & DLW 6/26/78
;;;	Supdup Block negotiation added by BSG, 8/10/78,
;;;	who also hand-coded the MIDAS-coded ITS-side that day.
;;;
;;;	Modified 29 April 1981 by WMY, BARMAR and DCP to use the SUPDUP
;;;	scroll-region operation, and to clean up some names.


(declare (defpl1 e_pl1_$return_supdup_info "" (return bit (36.))(return fixed bin (35.))(return fixed bin (35.))))
(declare (defpl1 e_pl1_$set_multics_tty_modes ""))

(declare (eval (read)))
(do x (read)(read)(eq x 'applesauce)(eval x))

(setsyntax '% 'macro 'DCTL-tdcode-macro)

;;;	Define the old %TD codes.

(setq DCTL-tdcode-list
      '(
      TDMOV   200   ; absolute cursor position: old_x old_y new_x new_y
      TDEOF   202	; clear to end of screen (file)
      TDEOL   203	; clear to end of line
      TDNOP   210	; no-op
      TDMV0   217	; absolute cursor position: new_x new_y
      TDCLR   220	; clear whole screen and home
      TDBEL   221	; ding dong
      TDILP   223	; insert lines: number_of_lines
      TDDLP   224	; delete lines: number_of_lines
      TDICP   225	; insert characters: number_of_characters
      TDDCP   226	; delete characters: number_of_characters
      TDRSU   232	; region scroll up:   lines_in_region lines_to_scroll
      TDRSD   233	; region scroll down: lines_in_region lines_to_scroll

;;;	Protocols of the Elders of ARPA.

      IAC     377	; IAC escape code for telnet connections
      SB	    250.	; supdup subnegotiation begin
      SE	    240.	; supdup subnegotiation end
      SUPDUP-OUTPUT 22.
      ))

(do x DCTL-tdcode-list (cddr x)(null x)(putprop (car x)(cadr x) 'tdcode))
(defun DCTL-tdcode-macro ()
       (or (get (read) 'tdcode)
	 (error "Undefined tdcode")))

(defun cana macro (x)
    (list 'not (list 'zerop (list 'boole 1 (cadr x)(caddr x)))))
applesauce


(declare (special X Y screenheight screenlinelen tty-type overstrike-availablep))
(declare (special DCTL-ttyopt-word tty-no-upmotionp))
(declare (special idel-lines-availablep idel-chars-availablep
	        region-scroll-availablep))


;;; Initialize terminal and terminal control package.
(defun DCTL-init ()
       ((lambda (sdparms)
	      (setq DCTL-ttyopt-word (car sdparms)
		  screenlinelen (cadr sdparms)
		  screenheight (min 60. (caddr sdparms))))
        (e_pl1_$return_supdup_info))

       ;; Since very bad stuff happens if the above code fails, check it out.
       (cond ((or (zerop DCTL-ttyopt-word)
	        (zerop screenlinelen)
	        (zerop screenheight))
	    (e_pl1_$set_multics_tty_modes)
	    (princ "Invalid SUPDUP info:")
	    (print (list DCTL-ttyopt-word screenlinelen screenheight))
	    (terpri)
	    (quit)))

       ;; decode the TTYOPT word, see MC:.INFO.;ITS TTYVAR for info
       (setq region-scroll-availablep (cana DCTL-ttyopt-word 000000000004))
       (setq idel-lines-availablep    (cana DCTL-ttyopt-word 000002000000))
       (setq idel-chars-availablep    (cana DCTL-ttyopt-word 000001000000))
       (setq overstrike-availablep    (cana DCTL-ttyopt-word 001000000000))
       (setq tty-no-upmotionp    (not (cana DCTL-ttyopt-word 000400000000)))
       (setq tty-type 'supdup_output)
       (setq X -777 Y -777)
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))

(defun DCTL-assert-scpos (x y)
       (and x (setq X x))
       (and y (setq Y y))
       (DCTL-tdmov X Y X Y))

; Go to next line on non-moveuppable terminals.
(defun DCTL-nextline ()
       (cond ((or (< X 0)(< Y 0))
	    (DCTL-position-cursor 0 0))
	   ((= Y (1- screenheight))
	    (DCTL-position-cursor X 0))
	   (t (DCTL-position-cursor X (1+ Y))
	      (DCTL-assert-scpos nil (1- Y)))))

;;; Move terminal's cursor to desired position.
;;; Real work is done in DCTL-real-position-cursor.

;;;	This hairy hack is solely for the benefit of printing tty's,
;;;	and interfaces Multics EMACS' notion of a prtty "screen" to ITS's.


(defun DCTL-position-cursor (x y)
       (cond ((and (= x X)(= y Y)))		;aok, exit.
	   ((not tty-no-upmotionp)(DCTL-real-position-cursor x y))
	   ((or (< X 0)(< Y 0))		;randomized?
	    (DCTL-tdmov 50 0 0 1)
	    (DCTL-tdmov 0 0 x y))
	   ((= Y y)(DCTL-real-position-cursor x y))
	   ((= y 0)(DCTL-tdmov X 0 X 1)
		 (DCTL-tdmov X y x y))
	   (t (DCTL-tdmov X (1- y) x y))))


(defun DCTL-tdmov (oldx oldy newx newy)
       (setq X newx Y newy)
       (DCTL-begin-subnegotiation 5)
       (Rtyo %TDMOV)
       (Rtyo oldy)
       (Rtyo oldx)
       (Rtyo newy)
       (Rtyo newx)
       (DCTL-out-scpos))


;;; Actually move a tty cursor.

(defun DCTL-real-position-cursor (x y)
       (DCTL-begin-subnegotiation 3)
       (Rtyo %TDMV0)
       (Rtyo y)
       (Rtyo x)
       (setq X x Y y)
       (DCTL-out-scpos))

(defun DCTL-ring-tty-bell ()
       (DCTL-begin-subnegotiation 1)
       (Rtyo %TDBEL)
       (DCTL-out-scpos))
(defprop supdup_output t tintinnabulum-ipsum-meum-sono)

; Output string.
(defun DCTL-display-char-string (string)
       ((lambda (len)
	      (DCTL-begin-subnegotiation len)
	      (setq X (+ X len))
	      (Rprinc string)
	      (DCTL-out-scpos))
        (stringlength string)))

; Clear whole screen.
(defun DCTL-clear-rest-of-screen ()
       (cond (tty-no-upmotionp
	    (DCTL-nextline))	   
	   (t (DCTL-begin-subnegotiation 1)
	      (Rtyo (cond ((and (= X 0)(= Y 0)) %TDCLR)
		        (t %TDEOF)))
	      (DCTL-out-scpos))))

; Clear to end of line.
(defun DCTL-kill-line ()
       (cond (tty-no-upmotionp		;do nextline op
	    (DCTL-nextline))
	   (t (DCTL-begin-subnegotiation 1)
	      (Rtyo %TDEOL)
	      (DCTL-out-scpos))))

; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       ((lambda (len)
	      (DCTL-begin-subnegotiation (+ 2 len))
	      (Rtyo %TDICP)
	      (Rtyo len)
	      (Rprinc str)
	      (setq X (+ X len))
	      (DCTL-out-scpos))
        (stringlength str)))

; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (DCTL-begin-subnegotiation 2)
       (Rtyo %TDDCP)
       (Rtyo n)
       (DCTL-out-scpos))

; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (DCTL-begin-subnegotiation 2)
       (Rtyo %TDILP)
       (Rtyo n)
       (DCTL-out-scpos))


; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (DCTL-begin-subnegotiation 2)
       (Rtyo %TDDLP)
       (Rtyo n)
       (DCTL-out-scpos))

;;; Scroll region N lines long up m lines
(defun DCTL-scroll-up-region (nlines bottom-line)
       (DCTL-begin-subnegotiation 3)
       (Rtyo %TDRSU)
       (Rtyo (1+ (- bottom-line Y)))
       (Rtyo nlines)
       (DCTL-out-scpos))

;;; Scroll region N lines long down m lines
(defun DCTL-scroll-down-region (nlines bottom-line)
       (DCTL-begin-subnegotiation 3)
       (Rtyo %TDRSD)
       (Rtyo (1+ (- bottom-line Y)))
       (Rtyo nlines)
       (DCTL-out-scpos))

;;; Internal frobula used by above.  Begins the SUPDUP-OUTPUT subnegotiation.
;;; This includes the proper TDcodes, and constant 2, then the number of
;;; TDcodes coming after this.
(defun DCTL-begin-subnegotiation (tdcode-count)
       (mapc 'Rtyo '(%IAC %SB %SUPDUP-OUTPUT 2))
       (Rtyo tdcode-count)) ;better be not 377 or other funnies.

(defun DCTL-out-scpos ()
       (cond ((or (> X screenlinelen) (< X 0) (> Y (1- screenheight)) (< Y 0))
	    (print "=====> ")
	    (prin1 (cons X Y))
	    (break "DCTL-out-scpos has seen losing X and/or Y." t)))
       (Rtyo X)
       (Rtyo Y)
       ;; end the subnegotiation
       (mapc 'Rtyo '(%IAC %SE)))
