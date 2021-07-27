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
;;;	supdupctl -- Written by BSG 7/12/78 after he wrote a
;;;	SUPDUP user end and discovered he didn't have a server.
;;;	From PYZCTL.
;;;
;;;       From old aitvctl, from dm2500ctl.
;;;

;;;	Define the old %TD codes.

(declare (eval (read)))
(do x (read)(read)(eq x 'applesauce)(eval x))

(setsyntax '% 'macro 'DCTL-tdcode-macro)

(setq DCTL-tdcode-list '(

	TDEOF	202
	TDEOL	203
	TDNOP	210
	TDMV0	217
	TDMOV	200
	TDCLR	220
	TDBEL	221
	TDILP	223
	TDDLP	224
	TDICP	225
	TDDCP	226
		      ))

(do x DCTL-tdcode-list (cddr x)(null x)(putprop (car x)(cadr x) 'tdcode))
(defun cana macro (x)
    (list 'not (list 'zerop (list 'boole 1 (cadr x)(caddr x)))))

(defun DCTL-tdcode-macro ()
       (or (get (read) 'tdcode)
	 (error "Undefined tdcode")))

applesauce


(declare (special X Y screenheight tty-no-upmotionp screenlinelen tty-type overstrike-availablep e-quit-transparency))
(declare (special DCTL-ttyopt-word DCTL-aobjn-count))
(declare (special idel-lines-availablep idel-chars-availablep))


;;; Initialize terminal and terminal control package.

(defun DCTL-init ()
       (Rprinc "Multics EMACS Supdup Server")
       (Rtyo 15)(Rtyo 12)
       (e_pl1_$dump_output_buffer)
       (setq DCTL-aobjn-count (lsh (- (DCTL-get-supdup-36-word)) -18.))
       (DCTL-get-supdup-36-word) ;TCTYP
       (setq DCTL-ttyopt-word (DCTL-get-supdup-36-word))
       (setq screenheight (DCTL-get-supdup-36-word))
       (setq screenheight (min 64. screenheight))
       (setq screenlinelen (DCTL-get-supdup-36-word))
       (DCTL-get-supdup-36-word) ;TTYROL
       (setq DCTL-aobjn-count (- DCTL-aobjn-count 5))
       (do ()((= DCTL-aobjn-count 0))
          (setq DCTL-aobjn-count (1- DCTL-aobjn-count))
          (DCTL-get-supdup-36-word))
       (setq idel-chars-availablep (cana DCTL-ttyopt-word 000001000000))
       (setq idel-lines-availablep (cana DCTL-ttyopt-word 000002000000))
       (setq overstrike-availablep (cana DCTL-ttyopt-word 001000000000))
       (setq tty-no-upmotionp (not (cana DCTL-ttyopt-word 000400000000)))
       (setq tty-type 'supdup)
       (Rtyo %TDNOP)
       (set-permanent-key '^\    'supdup-ITP-escape)
       (set-permanent-key 'esc-@ 'supdup-300-escape)
       (setq X -777 Y -777)
       (DCTL-position-cursor 0 0)
       (DCTL-clear-rest-of-screen))

(defun DCTL-get-supdup-36-word ()
     (do ((w 0 (+ (lsh w 6) b))
          (b)
          (i 1 (1+ i)))
         ((> i 6) w)

	(setq b (DCTL-gnz-char)))))))


(defun DCTL-assert-scpos (x y)
       (and x (setq X x))
       (and y (setq Y y))
       (DCTL-tdmov X Y X Y))

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
       (Rtyo %TDMOV)
       (Rtyo oldy)
       (Rtyo oldx)
       (Rtyo newy)
       (Rtyo newx))


;;; Actually move a tty cursor.

(defun DCTL-real-position-cursor (x y)
       (Rtyo %TDMV0)
       (Rtyo y)
       (Rtyo x)
       (setq X x Y y))


(defun DCTL-ring-tty-bell ()
       (Rtyo %TDBEL))

(defprop supdup t tintinnabulum-ipsum-meum-sono)

; Output string.
(defun DCTL-display-char-string (string)
       ((lambda (len)
	      (setq X (+ X len))
	      (Rprinc string))
        (stringlength string)))

; Clear whole screen.
(defun DCTL-clear-rest-of-screen ()
       (cond (tty-no-upmotionp (DCTL-nextline))
	   (t (Rtyo %TDEOF))))

; Go to next line on non-moveuppable terminals.
(defun DCTL-nextline ()
       (cond ((= Y (1- screenheight))
	    (DCTL-position-cursor X 0))
	   (t (DCTL-position-cursor X (1+ Y)))))

; Clear to end of line.

(defun DCTL-kill-line ()
       (Rtyo %TDEOL))

; Insert character string in line at current position.
(defun DCTL-insert-char-string (str)
       ((lambda (len)
	      (Rtyo %TDICP)
	      (Rtyo len)
	      (Rprinc str)
	      (setq X (+ X len)))
        (stringlength str)))

; Delete characters from current position in line.
(defun DCTL-delete-chars (n)
       (Rtyo %TDDCP)
       (Rtyo n))

; Insert n blank lines at current position.
(defun DCTL-insert-lines (n)
       (Rtyo %TDILP)
       (Rtyo n))


; Delete n lines at current position.
(defun DCTL-delete-lines (n)
       (Rtyo %TDDLP)
       (Rtyo n))

; Intelligent terminal protocol Handlers.

(defun DCTL-gnz-char ()
   (do x (e_pl1_$get_char)(e_pl1_$get_char) nil
         (cond ((= x 377)((lambda (e-quit-transparency) (telnet-loser (DCTL-gnz-char))) 'leave-it)(setq x -1)))
       (or (< x 0)(return x))))

(defun supdup-300-escape ()
       ((lambda (c)
        (cond ((= c 301)(quit))
	    ((= c 302)(do x (DCTL-gnz-char)(DCTL-gnz-char)(= x 0)))
	    (t () )))
        (DCTL-gnz-char)))

(defun supdup-ITP-escape ()
       (real-supdup-ITP-escape (DCTL-gnz-char)))

(defun real-supdup-ITP-escape (c)
	(cond ((= c 34)) ;too bad
	      ((= c 003)  ;PIATY
	       (do ()(nil)
		  ((lambda (d)
		      (cond ((= d 34)((lambda(e)
				     (cond ((= e 003))
					 (t (real-supdup-ITP-escape e))))
				  (DCTL-gnz-char)))
			  (t (full-redisplay)
			     (process-char d)
			     (return nil))))
		   (DCTL-gnz-char))))
	      ((= c 020)   ;^P
	       (setq Y (DCTL-gnz-char) X (DCTL-gnz-char)))
	      (t ((lambda (d)
		  (setq c (boole 1 3 c));meta, control
	            (and (cana d 1)(setq d (- d 100)))
		  (xec-cmd-triplet (lsh c -1) d nil))
	         (DCTL-gnz-char)))))
