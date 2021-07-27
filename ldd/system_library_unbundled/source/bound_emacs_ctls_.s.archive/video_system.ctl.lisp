;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1981 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;
;;;	Video System CTL uses Multics Video System

;;; HISTORY COMMENTS:
;;;  1) change(83-12-03,Margolin), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;       Ripped off from VIP7200ctl  BSG 6/6/78 (!)
;;;               Suzanne Krupp 12/30/80
;;;               Standardized to not force vs on, 22 June 1981 RMSoley
;;;               Protcol for window status BIM July 1981
;;;               Modified to check actual terminal capabilities before setting
;;;                    the flags by WMY, 11 August 1981
;;;               Add DCTL-prologue, which recomputes the window/terminal
;;;                    info.  Barmar, 3 December 1983
;;;  2) change(85-01-25,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Fix code declaration in video_system_ctl_util_$get_terminal_capabilities
;;;     to have decimal point.
;;;                                                      END HISTORY COMMENTS


(declare
 (special X Y tty-type rdis-whitespace-optimize
	idel-lines-availablep idel-chars-availablep
	DCTL-prologue-availablep DCTL-epilogue-availablep
	iocb_ptr code columns rows lines_per_scroll
	y_origin x_origin screenlinelen screenheight
	region-scroll-availablep overstrike-availablep ospeed)
 (*expr convert_status_code_ e_lap_$rtrim error_table_ Rprinc e_pl1_$get_iocb)

 (defpl1 window_$position_cursor ""
         (ptr) (fixed bin) (fixed bin) (return (setq code) fixed bin(35.)))
 (defpl1 window_$clear_to_end_of_window ""
         (ptr) (return (setq code) fixed bin(35.)))
 (defpl1 window_$clear_to_end_of_line ""
         (ptr) (return (setq code) fixed bin(35.)))
 (defpl1 window_$scroll_region ""
         (ptr) (fixed bin) (fixed bin) (fixed bin)
         (return (setq code) fixed bin(35.)))
 (defpl1 window_$insert_text ""
         (ptr) (char(*)) (return (setq code) fixed bin(35.)))
 (defpl1 window_$overwrite_text ""
         (ptr) (char(*)) (return (setq code) fixed bin(35.)))
 (defpl1 window_$delete_chars ""
         (ptr) (fixed bin) (return (setq code) fixed bin(35.)))
 (defpl1 window_$clear_window ""
         (ptr) (return (setq code) fixed bin(35.)))
 (defpl1 window_$bell "" (ptr) (return (setq code) fixed bin (35.)))
 ;;; Perform primitive window status check. to be haired up later.
 (defpl1 e_pl1_$check_for_window_status "" (fixed bin (35.)))

 ;;; This pl1 subroutine returns information about available terminal features.
 (defpl1 video_system_ctl_util_$get_terminal_capabilities ""
         (ptr)				; iocb pointer
         (lisp)				; the constant "t"
         (lisp)				; the constant "nil"
         (return (setq region-scroll-availablep) lisp)
         (return (setq idel-chars-availablep) lisp)
         (return (setq overstrike-availablep) lisp)
         (return (setq ospeed) fixed bin)
         (return (setq code) fixed bin(35.)))
         
 ;;; This pl1 routine returns infomation about the position and size of the
 ;;; window whose iocb pointer is iocb_ptr.
 (defpl1 video_system_ctl_util_$get_window_info ""
         (ptr)				; iocb_ptr
         (return (setq y_origin) fixed bin)	; Y_origin - line
         (return (setq x_origin) fixed bin)	; X_origin - col
         (return (setq screenlinelen) fixed bin)	; width
         (return (setq screenheight) fixed bin)	; height
         (return (setq code) fixed bin(35.)))
 )

;;; Initialize terminal and terminal control package.

(defun DCTL-init ()
       (setq iocb_ptr (e_pl1_$get_iocb))

       (putprop 'video_system t 'tintinnabulum-ipsum-meum-sono)
       (setq tty-type 'video_system
	   DCTL-prologue-availablep t
	   DCTL-epilogue-availablep t)

       (DCTL-prologue)			;initialize window/terminal info

       (window_$clear_window iocb_ptr)
       (e_pl1_$check_for_window_status code)
       (setq X 0
	   Y 0
;;	   rdis-whitespace-optimize nil ;; removed 12/3/83
	   ))

;;; Prologue code
(defun DCTL-prologue ()
       (video_system_ctl_util_$get_window_info iocb_ptr)
       (cond ((zerop code))
	   ((= code (error_table_ 'undefined_order_request))   ;not in video
	    (Rprinc "emacs: Video system CTL invoked with no video system."))
	   (t (Rprinc (catenate "emacs: "
			    (e_lap_$rtrim
			      (cadr (convert_status_code_ code)))
			    " Can't get window info."))))

       (video_system_ctl_util_$get_terminal_capabilities iocb_ptr t nil)
       (cond ((not (zerop code))
	    (Rprinc (catenate "emacs: "
			  (e_lap_$rtrim
			    (cadr (convert_status_code_ code)))
			  " Can't get terminal capabilities.")))))

;;; Epilogue code
(defun DCTL-epilogue ()
       (window_$clear_window iocb_ptr)
       (e_pl1_$check_for_window_status code))

;;; Move terminal's cursor to desired position.
(defun DCTL-position-cursor (x y)
       (window_$position_cursor iocb_ptr (1+ y) (1+ x))
       (e_pl1_$check_for_window_status code)
       (setq X x Y y))

;;; Output string.
(defun DCTL-display-char-string (string)
       (window_$overwrite_text iocb_ptr string)
       (e_pl1_$check_for_window_status code)
       (setq X (+ X (stringlength string))))

;;; Clear entire screen
(defun DCTL-clear-screen ()
       (window_$clear_window iocb_ptr)
       (e_pl1_$check_for_window_status code))

;;; Clear to end of screen.
(defun DCTL-clear-rest-of-screen ()
       (window_$clear_to_end_of_window iocb_ptr)
       (e_pl1_$check_for_window_status code))

;;; Clear to end of line.
(defun DCTL-kill-line ()
       (window_$clear_to_end_of_line iocb_ptr)
       (e_pl1_$check_for_window_status code))

;;; Scroll down.
(defun DCTL-scroll-down-region (nlines bottom)
       (window_$scroll_region iocb_ptr (1+ Y) (1+ (- bottom Y)) nlines)
       (e_pl1_$check_for_window_status code))
       

;;; Scroll up.
(defun DCTL-scroll-up-region (nlines bottom)
       (window_$scroll_region iocb_ptr (1+ Y) (1+ (- bottom Y)) (- nlines))
       (e_pl1_$check_for_window_status code))


;;; Insert a string of characters on the screen.
(defun DCTL-insert-char-string (str)
       (window_$insert_text iocb_ptr str)
       (e_pl1_$check_for_window_status code)
       (setq X (+ X (stringlength str))))

;;; Delete a string of characters from the screen.
(defun DCTL-delete-chars (n)
       (window_$delete_chars iocb_ptr n)
       (e_pl1_$check_for_window_status code))

;;; Ring the terminal's bell.
(defun DCTL-ring-tty-bell ()
       (window_$bell iocb_ptr)
       (e_pl1_$check_for_window_status code))
