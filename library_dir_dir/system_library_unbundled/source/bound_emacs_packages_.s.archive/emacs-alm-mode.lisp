;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;

;;; HISTORY COMMENTS:
;;;  1) change(81-07-31,Hornig), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     to separate electric-alm mode.
;;;  2) change(84-12-27,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to register and set mode-identification
;;;     in alm-mode, as it is required by compile-buffer.
;;;  3) change(85-01-27,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to declare mode-identification special.
;;;                                                      END HISTORY COMMENTS


(%include e-macros)

(declare (special fill-column comment-column comment-prefix fill-prefix
	        compiler compile-options mode-identification))

(defvar alm-mode-hook nil)

(defun alm-mode ()
       (register-local-var 'compiler)
       (register-local-var 'compile-options)
       (register-local-var 'mode-identification)
       (setq current-buffer-mode 'ALM compiler 'alm compile-options ""
	   mode-identification -3)
       (setq comment-column 40. comment-prefix "")
       (negate-minor-mode 'electric)
       (set-key 'ESC-^C 'compile-buffer)
       (if alm-mode-hook (errset (funcall alm-mode-hook))))

(defun electric-alm-mode ()
       (alm-mode)
       (setq fill-prefix TAB)
       (set-key ': 'alm-label-hacker)
       (set-key '^M 'nl-nb-line)
       (assert-minor-mode 'electric))

(defun nl-nb-line ()
       (and (eolp)(delete-white-sides))
       (new-line))

(defun alm-label-hacker ()
       (save-excursion
        (go-to-beginning-of-line)
        (if (at-white-char)(delete-white-sides)))
       (insert-string ":")
       (format-to-col 10.))

