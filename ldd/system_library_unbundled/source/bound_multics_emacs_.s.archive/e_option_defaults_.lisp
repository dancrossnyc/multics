;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1984 *
;;; *                                                         *
;;; ***********************************************************

;;; Default option setting for Multics Emacs.
;;; Culled from bound_multics_emacs_, bound_emacs_full_,
;;; bound_emacs_packages_, and bound_emacs_rmail_


;;; HISTORY COMMENTS:
;;;  1) change(84-01-19,Margolin), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     Created.  Changed default paragraph-definition-type
;;;     to 2, remember-empty-responses to nil, find-file-set-modes to t,
;;;     in the process.
;;;  2) change(84-12-30,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     added suppress-remarks.
;;;  3) change(86-02-24,Margolin), approve(86-02-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added find-file-check-dtcm, save-same-file-check-dtcm, and
;;;     write-file-overwrite.
;;;  4) change(87-01-30,Margolin), approve(87-01-30,MCR7607),
;;;     audit(87-02-13,RBarstad), install(87-03-25,MR12.1-1014):
;;;     Added options for emacs-compilations: compile-local-display,
;;;     compile-two-windows, and one-error-scan-buffer.
;;;                                                      END HISTORY COMMENTS


(declare (*expr register-option))

;;; e_basic_

(register-option 'paragraph-definition-type 2)
(register-option 'kill-ring-max-size 10.)
(register-option 'default-fill-column 78.)
(register-option 'default-comment-column 60.)
(register-option 'quit-on-break t)
(register-option 'no-minibuffer-<> nil)
(register-option 'underline-whitespace nil)
(register-option 'remember-empty-response nil)
(register-option 'rubout-tabs-into-spaces nil)
(register-option 'track-eol-opt nil)
(register-option 'gratuitous-marks nil)

;;; e_interact_

(register-option 'eval:eval t) ; Unfortunately
(register-option 'eval:assume-atom nil)
(register-option 'eval:correct-errors nil)
(register-option 'eval:prinlevel 3)
(register-option 'eval:prinlength 6)
(register-option 'suppress-minibuffer nil)
(register-option 'autoload-inform nil)
(register-option 'suppress-remarks nil)
(register-option 'command-bell nil)
(register-option 'command-bell-count nil)
(register-option 'meter-commands nil)
(defprop command-bell t value-ok-anything)
(defprop command-bell-count t value-ok-anything)
(defprop meter-commands t value-ok-anything)

;;; e_multics_files_

(register-option 'check-newline nil) ; The wrong thing.
(register-option 'add-newline   't)  ; The right thing.
(register-option 'read-file-force nil)
(register-option 'find-file-set-modes t)
(register-option 'find-file-check-dtcm t)
(register-option 'save-same-file-check-dtcm t)
(register-option 'write-file-overwrite nil)

;;; e_redisplay_

(register-option 'rdis-wosclr-opt nil)		;11/23/78 sorry, Olin. -b
(register-option 'display-ctlchar-with-^ nil)
(register-option 'suppress-ctlchar-display nil)
(register-option 'suppress-backspace-display nil)
(register-option 'suppress-rubout-display nil)
(register-option 'rdis-whitespace-optimize t) ;made t 9/12/80 -- BSG
(register-option 'screen-overlap 1)

;;; e_window_mgr_

(register-option 'pop-up-windows nil)

;;; emacs-compilations

(register-option 'compile-local-display nil)
(register-option 'compile-two-windows nil)
(register-option 'one-error-scan-buffer t)

;;; emacs-completions

(register-option 'cmp:allow-ambiguous 'On)

;;; emacs-console-messages

(register-option 'short-message-accept nil)	;default is long
(register-option 'fill-messages nil)		;default is to not fill
(register-option 'message-hook nil)		;default is normal acceptor

;;; emacs-lisp-debug-mode

(register-option 'ldebug-prinlevel 6.)
(register-option 'ldebug-prinlength 10.)
(register-option 'ldebug-base 8.)
(register-option 'ldebug-ibase 8.)

;;; emacs-lisp-mode

(register-option 'elcp t)
(register-option 'lisp-indent-fuzz 1)

;;; emacs_pl1_mode_

(register-option 'pl1-indentation 5)
(register-option 'pl1-first-column 10.)
(register-option 'pl1-compile-options "")
(register-option 'pl1-inding-style 1)
(register-option 'pl1-dcl-style 1)
(register-option 'pl1-dcl-column 41.)
(register-option 'pl1-line-length 112.)
(register-option 'pl1-comment-style 1)
(register-option 'pl1-comment-column 61.)
(register-option 'pl1-comment-column-delta 10.)

;;; emacs_rmail_

(register-option 'rmail-original-yank-indent 4)
(register-option 'rmail-send-acknowledgement t)
(register-option 'rmail-request-acknowledgement nil)
(register-option 'rmail-reply-include-authors t)
(register-option 'rmail-reply-include-recipients nil)
(register-option 'rmail-reply-include-self nil)
(register-option 'rmail-header-format 'default-formatting-mode)
