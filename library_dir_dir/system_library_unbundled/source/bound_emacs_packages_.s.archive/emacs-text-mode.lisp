;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1981 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;; Text mode, cause why not.
;;; Richard Mark Soley 23 November 1981
;;; Modified sometime before 2 November 1984 by B. Margolin to
;;;	   allow text-mode-delete-line-indentation to take a numeric
;;;	   argument.
;;;

(%include e-macros)

(declare (special SPACE text-mode-hook))

(declare (*expr delete-line-indentation fill-mode))

(defcom text-mode
        (setq current-buffer-mode 'Text)
        (establish-local-var 'comment-column 0)
        (establish-local-var 'comment-prefix "")
        (mapc '(lambda (x) (set-key (car x) (cadr x)))
	    '(
	    (ESC-^	text-mode-delete-line-indentation)
	    ))
        (fill-mode)
        (and (boundp 'text-mode-hook) (funcall text-mode-hook)))

(defcom text-mode-delete-line-indentation
        &numeric-argument (&pass)
        (delete-line-indentation)
        (insert-string SPACE))

(defcom-synonym runoff-mode text-mode)
(defcom-synonym compose-mode text-mode)

