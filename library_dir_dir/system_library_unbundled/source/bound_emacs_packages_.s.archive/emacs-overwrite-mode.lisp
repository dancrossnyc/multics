;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;; Modified: 1 July 1981 RMSoley to use Multics rubout character
;;;		and to add overwrite-mode-off.
;;; Modified: 3 December 1983 B. Margolin to fix overwrite-mode-off
;;;	    to correctly reset ^D.
;;;

(%include e-macros)

(declare (special MCS-editing-characters)
         (*expr self-insert))

(defun rubout-character macro (form) '(ItoC (cadr MCS-editing-characters)))

(defun overwrite-mode ()
       (assert-minor-mode 'overwrite)
       (set-key 'esc-D 'overwrite-mode-delete-word)
       (set-key 'esc-# 'overwrite-mode-rubout-word)
       (set-key 'esc-\177 'overwrite-mode-rubout-word)
       (set-key (rubout-character) 'overwrite-mode-rubout-char)
       (set-key '\177 'overwrite-mode-rubout-char)
       (set-key '^D 'overwrite-mode-delete-char)
       (map-over-emacs-commands
        '(lambda (sym fun arg)
	       (and (eq fun 'self-insert)
		  (set-key sym 'overwrite-mode-self-insert))
	       arg)
         nil))

(defprop overwrite-off overwrite-mode-off expr)
(defprop overwriteoff overwrite-mode-off expr)

(defun overwrite-mode-off ()
       (negate-minor-mode 'overwrite)
       (set-key 'esc-D 'delete-word)
       (set-key 'esc-# 'rubout-word)
       (set-key 'esc-\177 'rubout-word)
       (set-key (rubout-character) 'rubout-char)
       (set-key '\177 'rubout-char)
       (set-key '^D 'delete-char)
       (map-over-emacs-commands
        '(lambda (sym fun arg)
	       (and (eq fun 'overwrite-mode-self-insert)
		  (set-key sym 'self-insert))
	       arg)
         nil))

(defun overwrite-mode-self-insert ()
       (or (eolp)(delete-char))
       (self-insert))

(defun overwrite-mode-delete-char ()
       (if (not (eolp))
	 (delete-char)
	 (insert-char " ")))

;;; old delete-char left cursor in same place, "gobbled" chars
;;;(defun overwrite-mode-delete-char ()
;;;       (if (not (eolp))
;;;	 (if (at-white-char)(forward-char)
;;;	   else (delete-char)
;;;	        (save-excursion
;;;	         (skip-to-whitespace)
;;;	         (insert-string " ")))))

(defun overwrite-mode-rubout-char ()
       (or (bolp)(progn (backward-char)
		    (delete-char)
		    (insert-char " ")
		    (backward-char))))

(defprop overwrite-mode-delete-word forward kills)
(defun overwrite-mode-delete-word ()
   (with-mark m
      (forward-word)
      (let ((hp (cur-hpos)))
	 (kill-backwards-to-mark m)
	 (spaces-to-hpos hp)))
   (merge-kills-forward))

(defprop overwrite-mode-rubout-word reverse kills)
(defun overwrite-mode-rubout-word ()
       (with-mark m
	(let ((hpos (cur-hpos)))
	     (backward-word)
	     (kill-forward-to-mark m)
	     (merge-kills-reverse)
	     (save-excursion
	       (spaces-to-hpos hpos)))))

(defun spaces-to-hpos (x)
       (do ((hpdiff (- x (cur-hpos)) (1- hpdiff)))
	 ((< hpdiff 1))
	 (insert-char " ")))

(defun overwrite-mode-insert-string (string)
       (with-mark start
	        (let ((start-pos curpointpos))
		   (go-to-end-of-line)
		   (if (< (- curpointpos start-pos) (stringlength string))
		       (kill-backwards-to-mark start)
		       else
		       (go-to-mark start)
		       (do-times (stringlength string) (delete-char)))))
       (insert-string string))
