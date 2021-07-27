;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1981 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;; Emacs completion command and underpinnings.
;;;
;;; Richard Mark Soley and Barry Margolin, August 1981
;;; Modified 19 November 1981 RMSoley for trying other completions on
;;;	successive ESC-SPACE's, disallow minibuffer, get rid of table.
;;; Modified 3 October 1982 B. Margolin for not depending upon being
;;;	bound to ESC-SPACE.
;;; Modified 19 January 1984 B. Margolin to comment out register-option form,
;;;	   as it was moved to e_option_defaults_.
;;;

(declare (special completion-list cmp:worked cmp:mark cmp:last-completion
	        cmp:allow-ambiguous X Y minibufferp
	        previous-command current-command)
         (*lexpr cmp:get-completion)
         (*expr DCTL-position-cursor))

(%include e-macros)

(eval-when (eval compile)
(defun abort-completion macro (form)
       '(prog2 (ring-tty-bell) (throw 0 nocomplete)))
(defun catch-abort macro (form)
       `(catch ,@(cdr form) nocomplete)))

(or (boundp 'completion-list) (setq completion-list nil))
(setq cmp:worked nil cmp:mark nil cmp:last-completion nil)
;;; (register-option 'cmp:allow-ambiguous 'On) ;moved to e_option_defaults_

(defcom complete-command
        &numeric-argument (&pass)
        (cond ((not minibufferp) (command-quit))
	    (numarg 
	      (or (eq previous-command current-command)
		(setq cmp:worked nil))
	      (cmp:display-completions))
	    ((cmp:undo-completion?)
	     (without-saving (wipe-point-mark cmp:mark))
	     (release-mark cmp:mark)
	     (catch-abort
	       (let ((completion-info
		     (cmp:get-completion (cmp:get-word)
				     cmp:last-completion)))
		  (cond (completion-info
			(setq cmp:worked t
			      cmp:last-completion (car completion-info))
			(insert-string
			  (substr (car completion-info)
				(cdr completion-info)))
			(insert-string SPACE))
		        (t (setq cmp:worked nil cmp:mark nil))))))
	    (t (catch-abort
	         (let ((completion-info (cmp:get-completion (cmp:get-word))))
		    (cond (completion-info
			  (setq cmp:worked t
			        cmp:last-completion (car completion-info))
			  (insert-string
			    (substr (car completion-info)
				  (cdr completion-info)))
			  (insert-char SPACE))))))))

(defun cmp:undo-completion? ()
       (and cmp:worked
	  cmp:mark
	  (eq previous-command 'complete-command)))

(defun cmp:set-mark ()
       (and cmp:mark (release-mark cmp:mark))
       (setq cmp:mark (set-mark)))

(defun cmp:get-word ()
       (cmp:set-mark)
       (with-mark
         here
         (go-to-beginning-of-line)
         (prog1 (point-mark-to-string here)
	      (go-to-mark here))))

(defun cmp:get-completion lexpr
       (let ((word (arg 1))
	   (ignore-until (and (> lexpr 1) (arg 2)))
	   (found nil))
	  (do ((words (cond (ignore-until
			  (cdr (member ignore-until completion-list)))
			(t completion-list))
		    (cdr words)))
	      ((null words)
	       (cond (found found)
		   (t (setq cmp:last-completion nil)
		      (abort-completion))))
	      (let ((cur-word (car words)))
		 (and (= (index cur-word word) 1)
		      (cond
		        (cmp:allow-ambiguous
			(return (cons cur-word
				    (1+ (stringlength word)))))
		        (found (abort-completion))
		        (t (setq found
			       (cons cur-word
				   (1+ (stringlength word)))))))))))

(defun cmp:display-completions ()
       (or completion-list
	 (display-error "There are no completions in effect."))
       (let ((littleX X) (littleY Y))
	  (init-local-displays)
	  (local-display-generator-nnl "Current Completions in Effect")
	  (local-display-generator-nnl "")
	  (do ((words completion-list (cdr words)))
	      ((null words))
	      (local-display-generator-nnl (car words)))
	  (end-local-displays)
	  (DCTL-position-cursor littleX littleY)))
