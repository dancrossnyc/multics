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
;;;	Directory editor
;;;	Archy, '78, extracted from e_macops_ 8/16/79 by BSG
;;;	Modified: 12/8/83 - B. Margolin - don't	allow d/u when on
;;;		blank line at the end.  Also modernized some code
;;;		(defun->defcom, lambda->let, comout-get-output).
;;;

(%include e-macros)
(declare (special dired-segs-to-delete dired-buffer-to-go-back-to dired-dir)
         (*expr absolute_pathname_ convert_status_code_ error_table_
	      expand_pathname_ go-to-hpos minibuffer-remark))

(declare (defpl1 cu_$level_get "" (return fixed bin)))
(defprop trim e_lap_$trim expr)
(declare (defpl1 hcs_$get_user_effmode
	         "" (char (*))(char (*))(char (*))(fixed bin)(return fixed bin)(return fixed bin (35.))))

(defcom edit-dir
        &numeric-argument (&pass)
        &arguments ((dir &default &eval (if numarg (minibuf-response "Directory: " NL)
			        else "")))
        (setq dir (absolute_pathname_ dir))
        (if (not (zerop (cadr dir)))
	  (display-com-error (cadr dir) "")
	  else
	  (setq dir (trim (car dir))))
        (let ((access-ok (dired-access-check dir)))
	   (if (not access-ok)
	       (display-error-noabort "Warning: Modify access on " dir " lacking.")))
        (go-to-or-create-buffer 'Dir/ Edit)
        (setq buffer-modified-flag t)
        (comout-get-output "list -pn" dir
		       "-mode -name -primary -length -no_header")
        (setq fpathname dir)			;for user
        (do-forever (go-to-beginning-of-line)
		(insert-string TAB)
		(if (lastlinep)(stop-doing))
		(next-line))
        (go-to-beginning-of-buffer)
        (dired-mode)
        (setq dired-segs-to-delete nil)
        (setq dired-buffer-to-go-back-to previous-buffer)
        (setq dired-dir dir)
        (setq read-only-flag t buffer-modified-flag nil)
        (select-buffer-find-window current-buffer 'cursize))

(defun dired-mode ()
       (setq current-buffer-mode 'DIRED)
       (register-local-var 'dired-segs-to-delete)
       (register-local-var 'dired-buffer-to-go-back-to)
       (register-local-var 'dired-dir)
       (set-key 'D 'dired-mark-for-deletion)
       (set-key 'd 'dired-mark-for-deletion)
       (set-key 'n 'next-line-command)
       (set-key 'p 'prev-line-command)
       (set-key 'N 'next-line-command)
       (set-key 'P 'prev-line-command)
       (set-key 'R 'dired-rename)
       (set-key 'r 'dired-rename)
       (set-key 'U 'dired-unmark-for-deletion)
       (set-key 'u 'dired-unmark-for-deletion)
       (set-key 'E 'dired-examine-file)
       (set-key 'e 'dired-examine-file)
       (set-key 'Q 'dired-quit)
       (set-key 'q 'dired-quit)
       (set-key '^X^Q 'dired-quit)
       (set-key '^XB 'dired-quit-and-go-buffer))

(defcom dired-mark-for-deletion
        (if (lastlinep) (command-quit))		;Last line is empty
        (go-to-beginning-of-line)
        (if-at "D"
	     else (without-modifying (insert-string "D"))
		(setq dired-segs-to-delete (cons (dired-get-filename)
					   dired-segs-to-delete)))
        (next-line))

(defcom dired-unmark-for-deletion
        (if (lastlinep) (command-quit))		;Last line is empty
        (go-to-beginning-of-line)
        (if-at "D" (without-modifying (delete-char))
	         (setq dired-segs-to-delete (delete
				        (dired-get-filename)
				        dired-segs-to-delete)))
        (next-line))
			    
(defcom dired-examine-file
        (let ((dname dired-dir)
	    (ename (dired-get-filename)))
	   (find-buffer-in-window '|Dired Examine|)
	   (set-key '^X^Q 'dired-exit-examine-buffer)
	   (read-in-file (catenate dname ">" ename))
	   (minibuffer-remark "Use ^X^Q to return to DIRED")))

(defcom dired-exit-examine-buffer
        (set-buffer-self-destruct current-buffer)
        (find-buffer-in-window '|Dir Edit|))

(defcom dired-quit
        (cond ((null dired-segs-to-delete)
	     (set-buffer-self-destruct current-buffer)
	     (select-buffer-window dired-buffer-to-go-back-to nil)
	     (setq previous-buffer current-buffer))
	    (t (dired-m-access-check)		;aborts if access lacking
	       (init-local-displays)
	       (local-display-generator-nnl "Files to delete:")
	       (local-display-generator-nnl "")
	       (mapc 'local-display-generator-nnl dired-segs-to-delete)
	       (local-display-generator "---------------")
	       (if (yesp "Deleting the above listed files, OK? ")
		 (dired-delete-files dired-segs-to-delete)
		 (set-buffer-self-destruct current-buffer)
		 (select-buffer-window dired-buffer-to-go-back-to nil)
		 (setq previous-buffer current-buffer)
		 else (go-to-beginning-of-buffer)))))

(defcom dired-quit-and-go-buffer
        &arguments ((buffer &symbol &prompt "Select Buffer: "
		        &default  &eval dired-buffer-to-go-back-to))
        (set-buffer-self-destruct current-buffer)
        (let ((prevbuf dired-buffer-to-go-back-to))
	   (select-buffer-window buffer nil)
	   (setq previous-buffer prevbuf)))

(defun dired-get-filename ()
        (go-to-end-of-line)
        (skip-back-whitespace)
        (with-mark m (go-to-beginning-of-line)
	           (go-to-hpos 20.)
		 (prog1 (point-mark-to-string m)
		        (go-to-beginning-of-line))))

(declare (defpl1 delete_$path "" (char (*))(char (*))(bit (6))(char (*))
	       (return fixed bin (35.))))


(defun dired-delete-files (seg-list)		;bsg 5/3/79 for delete_
       (let  ((err-list nil)
	    (code))
	   (mapc '(lambda (file)
		        (setq code (delete_$path dired-dir file
					   (lsh 44 30.) "emacs"))
		        (or (= 0 code)(setq err-list
				        (cons (cons file code)
					    err-list))))
	         seg-list)
	   (if (not (null err-list))
	       (init-local-displays)
	       (mapc 'local-display-generator-nnl
		   '("Errors encountered during deletions:"
		      "These files not deleted:"
		      ""))
	       (mapc '(lambda (x)(local-display-generator-nnl
			       (catenate
			         (e_lap_$trim
				 (cadr (convert_status_code_ (cdr x))))
			         " "
			         (car x))))
		   err-list)
	       (end-local-displays))))

(defun dired-access-check (dir)
       (let ((epr (expand_pathname_ dir)))
	  (let ((hcssr (hcs_$get_user_effmode (car epr)(cadr epr) ""
				        (cu_$level_get))))
	       (if (not (zerop (cadr hcssr)))
		 (if (= (cadr hcssr)(error_table_ 'incorrect_access))
		     (rplaca hcssr 77)
		     (display-error-noabort "Warning: cannot check access on " dir ".")
		     else
		     (display-com-error (cadr hcssr) dir)))
	       (let ((mode (car hcssr)))
		  (if (zerop (boole 1 mode 10))
		      (display-error "dired: Status permission on " dir " lacking."))
		  (not (zerop (boole 1 2 mode)))))))

;; 11/24/79 BSG

(declare (defpl1 hcs_$chname_file "" (char (*))(char (*))(char (*))(char (*))(return fixed bin (35.))))

(defcom dired-rename
        &prologue dired-m-access-check
        &arguments ((new &string &prompt
		     &eval (catenate "New name for " (dired-get-filename) ": ")))
        (let ((old (dired-get-filename)))
	   (let ((code (hcs_$chname_file dired-dir old old new)))
	        (or (zerop code)
		  (display-com-error code new))
	        (go-to-hpos 20.)
	        (without-modifying (without-saving (kill-to-end-of-line))
			       (insert-string new)
			       (go-to-beginning-of-line)))))

(defun dired-m-access-check ()
       (if (not (dired-access-check dired-dir))
	 (display-error "Modify access lacking on " dired-dir)))
