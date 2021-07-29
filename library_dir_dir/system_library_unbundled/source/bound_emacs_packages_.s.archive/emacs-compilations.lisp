;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-04-23,Margolin), approve(86-04-23,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declarations so that it would compile without warnings.
;;;  2) change(86-11-24,Margolin), approve(87-01-27,MCR7607),
;;;     audit(87-02-13,RBarstad), install(87-03-25,MR12.1-1014):
;;;     To not use "file_output" buffer, and to add one-error-scan-buffer,
;;;     compile-two-windows, and compile-local-display options.
;;;  3) change(87-01-28,Margolin), approve(87-01-28,MCR7607),
;;;     audit(87-02-13,RBarstad), install(87-03-25,MR12.1-1014):
;;;     Added display-compilation-result.
;;;                                                      END HISTORY COMMENTS


;;;
(%include e-macros)
(%include other_other)


;;; Common part of error list processor.
;;; Paul Schauble, DVCP, Phoenix.

(declare (special error-list-builder e-list error-scan-buffer
	        mode-identification fill-prefix buffer-minor-modes
	        current-buffer buffer-uid compiler fpathname
	        compile-options two-window-mode number-of-lines-in-buffer))
(declare (*expr comout-to-buffer create-new-window-and-stay-here
	      redisplay-current-window-relative save-same-file))

(defvar ((compile-local-display nil)
         (compile-two-windows nil)
         (error-list '())
         error-source-buffer
         nuwindows
         (one-error-scan-buffer t)))

(defun locate-next-error ()
       (if (not (memq 'Error/ scan buffer-minor-modes))
	 (build-new-error-list))
       (if e-list		; Advance to next error
	 (let ((error-entry (car e-list)))
	      (save-excursion-buffer
	        (find-buffer-in-window error-scan-buffer)
	        (without-modifying
		(if-at '/=
		       (delete-char) (delete-char)
		       (insert-string "  "))
		(go-to-mark (car error-entry))
		(delete-char) (delete-char)
		(insert-string "=>"))
	        (go-to-beginning-of-line)
	        (redisplay-current-window-relative 0)
	        (find-buffer-in-window previous-buffer))
	      (let ((z (cdr error-entry)))
		 (if z (go-to-mark z))))
	 (setq e-list (cdr e-list))
	 else
	 (exit-error-scan-mode)
	 (display-error "No more errors.")))
		 
(defun build-new-error-list ()
       (if error-list			;  Wipe old mark list
	 (save-excursion-buffer
	   (go-to-buffer error-scan-buffer)
	   (exit-error-scan-mode)))
       (let ((crufty-error-list-builder error-list-builder)
	   ;; Ack! Not bound correctly in file_output!!!
	   (temp-error-list nil)
	   (other-buffer current-buffer)
	   (buffer-modified-flag t))
	  (save-excursion-buffer
	    (go-to-buffer error-scan-buffer)
	    (unless (eq error-source-buffer other-buffer)
		  (display-error "This buffer was not the last one compiled."))
	    (without-modifying
	      (go-to-end-of-buffer)
	      (setq temp-error-list (funcall crufty-error-list-builder)))
	    (if (not (symbolp temp-error-list))
	        (setq buffer-uid -143
		    read-only-flag t)
	        else
	        (setq buffer-uid 0)))
	  (setq error-list temp-error-list))	;get into this buffer's variable
       (cond
         ((null error-list)
	(display-error "No errors found."))
         ((eq error-list 'not-compile)
	(setq error-list nil)
	(display-error "Last comout was not a compilation."))
					; That error returned so that error-list-builder does
					; not command-quit while in file_output
         ;; The mark list is a list of (<mark in file_output> . <line num>)
         ;; convert the line num to a mark in the source buffer.
         (t
	 (go-to-beginning-of-buffer)
	 (let ((line 1) (target) (move))
	      (dolist (error-entry error-list)
		(setq target (cdr error-entry))
		(if target
		    (setq move (- target line))
		    (cond
		      ((= move 0))		; on that line now
		      ((> move 0)		; must advance
		       (do-times move (next-line)))
		      (t			; else move back
		        (do-times (- move)(prev-line))))
		    (setq line target)
		    (rplacd error-entry (set-mark)))))
					; error-list is now list of (<mark in file_output> . <mark in source>)
	 (setq e-list error-list)
	 (assert-minor-mode 'Error/ scan))))

       
 
(defun exit-error-scan-mode ()
       ; Must be in source buffer when called
       (dolist (error-entry error-list)
	     (and (cdr error-entry)
		(release-mark (cdr error-entry))))
       (save-excursion-buffer
         (go-to-buffer error-scan-buffer)
         (setq read-only-flag nil
	     buffer-uid 0
	     buffer-modified-flag t)
         (if-at '/=
	      (delete-char) (delete-char)
	      (insert-string "  "))
         (dolist (error-entry error-list)
	       (release-mark (car error-entry)))
         (setq buffer-modified-flag nil))
       (negate-minor-mode 'Error/ scan)
       (setq error-list nil
	   e-list nil))

;;;
;;; Conditional new line, does new line and insert special prefix
;;;   if the current line has significant contents, it will be used.
;;;   Note that the new prefix must be an arg, since this needs both 
;;;   the old and new values.

(defun conditional-new-line (pfx)
       (go-to-beginning-of-line)
       (if (or (line-is-blank)
	     (and (looking-at fill-prefix)
		(= curlinel (1+ (stringlength fill-prefix)))))
	 (without-saving (kill-to-end-of-line))
	 else
	 (go-to-end-of-line)
	 (let ((fill-prefix "")) (new-line)))
       (if pfx (insert-string pfx)))


;;;
;;; Fortran compilatons, January 29, 1979, by Paul Schauble
;;;

(defun compile-buffer ()
       (if buffer-modified-flag (save-same-file))
       (mapc 'register-local-var
	   '(error-scan-buffer error-list e-list))
       (if error-list (exit-error-scan-mode))
       (let ((compile-command
	     (catenate compiler " " fpathname " " compile-options))
	   (curbuf current-buffer)
	   (type-buffer-expected mode-identification))
	  (setq error-scan-buffer
	        (cond (one-error-scan-buffer '|Compilation Errors|)
		    (t (make_atom
		         (catenate current-buffer " Errors")))))
	  (minibuffer-print compile-command "<>")
	  (and compile-two-windows
	       (< nuwindows 2)
	       (create-new-window-and-stay-here))
	  (comout-to-buffer error-scan-buffer compile-command)
	  (register-local-var 'error-source-buffer)
	  (setq error-source-buffer curbuf)
	  (setq buffer-uid type-buffer-expected)
	  (cond ((> nuwindows 1))		;display in other window
	        (compile-local-display	;local display the errors
		(display-buffer-as-printout)
		(end-local-displays))
	        (t (display-compilation-result)))    ;local display success/failure
	  (find-buffer-in-window curbuf)))

(defun display-compilation-result ()
       (init-local-displays)
       (local-display-generator
         (cond ((> number-of-lines-in-buffer 2)
	      (catenate (decimal-rep number-of-lines-in-buffer)
		      " lines of compilation errors were generated."))
	     (t "No compilation errors were generated.")))
       (end-local-displays))

(defun set-compiler (comp)
       (setq compiler (catenate comp " ")))

(defun set-compile-options n
       (setq compile-options "")
       (do i 1 (1+ i) (> i n)
	 (setq compile-options (catenate compile-options " " (arg i)))))

