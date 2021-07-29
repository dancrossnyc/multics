;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-02-10,LJAdams), approve(86-02-25,MCR7361),
;;;     audit(86-04-17,Margolin), install(86-08-20,MR12.0-1136):
;;;     EMACS extension to add history comments while within emacs.
;;;  2) change(86-04-21,Margolin), approve(86-04-21,MCR7361),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added *expr declaration for requote_string_.  Made add-history-comment
;;;     write out the buffer first.
;;;  3) change(86-05-04,Margolin), approve(86-05-04,MCR7361),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Changed hcom:get-summary to skip back over trailing whitespace in the
;;;     summary.  Changed hcom:add-hcom-apv to use without-saving around its
;;;     wipe-point-mark.
;;;                                                      END HISTORY COMMENTS

(%include e-macros)

(declare (*expr
    create-new-window-and-go-there          buffer-kill
    fill-mode			    delete-window
    go-to-or-create-buffer		    minibuffer-clear-all
    redisplay-command		    reverse-string-search
    requote_string_ save-same-file
    redisplay-current-window-relative))

(defcom-synonym add-hcom add-history-comment)

(defvar hcom:apv-value)
(defvar hcom:cfix)
(defvar hcom:ctl-arg)
(defvar hcom:orig-buffer)
(defvar hcom:pathname)
(defvar hcom:summary)
(defvar nuwindows)

(defcom add-history-comment
        &doc "Adds a history comment to the current source program."
        (setq hcom:orig-buffer current-buffer)
        (if (not fpathname) 
            (display-error "Current buffer has not been written to a file."))
        (if buffer-modified-flag
	  (save-same-file))
        (setq hcom:pathname fpathname)
        (create-new-window-and-go-there)	;if user has one window split the screen
					;if user has multiple windows dont destroy what he has
       (go-to-or-create-buffer 'hcom/ Result/ Buffer)
       (fill-mode)
       (setq fill-column 72.)
       (minibuffer-print "Type in summary.  Type ^X^S to end summary")
       (set-key '^X^S 'hcom:get-summary))

(defcom hcom:get-summary ()
       &doc "Gets the summary and approve value."
       (minibuffer-clear)
       (go-to-beginning-of-buffer)
       (skip-over-whitespace)
       (go-to-beginning-of-line)		;skip any leading blank lines
       (with-mark mark
	        (go-to-end-of-buffer)
	        (skip-back-whitespace)	;trim any trailing whitespace
	        (setq hcom:summary
		    (requote_string_ (point-mark-to-string mark))))
       (hcom:get-approve-value))

(defun hcom:get-approve-value ()
    (setq hcom:apv-value (trim-minibuffer-response "Type approve value:  "))
    (if (samepnamep hcom:apv-value "")		;null string treated as no approve value
       (hcom:put-hcom-napv)
       else
       (requote_string_ hcom:apv-value)
       (if (samepnamep "fix_" (substr hcom:apv-value 1 4))
	 (setq hcom:cfix t)
	 (setq hcom:ctl-arg " -cfix")
           else
	 (setq hcom:ctl-arg "")
           (setq hcom:cfix nil))
       (hcom:put-hcom-apv)))

(defun hcom:put-hcom-apv ()
   (hcom:add-hcom-apv t)
   (go-to-beginning-of-buffer)
   (if (eolp)				;hcom returns a blank line if there is no error
       (if hcom:cfix			;no database checking is done for cfixes
	 (hcom:display)
           else
	 (if (yesp "OK?")			;user verifies if mcr number is all right
	     (hcom:add-hcom-apv nil)
	     (if (not (eolp))		;error was found
	              (find-buffer-in-window hcom:orig-buffer)
		    (display-buffer-as-printout)
		    (end-local-displays)
		    (hcom:error)
		    else
		    (hcom:display))
	     else
	     (hcom:get-approve-value)))		;wrong mcr entered
       else
       (hcom:error)))
 
(defun hcom:display ()
       (if (> nuwindows 1)
	 (select-other-window)
	 (delete-window nuwindows))
       (minibuffer-clear-all)
       (read-in-file hcom:pathname)
       (if (not (forward-search "END HISTORY COMMENTS"))
	 (go-to-beginning-of-buffer))
       (redisplay-current-window-relative -1)
       (buffer-kill 'hcom/ Result/ Buffer))

(defun hcom:error ()
       (ring-tty-bell)
       (if (yesp "re-enter approve value?")
	 (hcom:get-approve-value)
	 else
	 (hcom:display)))

(defun hcom:add-hcom-apv (get-question)
   (go-to-or-create-buffer hcom:orig-buffer)       
   (comout-get-output
     (if get-question
         "answer no"  
         else
         "answer yes -brief")
     (requote_string_
       (catenate
         "history_comment add "
         (requote_string_ hcom:pathname)
         " -approve "
         hcom:apv-value
         " -summary "
         hcom:summary
         hcom:ctl-arg)))

   (if get-question
       (go-to-end-of-buffer)
       (if (reverse-string-search "OK?")
           (go-to-beginning-of-line)
           (with-mark mark
                      (go-to-end-of-buffer)
                      (without-saving
		    (wipe-point-mark mark))))
       (display-buffer-as-printout)
       (end-local-displays)))

(defun hcom:put-hcom-napv ()
       (go-to-or-create-buffer hcom:orig-buffer)
       (comout-get-output "history_comment add "
		      (requote_string_ hcom:pathname)
		      " -no_approve -summary "
		      hcom:summary)

       (go-to-beginning-of-buffer)
       (if (eolp)
	 (hcom:display)
	 else
	 (hcom:error))) 
