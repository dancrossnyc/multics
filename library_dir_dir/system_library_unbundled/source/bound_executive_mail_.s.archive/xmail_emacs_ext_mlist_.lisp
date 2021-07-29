;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-01-07,LJAdams), approve(86-03-19,MCR7358),
;;;     audit(86-04-18,RBarstad), install(86-05-28,MR12.0-1062):
;;;     Added change to display hyphens to delimit bottom window in editor.
;;;     Changed help screen formats as per MTB701.
;;;                                                      END HISTORY COMMENTS


;;;
;;; Author unknown
;;; Modified September 21, 1983 DJ Schimke to fix bad placement of error 
;;;   message in xmail:yesp when other than a valid yes/no response was 
;;;   given. phx 15964
;;;
;;; Modified October 20, 1983 DJ Schimke to add xmail:quit-handler so
;;;   hitting the quit key will prompt to be sure the user intends to
;;;   quit. phx13018 Also added a call to xmail_window_manager_$reconnect_test
;;;   so reconnection doesn't look like the BREAK key was hit. phx 13227
;;;
;;; 84-08-06 Davids: Added the ESC-< and ESC-> requests.
;;;
;;; 84-08-07 Davids: Added the ESC-? command. This required also adding the
;;; display-buffer-as-printout function and loadlib-ing the e_self_documentor_.
;;; help text for xmail:find-error, xmail:quit, and xmail-help were also added.
;;; ESC-<backspace> was also defined as rubout-word.
;;;
;;; 84-11-04 JG Backs: Deleted the commented out "loadlib 'e_macops_" entry
;;; because it was replaced by the display-buffer-as-printout function as
;;; mentioned in 84-08-07 history.  Also cleaned up the documentation for
;;; xmail:help and xmail:quit.  Audit change.

(%include e-macros)

(declare (special
           pop-up-windows
	 xmail-error-info-set
	 xmail-incorrect-line-list
	 xmail-mlist-suffix
	 xmail-code
           quit-handler-invoked
           test-str1))

(declare
  (*lexpr xmail:quit-handler)
  (*expr error-table kill-to-beginning-of-line minibuffer-clear-all
         quit-force save-same-file signalquit redisplay-command
         key-prompt get-key-binding get-key-name describe-internal))

(declare (defpl1 xmail_validate_$addr ""
	       (char(*)) (return (setq xmail-code) fixed bin(35.))))
;;; Test for reconnect.
(declare (defpl1 xmail_window_manager_$reconnect_test ""
	       (return bit (1) aligned)))

(setq rdis-splln-mark
      (cons
        (cons
	""
	(ncons nil))
        0))

(setq mode-line-hook 'xmail:mode-line)
(setq quit-handler-invoked nil)

(set-permanent-key "ESC-U"     'xmail:find-error)

(set-permanent-key "^F"	'forward-char)
(set-permanent-key "^B"	'backward-char)
(set-permanent-key "^P"	'prev-line-command)
(set-permanent-key "^N"	'next-line-command)
(set-permanent-key "^A"       'go-to-beginning-of-line)
(set-permanent-key "^E"       'go-to-end-of-line)
(set-permanent-key "ESC-N"    'next-screen)
(set-permanent-key "ESC-P"    'prev-screen)
(set-permanent-key "CR"	'new-line)
(set-permanent-key "ESC-F"	'forward-word)
(set-permanent-key "ESC-B"	'backward-word)
(set-permanent-key "^K"	'kill-lines)
(set-permanent-key "^Y"	'yank)
(set-permanent-key "\177"	'rubout-char)
(set-permanent-key ""	'rubout-char)  ;backspace
(set-permanent-key "#"	'rubout-char)
(set-permanent-key "^D"	'delete-char)
(set-permanent-key "@"	'kill-to-beginning-of-line)
(set-permanent-key "ESC-#"	'rubout-word)
(set-permanent-key "ESC-"	'rubout-word)
(set-permanent-key "ESC-"     'rubout-word)
(set-permanent-key "ESC-D"	'delete-word)
(set-permanent-key "\"	'escape-char)
(set-permanent-key "ESC-<"    'go-to-beginning-of-buffer)
(set-permanent-key "ESC->"    'go-to-end-of-buffer)
(set-permanent-key "ESC-R"	'redisplay-command)
(set-permanent-key "ESC-Q"	'xmail:quit)
(set-permanent-key "ESC-?"    'xmail:help)

       ;; Load in help package
       ;; This isn't necessary when e_self_documentor_ gets bound with emacs_
       (loadlib 'e_self_documentor_)


(defun xmail:start ()
       (sstatus interrupt 16. 'xmail:quit-handler)
       (setq xmail-mlist-suffix "mls")
       (setq xmail-incorrect-line-list nil)
       (setq xmail-error-info-set nil)
       (xmail:set-info-window))

(defun xmail:mode-line ()
       (list
         (catenate "Mailing list being edited:  <"
	         (or
		 (and (null fpathname) "None")
		 (xmail:get-ename-minus-suffix fpathname xmail-mlist-suffix))
	         ">   ---   Type ESC q to quit")))

(defun xmail:set-info-window ()
       (let ((temp-buf-value current-buffer))
	  (setq pop-up-windows t)
	  (select-buffer-window 'xmail-info 1.)
	  (without-modifying (insert-string "---Please Type One Address Per Line---"))
	  (setq pop-up-windows nil)
	  (find-buffer-in-window temp-buf-value)))

(defun xmail:set-error-info ()
       (if (not xmail-error-info-set)
	 (let ((temp-buf-value current-buffer))
	      (find-buffer-in-window 'xmail-info)
	      (without-modifying
	        (insert-string (catenate NL "---To find errors, type ESC u---")))
	      (find-buffer-in-window temp-buf-value))
	 (setq xmail-error-info-set t)))

(defun xmail:reset-error-info ()
       (if xmail-error-info-set
	 (let ((temp-buf-value current-buffer))
	      (find-buffer-in-window 'xmail-info)
	      (without-modifying
	        (kill-to-beginning-of-line)
	        (rubout-char))
	      (find-buffer-in-window temp-buf-value))
	 (setq xmail-error-info-set nil)))

(defcom xmail:quit ()
       &doc "This will verify the addresses in the mailing list and, if there
are no errors, it will write out the list and return to the menu. If there are
errors you will be prompted to see if you still want to quit. If you respond
with no, you can use the ESC u command to locate the addresses that could
not be verified."
       (go-to-end-of-buffer)
       (go-to-beginning-of-line)
       (delete-white-sides)
       (do-forever
         (if (at-beginning-of-buffer) (stop-doing))
         (prev-line)
         (delete-white-sides)
         (if (eolp) (delete-char)))
       (if (empty-buffer-p current-buffer) (signalquit))
;;	 (if (xmail:yesp "This mailing list is empty.  Do you still wish to quit?")

;;	     (signalquit)))
;;	     (quit-force)))
       (xmail:minibuffer-print "Verifying addresses ...")
       (setq xmail-incorrect-line-list (xmail:mark-incorrect-lines))
       (if (null xmail-incorrect-line-list)
	 (xmail:minibuffer-print "Verified.")
	 (save-same-file)
	 (quit-force)
	 else
	 (if (xmail:yesp "One or more addresses cannot be verified.  Do you still want to quit?")
	     (signalquit)
;;	     (quit-force)
	     else
	     (xmail:set-error-info))))

(defun xmail:mark-incorrect-lines ()
       (prog (error-list error-code error-msg)
	   (if (empty-buffer-p current-buffer)
	       (return nil))
	   (setq error-list nil)
	   (save-excursion
	     (go-to-beginning-of-buffer)
	     (do-forever
	       (setq error-code (xmail:error-on-current-line))
	       (cond
	         ((= error-code (error-table 'xmail_err_ 'mailing_list))
		(setq error-msg "An address in a mailing list cannot be the name of another mailing list."))
	         ((= error-code (error-table 'xmail_err_ 'bad_mailing_list))
		(setq error-msg "An address in a mailing list cannot be the name of another mailing list."))
	         ((= error-code (error-table 'mlsys_et_ 'invalid_address_syntax))
		(setq error-msg "This address is incorrectly specified."))
	         ((= error-code (error-table 'mlsys_et_ 'no_mailbox))
		(setq error-msg "There is no local mailbox corresponding to this address."))
	         ((= error-code (error-table 'mlsys_et_ 'no_a_permission))
                    (setq error-msg "You have not been given access to the mailbox corresponding to this address."))
	         ((= error-code (error-table 'mlsys_et_ 'duplicate_address))
		(setq error-msg "This address is duplicated in this mailing list."))
	         ((not (= error-code 0))
		(setq error-code (error-table 'xmail_err_ 'unrecognizable_addr))
		(setq error-msg "This line appears to be unrecognizable as an address.")))
	       (if (not (= error-code 0))
		 (setq error-list (xmail:add-to-list error-list (cons (set-mark) error-msg))))
	       (if (lastlinep)
		 (stop-doing)
		 else
		 (next-line))))
	   (return error-list)))

(defun xmail:error-on-current-line ()
       (cond
         ((line-is-blank) 0)
         (t (save-excursion
	    (go-to-end-of-line)
	    (delete-white-sides)
	    (go-to-beginning-of-line)
	    (delete-white-sides)
	    (let ((test-str (with-mark m
				 (go-to-end-of-line)
				 (point-mark-to-string m))))
	         (xmail_validate_$addr test-str)
	         (if (= xmail-code 0)
		   (go-to-beginning-of-line)
		   (if (reverse-search test-str)
		       (go-to-end-of-line)
		       (delete-white-sides)
		       (go-to-beginning-of-line)
		       (delete-white-sides)
		       (setq test-str1 (with-mark n 
			          (go-to-end-of-line)
				(point-mark-to-string n)))
		       (if (samepnamep test-str test-str1)
		       (setq xmail-code (error-table 'mlsys_et_ 'duplicate_address)))
		       else
		       (go-to-end-of-line)
		       (if (forward-search test-str)
		       (go-to-end-of-line)
		       (delete-white-sides)
		       (go-to-beginning-of-line)
		       (delete-white-sides)
		       (setq test-str1 (with-mark n 
			          (go-to-end-of-line)
				(point-mark-to-string n)))
		       (if (samepnamep test-str test-str1)
			 (setq xmail-code (error-table 'mlsys_et_ 'duplicate_address))))))))
	  xmail-code)))

(defun xmail:add-to-list (temp-list thing)
       (prog ()
	   (if (null temp-list)
	       (return (ncons thing)))
	   (if (null (cdr temp-list))
	       (return (rplacd temp-list (ncons thing))))
	   (return (cons (car temp-list) (xmail:add-to-list (cdr temp-list) thing)))))

(defcom xmail:find-error ()
       &doc "This will position the cursor to the start of the next line with
an address that could not be verified. This may be done only after the
addresses have been verified via the ESC q command."
       (do-forever				;do until we find an error
         (if (null xmail-incorrect-line-list)
	   (xmail:minibuffer-print "<No more errors>")
	   (xmail:reset-error-info)
	   (stop-doing)
	   else
	   (let ((line-in-error (caar xmail-incorrect-line-list))
	         (error-msg (cdar xmail-incorrect-line-list))
	         (error-code 0))
	        (setq xmail-incorrect-line-list (cdr xmail-incorrect-line-list))
	        (go-to-mark line-in-error)
	        (setq error-code (xmail:error-on-current-line))
	        (if (not (= error-code 0))	;we found our error
		  (release-mark line-in-error)
		  (xmail:minibuffer-print error-msg)
		  (ring-tty-bell)
		  (stop-doing))))))

(defun xmail:get-ename-minus-suffix (partial-pname suffix)
       (prog (i ename)
	   (do-forever
	     (if (or (null partial-pname) (samepnamep partial-pname ""))
	         (setq ename nil)
	         (stop-doing))
	     (setq i (index partial-pname ">"))
	     (if (= i 0)
	         (setq i (index partial-pname (catenate "." suffix)))
	         (if (= i 0)
		   (setq ename partial-pname)
		   (stop-doing)
		   else
		   (setq ename (substr partial-pname 1 (1- i)))
		   (stop-doing))
	         else
	         (setq partial-pname (substr partial-pname (1+ i)))))
	   (return ename)))

(defun xmail:yesp (prompt)
       (prog (response ret-value)
	   (minibuffer-clear-all)
	   (do-forever
	     (setq response (minibuf-response (catenate prompt "  ") NL))
	     (minibuffer-print "")
	     (cond ((or (samepnamep response "yes")
		      (samepnamep response "y"))
		  (setq ret-value t)(stop-doing))
		 ((or (samepnamep response "no")
		      (samepnamep response "n"))
		  (setq ret-value nil)(stop-doing))
		 (t (minibuffer-print "Please answer ""yes"" or ""no""."))))
	   (minibuffer-clear-all)
	   (return ret-value)))

(defun xmail:minibuffer-print (str)
       (minibuffer-clear-all)
       (minibuffer-print str))

;;; This handles the BREAK key and reconnection.
(defun xmail:quit-handler arg arg
       (cond (quit-handler-invoked)		     ;are we recursing?
	   ((not (zerop (xmail_window_manager_$reconnect_test)))
	    (redisplay-command))	               ;reconnect
	   (t (cond (buffer-modified-flag	     ;break key
		    (setq quit-handler-invoked t)  ;prevent recursion
		    (cond ((xmail:yesp "Any pending work will be lost. Do you really want to quit?  ")
			 (signalquit))	     ;yes, quit
			(t (setq quit-handler-invoked nil) ;no, not quitting
			   (command-quit))))
		  (t (signalquit))))))	     ;buffer not changed, quit

;;; For giving the user help via describe-key
(defcom xmail:help &numeric-argument (&reject)
        &doc "Allows you to get an explanation of a command.  Entering
a ""?"" will produce a table of all the valid commands with a very short
description."
        (let  ((suppress-minibuffer nil)) suppress-minibuffer
	    (let ((key1 (key-prompt "Enter a key sequence (or ? for summary): ")))
	       (cond
	         ((not (and (= (car key1) 0) (= (car (cdr key1)) 77)))
	           (let ((symbol (get-key-binding key1))
		   (description (get-key-name key1)))
		  (describe-internal description symbol
				 (catenate description "           " symbol))))
                   (t
		(init-local-displays)
		   (local-display-generator-nnl "(ESC x = press escape key then press x;   BS = BACKSPACE = CTL h;")
		   (local-display-generator-nnl " CTL x = CTRL = hold CONTROL key down while pressing x)")
                       (local-display-generator-nnl "@:  Erase to Start of Line CTLn: Next Line             ESCd: Erase Word")
                       (local-display-generator-nnl "BS:   Backward Erase Char  CTLp: Previous Line         ESCf: Forward Word")
                       (local-display-generator-nnl "CTLa: Start of Line        CTLy: Retrieve Erased Text  ESCn: Next Screen")
                       (local-display-generator-nnl "CTLb: Backward Char        ESC<: Start of Mailing List ESCp: Previous Screen")
                       (local-display-generator-nnl "CTLd: Erase Char           ESC>: End of Mailing List   ESCq: Finished with")
                       (local-display-generator-nnl "CTLe: End of Line          ESC?: Editor Help                 Mailing List")
                       (local-display-generator-nnl "CTLf: Forward Char         ESCb: Backward Word         ESCr: Redisplay")
                       (local-display-generator-nnl "CTLg: Exit help            ESCBS: Backward Erase Word  ESCu: Find Bad Addresses")
		   (local-display-generator-nnl "CTLk: Erase to End of Line")
		(end-local-displays))))
	  (xmail:mode-line)))

(defun display-buffer-as-printout ()
       (save-excursion
         (init-local-displays)
         (go-to-beginning-of-buffer)
         (do-forever
	 (local-display-generator (curline-as-string))
	 (if (lastlinep) (stop-doing))
	 (next-line))))

(xmail:start)
