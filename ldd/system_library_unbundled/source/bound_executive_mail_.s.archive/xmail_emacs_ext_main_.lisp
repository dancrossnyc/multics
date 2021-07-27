;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Bull Inc., 1987                *
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(86-01-07,Blair), approve(86-02-26,MCR7358),
;;;     audit(86-04-18,RBarstad), install(86-05-28,MR12.0-1062):
;;;     85-05-10 Backs, Barstad, Davids, Dixon: Changed calls to replace-field
;;;     so that the name of the field ALWAYS terminates in a : character. This
;;;     prevents complete hose mode when the contents of a previous field contains
;;;     a string which is the same as the current field name minus the colon, i.e.
;;;     the To: field contains a person.project of XXX.Mcc screws things up when
;;;     the cc field is processed.
;;;     86-1-6 C Spitzer: Added check for 0 length reply segment in reply-mode.
;;;  2) change(86-01-07,LJAdams), approve(86-02-26,MCR7358),
;;;     audit(86-04-18,RBarstad), install(86-05-28,MR12.0-1062):
;;;     Added change to display hyphens to delimit bottom window in editor.
;;;     Changed help screen formats as per MTB701.
;;;  3) change(86-02-26,Blair), approve(86-02-26,MCR7358),
;;;     audit(86-04-18,RBarstad), install(86-05-28,MR12.0-1062):
;;;     Put a command-quit at the end of the code which is executed after a
;;;     reconnect to bring processing back into the edit loop. TR 19420
;;;  4) change(86-06-25,Blair), approve(86-07-15,MCR7447),
;;;     audit(86-07-16,LJAdams), install(86-07-21,MR12.0-1100):
;;;     Change xmail:replace-field to look for header keywords at the start of
;;;     lines only. TR 20269.
;;;  5) change(86-06-26,Blair), approve(86-07-15,MCR7447),
;;;     audit(86-07-16,LJAdams), install(86-07-21,MR12.0-1100):
;;;     Position cursor to end of original during reply.  Error_list #125.
;;;  6) change(86-10-15,Blair), approve(86-10-15,MCR7564),
;;;     audit(86-10-28,RBarstad), install(86-10-29,MR12.0-1201):
;;;     Make the parse-address-list skip over quoted strings so that it can
;;;     correctly determine what is a single address to send to
;;;     xmail_validate_.  Fixes error 129, TR 20591.
;;;  7) change(87-08-10,Blair), approve(87-12-10,MCR7818),
;;;     audit(87-12-23,LJAdams), install(88-01-12,MR12.2-1013):
;;;     Add processing for a reply-to field when sending mail.
;;;                                                      END HISTORY COMMENTS

;;;
;;; Notes:
;;;    84-08-09 Davids: Need to investigate which modes are really used
;;;    and remove references to and code for unused modes.
;;;
;;;    84-08-14 JG Backs: Refer to history of 84-08-14. If there are
;;;    no problems after a couple of months, completely delete the 
;;;    commmented out call and the two functions.
;;;
;;; Emacs extension to implement Executive Mail functions.
;;; Richard Mark Soley, July/August 1981
;;; Modified September 1981 RMSoley for bug fixes/small changes.
;;; Modified September/October 1981 RMSoley for key binding changes,
;;;	many changes, support tasking.
;;; Modified November 1981 RMSoley for new reply mode.
;;;
;;; Modified September 21, 1983 R Harvey to merge kills on kill ring 
;;;       for xmail:rubout-word. phx11987
;;;
;;; Modified October 20, 1983 DJ Schimke to add code to xmail:quit-handler so
;;;   hitting the quit key will prompt to be sure the user intends to
;;;   quit. phx13018 Also added a call to xmail_window_manager_$reconnect_test
;;;   so reconnection doesn't look like the BREAK key was hit. phx 13227
;;;   replaced all calls to yesp with xmail:yesp for consistency with the
;;;   mailing list extension.
;;;
;;; Modified February 1, 1984 DJ Schimke to make minibuffer at least 2 lines.
;;;    (3 lines during header error processing)
;;; 
;;; Modified February 6, 1984 DJ Schimke to check all recipient addresses when
;;; entering in EDIT mode (replying or sending defered). phx12960 phx12677
;;; 
;;; To replace the ESC-G (get file) functionallity,
;;; delete every occurence of the string ";*ESC-G*" in this file.
;;;
;;; 84-08-03 Davids: Modified xmail:go-to-beginning-of-buffer to check to see
;;; if it's in a minbuffer and if it is to execute the standard
;;; go-to-beginning-of-buffer command. This prevents a null pointer fault.
;;;
;;; 84-08-06 Davids: Modified xmail:go-to-beginning-of-buffer so that it called
;;; the standard go-to-beginning-of-buffer function if the change-header buffer
;;; is on the screen. If it is on the screen it means that the user is changing
;;; the header and the standard function is the one to be used. Also removed
;;; the *ESC-G* comments which allows the file insertion capability that Dave
;;; implemented. Changed the prompt to include the information that a null
;;; file name would cause an abort.
;;;
;;; 84-08-07 Davids: Added the general-help function. this required loadlib-ing
;;; e_macops_ and e_self_documentor_ as well as adding help text for each
;;; redefined command. Also defined the ESC-^H key sequence. Finally changed
;;; all the instructions output in xmail:instructions to include ESC ? and
;;; removed references to ESC g, which I do not think belonged in the
;;; instructions. The loadlibing of e_macops_ was replaced with the function
;;; definition of display-buffer-as-printout. This was the only function in
;;; e_macops_ that was referenced.
;;;
;;; 84-08-09 Davids: Added code for handling blind carbon copies. This
;;; consisted of duplicating the code for carbon copies.
;;;
;;; 84-08-14 JG Backs: Commented out call to xmail:fill-in-blanks in the
;;; function xmail:get-fields.  This was done to prevent "<None> <None>"
;;; from appearing when cc or bcc was blank in reply mode.  After a few months
;;; of no problem use, the call should be deleted along with the two functions
;;; xmail:fill-in-blanks and xmail:fill-in-one.
;;;
;;; 84-10-15 Davids: Added code to xmail:general-help so that if help is
;;; requested while in the minibuffer, i.e. answering a prompt, the correct set
;;; of requests is displayed. Also so that the summary is automatically 
;;; displayed. This gets around an emacs bug that causes emacs to go into hosed
;;; mode when there is an attempt to get help for a key while in the minibuffer
;;;
;;; 84-11-04 JG Backs: Corrected misspelled word in history section, took out
;;; one of the double references to minibufferp in Global variables, cleaned 
;;; up and rewrote some of the documentation to the user, and shortened part
;;; of the minibuffer-print line to say "ESC t to defer". Audit change.
;;;
;;; 84-11-26 Davids: Added a test to the xmail:refill function so that it
;;; calls xmail:beginning-of-paragraph only if it is not already at the
;;; the beginning of the paragraph. The beginning of paragraph function will
;;; move you to the beginning of the next paragraph if you are at the beginning
;;; of the current paragraph. This fixes TR18523.
;;;
;;; 85-01-04 JG Backs: Added an "or" test in xmail:general-help so that the
;;; full help summary for send-mode is displayed if send-mode or edit-mode
;;; (deferred messages).  Only the first two lines were being displayed in
;;; edit-mode.  Bugfix.
;;;
;;; Load in the necessary include files.
(%include e-macros)  
(%include backquote)

;;; To use in debug mode, remove the semicolon at the beginning of this line:
;(sstatus feature debug)

(declare
  ;; These are functions defined elsewhere in Emacs.
  (*lexpr minibuffer-response xmail:minibuffer-response report-error-noabort
          xmail:quit-handler)
  (*expr e_lap_$rtrim emacs$get_info_ptr error-table eval-lisp-line file-insert
         fill-current-line fill-mode kill-to-beginning-of-line lowercase-ttp
         mark-at-current-point-p minibuffer-clear-all signalquit buffer-kill
         prev-line-command quit-force redisplay-command save-same-file
         search-not-charset-forward set-minibuffer-size user_info_$homedir
         eval:internal exists-file set-permanent-key e_lap_$return-string
         set-the-mark beginning-of-paragraph runoff-fill-region get_pdir_
         end-of-paragraph expand-window-to-whole-screen rdis-choose-echo-linex
         create-new-window-and-go-there find-file-subr 
         at-beginning-of-paragraph substr 
         emacs$set_emacs_return_code loadlib 
         key-prompt get-key-binding get-key-name describe-internal)

;;; Global variables.
  (special xmail:subject-text xmail:cc-text xmail:bcc-text xmail:to-text xmail:reply-to-text
	 xmail:markers xmail:message-mark xmail:header-info xmail:mode
	 xmail:sleep xmail:silent-instructions known-buflist
	 paragraph-definition-type whitespace-charactertbl minibufferp
	 mode-line-hook fill-prefix suppress-minibuffer no-minibuffer-<>
           suppress-remarks quit-on-break buffer-creation-hook
           default-fill-column rdis-splln-mark screenheight selected-window
           message quit-handler-invoked rdis-mbuf-transient-linex
           minibufwindow xmail:reply-segment-bc nuwindows xmail:dashes 
	 DASHES screenlinelen DOUBLEQUOTE)

;;; Validate a single address.
  (defpl1 xmail_validate_$addr "" (char (*)) (return fixed bin (35.)))
;;; Get the user's name.
  (defpl1 user_info_ ""  (return char (22)))
;;; Test for reconnect.
  (defpl1 xmail_window_manager_$reconnect_test "" (return bit (1) aligned)))

(eval-when (eval compile)
	 ;; Force output to the minibuffer.
	 (defun force-minibuffer-print macro (form)
	        `(let ((suppress-minibuffer nil))
		    suppress-minibuffer
		    (minibuffer-print . ,(cdr form))))
	 ;; Macro-defining-macro for mode checking.
	 (defun defmode macro (form)
	        `(defun ,(make_atom (catenate (cadr form) "-mode"))
		      macro (form)
		      '(eq xmail:mode ',(cadr form))))

	 ;; Define the known modes.
	 (defmode send)
;;	 (defmode store)
	 (defmode fwd)
	 (defmode fwd-comment)
	 (defmode reply)
	 (defmode edit)
	 (defmode send-from-file)
	 (defmode prob-rept)
	 )

(defun xmail:setup ()
       (expand-window-to-whole-screen)

       ;;; Set the internal options of Emacs.
       (or (status feature debug)
	 (setq mode-line-hook 'xmail:mode-line	;Empty mode line.
	       paragraph-definition-type 2	;The "right" one.
	       suppress-minibuffer t		;No minibuffer output.
	       no-minibuffer-<> t		;No "<>" after mbuf input.
	       suppress-remarks t		;No Reading/Writing messages.
	       default-fill-column 72.	;Fill column for all buffers.
	       fill-column 72.		;Fill column for this buffer.
	       quit-on-break t		;Quit Emacs on BREAK.
	       quit-handler-invoked nil         ;Not yet
	       buffer-creation-hook 'xmail:turn-on-fill  ;Turn on fill mode.
	       ))

       ;;; Set internal variables to xmail.
       (setq xmail:to-text nil
	   xmail:cc-text nil
	   xmail:bcc-text nil
             xmail:reply-to-text nil
	   xmail:subject-text nil
	   xmail:silent-instructions t
	   xmail:sleep 3
	   xmail:markers nil)

       (setq DASHES "-----------------------------------------------------------------------------------------------------------------------------------------------------------------------")

       ;;; Set these keys for all buffers.
       (mapc '(lambda (x) (set-permanent-key (car x) (cadr x)))
	   '(
	   ("ESC-N"	next-screen)
	   ("ESC-P"	prev-screen)
	   ("^F"		forward-char)
	   ("^B"		backward-char)
	   ("^P"		prev-line-command)
	   ("^N"		next-line-command)
	   ("^Y"		yank)
	   ("CR"		new-line)
	   ("ESC-F"	forward-word)
	   ("ESC-B"	backward-word)
	   ("^K"		kill-lines)
	   ("\177"	rubout-char)
	   (""	          rubout-char) ;backspace
	   ("#"		rubout-char)
	   ("^D"		delete-char)
	   ("@"		kill-to-beginning-of-line)
	   ("ESC-#"	rubout-word)
	   ("ESC-"	rubout-word)
	   ("ESC-^H"	rubout-word)
	   ("ESC-D"	delete-word)
	   ("\"		escape-char)
	   ("ESC-R"	xmail:redisplay-command)
	   ("ESC-Q"	xmail:finished)

	   ("^A"		go-to-beginning-of-line)
	   ("^E"		go-to-end-of-line)
	   ("ESC-<"	xmail:go-to-beginning-of-buffer)
	   ("ESC->"	go-to-end-of-buffer)
	   ("ESC-?"         xmail:general-help)
	   ))

       ;; Load in help package
       ;; This isn't necessary when e_self_documentor_ gets bound with emacs_
       (loadlib 'e_self_documentor_)

       ;; If we are debugging, set ESC-ESC.

       (and (status feature debug)
	  (set-permanent-key "ESC-ESC" 'xmail:debugger)))

;;; Function to create empty mode line.
(defun xmail:mode-line ()
       (setq xmail:dashes (substr DASHES 1 screenlinelen))
       (list  xmail:dashes ""))

;;; ^L: redisplay full screen and print instructions.
(defcom xmail:redisplay-command
        &doc "Clears and then redisplays the text being worked on. The text
will be redisplayed so that the current line is centered in the window."
        (redisplay-command)
        (xmail:instructions))

;;; Turn on fill mode.
(defun xmail:turn-on-fill (n) n (fill-mode))

;;; Main function.  Do housekeeping, call correct mode function.
(defun xmail:start ()
       (xmail:setup)
       (fill-mode)
;;; depending on the size of the user_io window, set the size of the minibuffer
       (cond ((not (< screenheight 13.)) (set-minibuffer-size 5))
	   ((= screenheight 12.) (set-minibuffer-size 4))
	   ((= screenheight 11.) (set-minibuffer-size 3))
	   (t (set-minibuffer-size 2)))
       (setq xmail:mode
	   (make_atom
	     (e_lap_$rtrim
	       (e_lap_$return-string (emacs$get_info_ptr) 0 32.))))
       (or (reply-mode) (edit-mode) (fwd-mode)
	 (fwd-comment-mode) (destroy-buffer-contents))
       (go-to-beginning-of-buffer)
       (or (reply-mode)(edit-mode) (redisplay))
       (go-to-end-of-buffer)
       (or (= (cur-hpos) 0) (reply-mode) (edit-mode) (new-line))
       (setq xmail:silent-instructions t)
       (setq xmail:reply-segment-bc
	   (xmail:get-segment-bc process-dir "view_reply_seg"))
       (cond ((send-mode) (xmail:send))
;;	   ((store-mode) (xmail:store))
	   ((fwd-mode) (xmail:fwd))
	   ((fwd-comment-mode) (xmail:fwd-comment))
	   ((reply-mode) (xmail:reply))
	   ((edit-mode) (xmail:edit))
	   ((send-from-file-mode) (xmail:send-from-file))
	   ((prob-rept-mode) (xmail:problem-report)))
       (setq xmail:silent-instructions nil)
       (or (reply-mode) (edit-mode) (send-from-file-mode)
	 (line-is-blank) (new-line))
       (cond ((fwd-mode) (save-same-file) (xmail:quit-force)))
       (setq xmail:message-mark (set-mark))
       (set-minibuffer-size 3)
       (and (or xmail:subject-text xmail:to-text xmail:cc-text xmail:bcc-text
                xmail:reply-to-text)
	  (set-key "ESC-U"	'xmail:change-header))
       (or (fwd-mode)
	 (set-key "ESC-G"	'xmail:insert-file))

       (cond ((reply-mode)
	     (set-key "ESC-H"  'xmail:page-other-window)
	     (set-key "ESC-L"  'xmail:unpage-other-window)
	     (go-to-end-of-buffer)))

       (set-key "ESC-M"	'xmail:refill)
       (set-key "^B"	'xmail:backward-char)
       (set-key "^P"	'xmail:prev-line-command)
       (set-key "ESC-B"	'xmail:backward-word)
       (set-key "\177"	'xmail:rubout-char)
       (set-key ""	          'xmail:rubout-char) ;backspace
       (set-key "#"		'xmail:rubout-char)
       (set-key "ESC-#"	'xmail:rubout-word)
       (set-key "ESC-\177"	'xmail:rubout-word)
       (set-key "ESC-^H"	'xmail:rubout-word)
       (set-key "ESC-P"	'xmail:prev-screen)
       (xmail:instructions))

;;; Main function for SEND mode.
(defun xmail:send ()
       (set-key "ESC-T"  'xmail:finished-defer)
       (xmail:redisplay-command)
       (setq xmail:subject-text
	   (xmail:get-field "Enter Subject (optional): " "Subject:" "subject" nil t))
       (setq xmail:reply-to-text (e_lap_$rtrim (user_info_)))
       (xmail:insert-field "Reply-To:" "")
       (setq xmail:to-text
	   (xmail:get-field "Enter recipient(s): " "To:" "to" t t))
       (setq xmail:cc-text
	   (xmail:get-field "Enter cc (optional): " "cc:" "cc" t t))
       (setq xmail:bcc-text "")
       (xmail:insert-field "bcc:" "")
       (insert-string "Message:"))

;(defun xmail:store ()
;       (xmail:redisplay-command)
;       (setq xmail:subject-text
;	   (xmail:get-field "Enter Subject (optional): " "Subject:" "subject" nil t))
;       (setq xmail:to-text
;	   (xmail:get-field "Enter recipient(s) (optional): " "To:" "to" t t))
;       (setq xmail:cc-text
;	   (xmail:get-field "Enter cc (optional): " "cc:" "cc" t t))
;       (insert-string "Message:"))

;;; Main function for SEND-FROM-FILE mode.
(defun xmail:send-from-file ()
       (setq xmail:subject-text
	   (xmail:get-field "Enter Subject: " "Subject:" "subject" nil t))
       (setq xmail:to-text
	   (xmail:get-field "Enter recipient(s): " "To:" "to" t nil))
       (setq xmail:cc-text
	   (xmail:get-field "Enter cc: " "cc:" "cc" t t))
       (insert-string "Message:")
       (new-line)
       (save-excursion
         (file-insert (xmail:get-good-file "Get message from file: "))))

;;; Main function for FORWARD-WITH-COMMENT mode.
(defun xmail:fwd-comment ()
       (setq xmail:to-text
	   (xmail:get-field "Forward to: " "To:" "to" t nil))
       (setq fill-column 62.)		;Fill column for comments
       (insert-string "Comment (optional):"))

;;; Main function for REPLY mode.
(defun xmail:reply ()
;;       (setq rdis-splln-mark
;;          (cons
;;	     (cons
;;	       "----- Enter F6 to view previous page of reply, F7 to view next -----"
;;	       (ncons nil))
;;	     0))
       (if (> xmail:reply-segment-bc 0)
	 (let ((this-buffer current-buffer))
	      (create-new-window-and-go-there)
	      (go-to-or-create-buffer this-buffer))
	 (select-other-window)
	 (find-file-subr
	   (catenate (e_lap_$rtrim (get_pdir_)) ">view_reply_seg"))
	 (select-other-window)
	 )
       (go-to-beginning-of-buffer)
       (redisplay)
       (go-to-end-of-buffer)
       (setq xmail:reply-to-text (e_lap_$rtrim (user_info_)))
       (xmail:edit))

;;; Main function for EDIT mode.
(defun xmail:edit ()
       (set-key "ESC-T"  'xmail:finished-defer)
       (xmail:get-fields)
       (next-line)
       (go-to-beginning-of-line))

;;; Main function for FORWARD mode.
(defun xmail:fwd ()
       (setq xmail:to-text
	   (xmail:get-field "Forward to: " "To:" "to" t nil)))

;;; Main function for PROBLEM-REPORT mode.
(defun xmail:problem-report ()
       (insert-string
         "Please describe the Executive Mail problem you are having:"))

;;; Utility routine to get the bit count of a segment
;;;    (use the lsh (left-shift) function to prepare the bit strings to the PL/1
;;;    functions as Lisp passes weird bit strings by default).
(declare
  (defpl1 initiate_file_ ""
	(char (*))			;directory
	(char (*))			;entry
	(bit (36.))			;mode
	(return ptr)			;segment pointer
	(return fixed bin (24.))		;bit count
	(return fixed bin (35.)))		;code
  (defpl1 terminate_file_ ""
	(ptr)				;segment pointer
	(fixed bin (24.))			;bit count
	(bit (36.))			;switches
	(return fixed bin (35.)))		;ignore the returned code
  )

(defun xmail:get-segment-bc (dir entry)
       (prog (initiate-result terminate-result)
	   (setq initiate-result
	         (initiate_file_
		 dir			;containing directory
		 entry			;segment name
		 (lsh 1 35.)))		;read mode (0 bit on)
	   (if (not (= 0 (caddr initiate-result)))   ;3rd arg is the code
	       (return -1))			;some error, cannot get the bc
	   (setq terminate-result
	         (terminate_file_
		 (car initiate-result)	;1st arg is the segment pointer
		 0			;bit count to set
		 (lsh 1 33.)))		;only terminate the segment (bit 2 on)
	   (if (not (= 0 terminate-result))
	       (return -1))
	   (return (cadr initiate-result)))     ;2nd arg is the bit count
       )

;;; These functions replace the normal Emacs functions to make sure
;;; that the user cannot touch the message header.

;;; Replaces ^B command.
(defcom xmail:backward-char &numeric-argument (&repeat)
        &doc "Moves backwards one character in the buffer. Tabs and newline 
characters at the end of lines count as single characters.
$$$ will not allow you to stray backwards into the header."
        (or (mark-at-current-point-p xmail:message-mark)
	  (let ((numarg nil)) (backward-char))))

;;; Replaces ^P command.
(defcom xmail:prev-line-command &numeric-argument (&repeat)
        &doc "Move to previous line of buffer. $$$
will attempt to stay in the same horizontal position. It will not allow
you to stray back into the header."
        (or (mark-on-current-line-p xmail:message-mark)
	  (let ((numarg nil)) (prev-line-command))))

;;; Replaces ESC-B command.
(defcom xmail:backward-word &numeric-argument (&repeat)
        &doc "Moves backwards one word in the buffer.
$$$ will not allow you to stray backwards into the header."
        (let ((numarg nil)) (backward-word))
        (or (point>markp xmail:message-mark)
	  (go-to-mark xmail:message-mark)))

;;; Replaces ESC-< command
(defcom xmail:go-to-beginning-of-buffer &numeric-argument (&reject)
        &doc "Moves back to the beginning of the message - i.e.
to the beginning of the first line after the Message: header. If you are
answering a prompt, the cursor will move to the first character of your
answer."
        (cond (minibufferp (go-to-beginning-of-buffer))
	    ((buffer-on-display-in-window 'change-header) (go-to-beginning-of-buffer))
	    (t (go-to-mark xmail:message-mark))))

;;; Replaces ESC-V command.
(defcom xmail:prev-screen &numeric-argument (&repeat)
        &doc "Displays the previous screen (one back) of
this buffer, and leaves the cursor sitting either at the top of the screen
or immediately after the header, whichever is appropriate. A numeric 
argument (e.g. ESC 5 $$$) will move back that many screens."
        (cond ((mark-on-current-line-p xmail:message-mark))
	    ('else (prev-screen)
		 (or (point>markp xmail:message-mark)
		     (go-to-mark xmail:message-mark)))))

;;; ESC-^V
(defcom xmail:page-other-window
        &doc "Displays the next screen (one forward) of the mail
to which you are replying. A numeric argument
(e.g. ESC 5 $$$) will move forward that many screens."
        &numeric-argument (&pass)
        (let ((origwindow selected-window))
	   (unwind-protect
	     (progn
	       (select-other-window)
	       (if (null numarg)(next-screen)
		 else (if (> numarg 0)(next-screen)
			else (setq numarg (- numarg))
			(prev-screen))))
	     (select-window origwindow))))

;;; -1 ESC-^V.
(defcom xmail:unpage-other-window &numeric-argument (&pass)
        &doc "Displays the previous screen (one backward) of the mail
to which you are replying. A numeric argument
(e.g. ESC 5 $$$) will move backward that many
screens."
        (let ((numarg (- (or numarg 1))))
	   (xmail:page-other-window)))

;;; Replaces \177, # command.
(defcom xmail:rubout-char &numeric-argument (&repeat)
        &doc "Deletes the previous character - i.e. the one to the
left of the cursor. $$$ will not let you delete any of the header."
        (or (mark-at-current-point-p xmail:message-mark)
	  (let ((numarg nil))  (rubout-char))))

(defun defkill macro (form) `(defprop ,(cadr form) ,(caddr form) kills))
(defkill xmail:rubout-word reverse)

;;; Replaces ESC-\177, ESC-# command.
(defcom xmail:rubout-word
        &doc "Deletes the word to the left of the cursor. More specifically
deletes characters backwards until the beginning of the word. 
$$$ will not allow you to delete back into the header. Successive $$$s are
merged and may be retrieved with a single ^Y."
        &numeric-argument (&repeat)
        &negative-function delete-word
        (with-mark here
	         (backward-word)
	         (cond ((point>markp xmail:message-mark))
		     (t
		       (go-to-mark xmail:message-mark)))
	         (kill-forward-to-mark here)
	         (merge-kills-reverse)))

;;; For re-filling a region.
(defcom xmail:refill
        &doc "Fills (reformats) the current paragraph, lining up left margin."
        (save-excursion
	(cond ((not (at-beginning-of-paragraph)) (xmail:beginning-of-paragraph)))
	(set-the-mark)
	(end-of-paragraph)
	(without-saving (runoff-fill-region))))

;;; Go to beginning, but NOT INTO HEADER FIELDS!
(defun xmail:beginning-of-paragraph ()
       (beginning-of-paragraph)
       (or (point>markp xmail:message-mark)
	 (go-to-mark xmail:message-mark)))

;;; For inserting files into the buffer.
(defcom xmail:insert-file
        &doc "Will insert a file
at the current cursor position."
        (save-excursion
	(file-insert (xmail:get-good-file "Get message from file (just press RETURN to abort): ")))
        (xmail:instructions))

;;; Pick up fields from the buffer.
(defun xmail:get-fields ()
       (go-to-beginning-of-buffer)
       (do ((contents "")
	  (field ""))
	 ((or (looking-at "Message:")
;;;	      (looking-at "Comment (optional):")
	      (looking-at "Reply:"))
	  (xmail:interpret-field field contents)
;;;	  (xmail:fill-in-blanks)
	  (redisplay))
	 (cond ((looking-at TAB)
	        (forward-char)
	        (with-mark here
		         (go-to-end-of-line)
		         (setq contents
			     (catenate contents
				     (point-mark-to-string here)))))
	       (t (xmail:interpret-field field contents)
		(with-mark begin
			 (forward-search ":")
			 (setq field (point-mark-to-string begin)))
		(forward-char)
		(with-mark here
			 (go-to-end-of-line)
			 (setq contents (point-mark-to-string here)))))
	 (next-line) (go-to-beginning-of-line)))


;;; Interpret contents of field and setq appropriate variable.
(defun xmail:interpret-field (name contents)
       (setq contents (e_lap_$trim contents))
       (cond ((nullstringp name))
	   ((samepnamep name "To:")
	    (setq xmail:to-text (xmail:correct "to" contents t))
	    (xmail:replace-field "To:" xmail:to-text))
	   ((samepnamep name "Subject:")
	    (setq xmail:subject-text contents)
	    (xmail:replace-field "Subject:" xmail:subject-text))
	   ((samepnamep name "bcc:")
	    (setq xmail:bcc-text (xmail:correct "bcc" contents t))
	    (xmail:replace-field "bcc:" xmail:bcc-text))
             ((samepnamep name "Reply-To:")
              (setq xmail:reply-to-text (xmail:correct "Reply-To" contents t))
              (xmail:replace-field "Reply-To:" xmail:reply-to-text))
	   ((samepnamep name "cc:")
	    (setq xmail:cc-text (xmail:correct "cc" contents t))
	    (xmail:replace-field "cc:" xmail:cc-text))))

;;; Display set of instructions in the minibuffer.
(defun xmail:instructions ()
       (let ((suppress-minibuffer nil)) suppress-minibuffer
	  (cond ((or minibufferp xmail:silent-instructions))
	        ((eq current-buffer 'change-header)
	         (minibuffer-clear-all)
	         (minibuffer-print
		 (cond ((samepnamep (car xmail:header-info) "Subject")
		        "Entering or modifying the ""Subject"" text.")
		       ((samepnamep (car xmail:header-info) "To")
		        "Entering or modifying the ""To"" recipient(s).")
		       ((samepnamep (car xmail:header-info) "bcc")
		        "Entering or modifying the ""bcc"" recipient(s).")
		       ((samepnamep (car xmail:header-info) "Reply-To")
		        "Entering or modifying the ""Reply-To"" recipient(s).")
		       ((samepnamep (car xmail:header-info) "cc")
		        "Entering or modifying the ""cc"" recipient(s).")))
	         (minibuffer-print "Enter ESC ? for help    ESC q when done."))
	        ((fwd-mode)
	         (minibuffer-clear-all)
	         (minibuffer-print "Enter ESC ? for help    ESC q when done.")
	         (minibuffer-print "Enter ESC u to change header."))
	        ((edit-mode)
	         (minibuffer-clear-all)
	         (minibuffer-print "Enter ESC ? for help    ESC q to send     ESC t to defer.")
	         (minibuffer-print "Enter ESC u to enter/change header fields."))
	        ((send-mode)
	         (minibuffer-clear-all)
	         (minibuffer-print "Enter ESC ? for help    ESC q to send     ESC t to defer.")
	         (minibuffer-print "Enter ESC u to enter/change header field."))
	        ((reply-mode)
	         (minibuffer-clear-all)
	         (minibuffer-print "Enter ESC ? for help    ESC q to send reply     ESC t to defer reply.")
	         (minibuffer-print "Enter ESC u to change header."))
	        ((not (or xmail:subject-text xmail:to-text xmail:reply-to-text xmail:cc-text xmail:bcc-text))
	         (minibuffer-clear-all)
	         (minibuffer-print "Enter ESC ? for help    ESC q when done."))
	        (xmail:subject-text
		(minibuffer-clear-all)
		(minibuffer-print "Enter ESC ? for help   ESC q when done.")
		(minibuffer-print "Enter ESC u to change ""Subject"", "
			        """To:"", or ""cc:"""))
	        (xmail:cc-text
		(minibuffer-clear-all)
		(minibuffer-print "Enter ESC ? for help   ESC q when done.")
		(minibuffer-print "Enter ESC u to change "
			        "recipients or cc."))
	        (t
		(minibuffer-clear-all)
		(minibuffer-print "Enter ESC ? for help   ESC q when done.")
		(minibuffer-print "Enter ESC u to change ""To:""."))
	        )))

;;; Get the contents of a header field from the user, checking
;;; for validity if necessary, and giving help if asked for.
(defun xmail:get-field (prompt title printable check? allow-blank)
       (minibuffer-clear-all)
       (do ((ans (xmail:minibuffer-response prompt)
	       (xmail:minibuffer-response prompt)))
	 (())
	 (setq ans (e_lap_$trim ans))
	 (cond ((and (samepnamep (substr ans 1 1) "?")
		   (member printable '("to" "cc" "bcc" "reply-to")))
	        (xmail:help printable))
	       ((not check?) (return (xmail:insert-field title ans)))
	       ((and allow-blank (nullstringp ans))
	        (return (xmail:insert-field title "")))
	       ((nullstringp ans)
	        (force-minibuffer-print
		"At least one recipient required. For help enter ? and RETURN"))
	       (t (return
		  (xmail:insert-field
		    title
		    (xmail:correct printable ans allow-blank)))))))

;;; Check field, gather corrections.
(defun xmail:correct (printable field allow-blank)
       (do ((answers (xmail:parse-address-list field) (cdr answers))
	  (string ""))
	 ((null answers)
	  (xmail:instructions)
	  (cond ((and (not allow-blank) (nullstringp string))
	         (force-minibuffer-print "You must enter at least one """
				   printable ":"" address.")
	         (xmail:correct printable
			    (xmail:minibuffer-response
			      "Please enter address: " NL "")
			    allow-blank))
	        (t string)))
	 (let ((fixed (xmail:correct-one printable
				   (car answers)
				   (or (cdr answers)
				       allow-blank
				       (not (nullstringp string))))))
	      (or (nullstringp fixed)
		(setq string
		      (catenate string
			      (cond ((nullstringp string) "") (t ", "))
			      fixed))))))

;;; Check one address for consistency, get new if necessary.
;;; Added ability to enter > 1 address, with commas.
(defun xmail:correct-one (printable string allow-blank)
       (let ((list (xmail:parse-address-list string)))
	  (cond (list (xmail:solidify
		      (mapcar '(lambda (x) (xmail:single printable
						 x allow-blank))
			    list)))
	        (t (xmail:single printable "" allow-blank)))))

;;; Solidify a list of addresses into a string with commas.
(defun xmail:solidify (list)
       (do ((answer (car list) (cond ((nullstringp (car l)) answer)
			       ((nullstringp answer) (car l))
			       (t (catenate answer ", " (car l)))))
	  (l (cdr list) (cdr l)))
	 ((null l) (or answer ""))))

;;; Check a single address, no commas, for correctness.
(defun xmail:single (printable ans allow-blank)
       (setq ans (e_lap_$trim ans))
       (let ((code (xmail_validate_$addr ans)))
	  (cond ((and allow-blank (nullstringp ans)) ans)
	        ((nullstringp ans) ans)
	        ((zerop code) ans)
	        (t (minibuffer-clear-all)
;;;		 (setq rdis-mbuf-transient-linex (+ 2 (car minibufwindow)))
		 (xmail:set-minibuffer-line 2)
		 (cond ((= code (error-table 'mlsys_et_ 'invalid_address_syntax))
		        (force-minibuffer-print
			"Address incorrectly specified. For help enter ? and RETURN.")
		        (xmail:need-correction printable ans allow-blank))
		       ((= code (error-table 'mlsys_et_ 'no_mailbox))
		        (force-minibuffer-print
			"The name and/or project is not known. For help enter ? and RETURN")
		        (xmail:need-correction printable ans allow-blank))
		       ((= code (error-table 'mlsys_et_ 'mte_not_found))
		        (force-minibuffer-print
			"The address, or mailing list, is not known. For help enter ? and RETURN")
		        (xmail:need-correction printable ans allow-blank))
		       ((= code (error-table 'mlsys_et_ 'no_a_permission))
		        (force-minibuffer-print
			"You cannot send mail to the """ printable ":"" address.")
		        (xmail:need-correction printable ans allow-blank))
		       ((= code (error-table 'xmail_err_ 'mailing_list))
		        (xmail:expand-mailing-list ans))
		       (t (let ((suppress-minibuffer nil))
;;;		      suppress-minibuffer
			     (report-error-noabort code))
			(xmail:need-correction printable ans allow-blank)))))))

(defun xmail:set-minibuffer-line (lineno)
       (setq rdis-mbuf-transient-linex (+ lineno (car minibufwindow))))       

;;; Get new response from user for correction.
(defun xmail:need-correction (printable string blank)
       (xmail:correct-one printable
		      (xmail:get-an-entry printable string)
		      blank))

;;; Get one entry, doing help call if user wants.
(defun xmail:get-an-entry (printable string)
       (let ((prompt
	     (catenate "Please correct this """ printable ":"" address: ")))
	  (do ((in (e_lap_$trim (xmail:minibuffer-response prompt NL string))
		 (e_lap_$trim (xmail:minibuffer-response prompt NL new)))
	       (new string (substr in 2)))
	      ((not (= (index in "?") 1)) in)
	      (xmail:help printable))))

;;; Insert field title and contents into buffer.
(defun xmail:insert-field (title contents)
       (let ((under (- 9. (stringlength title))))
	  (insert-string title)
	  (with-mark
	    end-of-title
	    (insert-string (substr "__________" 1 under))
	    (insert-string " ")
	    (insert-string contents)
	    (do ((fill-prefix "	"))
	        ((not (> (cur-hpos) fill-column)))
	        (setq fill-prefix fill-prefix)
	        (fill-current-line))
	    (cond ((line-is-blank) (kill-to-beginning-of-line))
		(t (new-line)))
	    (save-excursion
	      (go-to-mark end-of-title)
	      (do-times under (delete-char))
	      (insert-string (substr "          " 1 under))
	      (and (nullstringp contents)
		 (xmail:insert-blank-info title)))))
       (or (fwd-mode) (redisplay))
       contents)

;;; Finished entire message; write out and punt if all is OK.
(defcom xmail:finished
        &doc "Sends the mail and returns to the Executive Mail menu."
        (emacs$set_emacs_return_code 0)
        (and minibufferp (command-quit))
        (go-to-mark xmail:message-mark)
        (cond ((fwd-comment-mode)
	     (xmail:remove-markers)
	     (save-same-file)
	     (xmail:quit-force))
	    ((nullstringp xmail:to-text)
	     (and (xmail:yesp "There are no primary recipients.  Quit? ")
		(xmail:exit))
	     (xmail:instructions)
	     (command-quit))
	    ((search-not-charset-forward whitespace-charactertbl)
	     (xmail:remove-markers)
	     (save-same-file)
	     (xmail:quit-force))
	    ((xmail:yesp "There is no message.  Quit? ")
	     (xmail:exit))
	    (t (xmail:instructions)
	       (command-quit))))

(defcom xmail:finished-d-reply
        (emacs$set_emacs_return_code 2)
        (and minibufferp (command-quit))
        (go-to-mark xmail:message-mark)
        (cond ((nullstringp xmail:to-text)
	     (and (xmail:yesp "There are no primary recipients.  Quit? ")
		(xmail:exit))
	     (command-quit))
	    ((search-not-charset-forward whitespace-charactertbl)
	     (xmail:remove-markers)
	     (save-same-file)
	     (xmail:quit-force))
	    ((xmail:yesp "There is no reply.  Quit? ")
	     (xmail:exit))
	    (t (xmail:instructions)
	       (command-quit))))

(defcom xmail:finished-defer
        &doc "Returns to the Executive Mail menu. The mail you were working on
is not sent, it is saved as ""defered mail""."
        (emacs$set_emacs_return_code 1)
        (and minibufferp (command-quit))
        (go-to-mark xmail:message-mark)
        (cond ((search-not-charset-forward whitespace-charactertbl)
	     (xmail:remove-markers)
	     (save-same-file)
	     (xmail:quit-force))
	    ((xmail:yesp "There is no text. Do you wish to quit? ")
	     (xmail:exit))
	    (t (xmail:instructions)
	       (command-quit))))

;;; For giving the user help via describe-key
(defcom xmail:general-help &numeric-argument (&reject)
        &doc "Will prompt you for the command you want help with. Entering
a ""?"" will produce a table of all the valid commands with a very short
description."
        (let  ((suppress-minibuffer nil)) suppress-minibuffer
	    (cond (minibufferp
		 (init-local-displays)
		 (local-display-generator-nnl "(ESC x = press escape key then press x;   BS = BACKSPACE = CTL h;")
		 (local-display-generator-nnl " CTL x = hold CONTROL key down while pressing x)")
		 (local-display-generator-nnl "@:    Erase to Start of Line   CTLf:  Forward Char          ESCBS: Backward")
                     (local-display-generator-nnl "BS:   Backward Erase Char      CTLg:  Exit Help                    Erase Word")
                     (local-display-generator-nnl "CTLa: Start of Line            CTLk:  Erase to End of Line  ESCd:  Erase Word")
                     (local-display-generator-nnl "CTLb: Backward Char            CTLy:  Retrieve Erased Text  ESCf:  Forward Word")
                     (local-display-generator-nnl "CTLd: Erase Char               ESC?:  Editor Help           ESCr:  Redisplay")
                     (local-display-generator-nnl "CTLe: End of Line              ESCb:  Backward Word")
		 (end-local-displays))
		(t
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
			 (local-display-generator-nnl " CTL x = hold CONTROL key down while pressing x)")
			 (cond
			   ((buffer-on-display-in-window 'change-header)
		 (local-display-generator-nnl "@: Erase to Start of Line CTLg: Exit Help            ESCb:  Backward Word ")
		 (local-display-generator-nnl "BS: Backward Erase Char   CTLk: Erase to End of Line ESCBS: Backward Erase Word")
		 (local-display-generator-nnl "CTLa: Start of line       CTLp: Previous Line        ESCn:  Next Screen")
		 (local-display-generator-nnl "CTLb: Backward Char       CTLy: Retrieve Erased Text ESCp:  Previous Screen")
		 (local-display-generator-nnl "CTLd: Erase Char          ESC<: Start of Header      ESCq:  Update Header Info")
		 (local-display-generator-nnl "CTLe: End of Line         ESC>: End of Header        ESCr:  Redisplay")
                     (local-display-generator-nnl "CTLf: Forward Char        ESC?: Editor Help"))
			   ((or (send-mode) (edit-mode))
			    (local-display-generator-nnl "@:  Erase to Start of Line  CTLn: Next Line             ESCf: Forward word")
			    (local-display-generator-nnl "BS:   Backward Erase Char   CTLp: Previous Line         ESCg: Get File")
			    (local-display-generator-nnl "CTLa: Start of Line         CTLy: Retrieve Erased Text  ESCm: Adjust Paragraph")
			    (local-display-generator-nnl "CTLb: Backward Char         ESC?: Editor Help           ESCn: Next Screen")
			    (local-display-generator-nnl "CTLd: Erase Char            ESC<: Start of Message      ESCp: Previous Screen")
			    (local-display-generator-nnl "CTLe: End of Line           ESC>: End of Message        ESCq: Send Message")
			    (local-display-generator-nnl "CTLf: Forward Char          ESCb: Backward Word         ESCr: Redisplay")
			    (local-display-generator-nnl "CTLg: Exit Help             ESCBS: Backward Erase Word  ESCt: Defer Message")
			    (local-display-generator-nnl "CTLk: Erase to End of Line  ESCd: Erase Word            ESCu: Change Subject or")
                                  (local-display-generator-nnl "                                                              Recipients"))
			   ((reply-mode)
			    (local-display-generator-nnl "@: Erase to Start of Line   CTLp: Previous Line         ESCh: Next Message Page")
			    (local-display-generator-nnl "BS: Backward Erase Char     CTLy: Retrieve Erased Text  ESCl: Previous Msg Page")
			    (local-display-generator-nnl "CTLa: Start of Line         ESC?: Editor Help           ESCm: Adjust Paragraph")
			    (local-display-generator-nnl "CTLb: Backward Line         ESC<: Start of Reply        ESCn: Next Screen")
			    (local-display-generator-nnl "CTLd: Erase Char            ESC>: End of Reply          ESCp: Previous Screen")
			    (local-display-generator-nnl "CTLe: End of Line           ESCb: Backward Word         ESCq: Send Reply")
			    (local-display-generator-nnl "CTLf: Forward Char          ESCBS: Backward Erase Word  ESCr: Redisplay")
			    (local-display-generator-nnl "CTLg: Exit Help             ESCd: Erase Word            ESCt: Defer Reply")
			    (local-display-generator-nnl "CTLk: Erase to End of Line  ESCf: Forward Word          ESCu: Change Recipients")
			    (local-display-generator-nnl "CTLn: Next Line             ESCg: Get File"))
			   ((fwd-comment-mode)
			    (local-display-generator-nnl "@: Erase to Start of Line   CTLn:  Next Line             ESCf: Forward Word")
			    (local-display-generator-nnl "BS: Backward Erase Char     CTLp:  Previous Line         ESCg: Get File")
			    (local-display-generator-nnl "CTLa: Start of Line         CTLy:  Retrieve Erased Text  ESCm: Adjust Paragraph")
			    (local-display-generator-nnl "CTLb: Backward Char         ESC?:  Editor Help           ESCn: Next Screen")
			    (local-display-generator-nnl "CTLd: Erase Char            ESC<:  Start of Comment      ESCp: Previous Screen")
			    (local-display-generator-nnl "CTLe: End of Line           ESC>:  End of Comment        ESCq: Forward Message")
			    (local-display-generator-nnl "CTLf: Forward Char          ESCb:  Backward Word         ESCr: Redisplay")
			    (local-display-generator-nnl "CTLg: Exit Help             ESCBS: Backward Erase Word   ESCu: Change Recipients")
			    (local-display-generator-nnl "CTLk: Erase to End of Line  ESCd:  Erase Word")))
			 (end-local-displays))))))
	    (xmail:instructions)))

(defun display-buffer-as-printout ()
       (save-excursion
         (init-local-displays)
         (go-to-beginning-of-buffer)
         (do-forever
	 (local-display-generator (curline-as-string))
	 (if (lastlinep) (stop-doing))
	 (next-line))))

;;; Local display help for recipient fields.
(defun xmail:help (which)
       (init-local-displays)
       (mapc
         'local-display-generator-nnl
         (cond ((samepnamep which "bcc")
	 '(
	 "Type the names of people whom you want to receive copies of your"
	 "message. When listing recipients, you can use a user name and"
	 "project (e.g., Smith.Finance), and/or the name of a mailing list"
           "(e.g., managers). Names must be separated by commas. You need"
           "not enter any recipients here. Conclude by typing RETURN (or its "
           "equivalent on your keyboard)."
	 ))
	 ((samepnamep which "cc")
	 '(
	 "Type the names of people whom you want to receive copies of your"
	 "message. When listing recipients, you can use a user name and"
	 "project (e.g., Smith.Finance), and/or the name of a mailing list"
           "(e.g., managers). Names must be separated by commas. You need"
           "not enter any recipients here. Conclude by typing RETURN (or its "
           "equivalent on your keyboard)."
            ))
	 ((samepnamep which "reply-to")
	 '(
           "Type the names of people whom you want to receive a reply to this"
	 "message. When listing recipients, you can use a user name and"
	 "project (e.g., Smith.Finance), and/or the name of a mailing list"
           "(e.g., managers). Names must be separated by commas. You need"
           "not enter any recipients here. If you do not enter any names, any"
           "replies will be sent to you. Conclude by typing RETURN (or its "
           "equivalent on your keyboard)."
            ))
	 ((or (fwd-mode) (fwd-comment-mode))
	  '(
	 "Type the names of people whom you want to receive copies of your"
	 "message. When listing recipients, you can use a user name and"
	 "project (e.g., Smith.Finance), and/or the name of a mailing list"
           "(e.g., managers). Names must be separated by commas. You must"
           "enter at least one recipient here. Conclude by typing RETURN (or"
	 "its equivalent on your keyboard)."
	  ))
	  (t
	  '(
	 "Type the names of people whom you want to receive copies of your"
	 "message. When listing recipients, you can use a user name and"
	 "project (e.g., Smith.Finance), and/or the name of a mailing list"
           "(e.g., managers). Names must be separated by commas. You must"
           "enter at least one recipient here if you wish to send the message."
           "Conclude by typing RETURN (or its equivalent on your keyboard)."
	  ))
	  ))
       (end-local-displays))

;;; Get a filename of a file that exists and can be read.
(defun xmail:get-good-file (prompt)
       (do ((name (xmail:minibuffer-response prompt)
	        (xmail:minibuffer-response prompt)))
	 ((xmail:good-file? name) name)))

;;; Check a file for validity: can we read it?
(defun xmail:good-file? (name)
       (let ((suppress-minibuffer nil))
	  suppress-minibuffer
	  (setq name (e_lap_$trim name))
	  (cond ((nullstringp name)
	         (minibuffer-print "Aborting file read.")
	         (sleep xmail:sleep)
	         (xmail:instructions)
	         (command-quit)))
	  (let ((exists? (catch (exists-file name 4) pgazonga)))
	       (cond ((null exists?)
		    (minibuffer-print "File not found.")
		    (ring-tty-bell)
		    nil)
		   ((atom exists?) nil)
		   (t t)))))

;;; Make change prompt.
(defun xmail:make-prompt ()
       (let ((string ""))
	  (and xmail:subject-text (setq string (catenate string "subj,")))
	  (and xmail:to-text (setq string (catenate string "to,")))
	  (and xmail:cc-text (setq string (catenate string "cc,")))
	  (and xmail:bcc-text (setq string (catenate string "bcc,")))
            (and xmail:reply-to-text (setq string (catenate string "reply-to,")))
	  (cond ((nullstringp string) (command-quit))
	        (t (catenate "What do you wish to modify? ("
			 (substr string 1 (1- (stringlength string)))
			 "): ")))))

;;; Prompt for which field wants to be changed, parse answer.
(defun xmail:which-field (prompt)
       (cond ((zerop (index prompt ",")) (substr prompt 30. 1))
	   (t (do ((ans (xmail:minibuffer-response prompt)
		      (xmail:minibuffer-response prompt)))
		(())
		(setq ans (e_lap_$trim ans))
		(cond ((nullstringp ans)
		       (xmail:instructions)
		       (command-quit)))
		(setq ans (substr (lowercase-ttp ans) 1 1))
		(and (or (and (samepnamep ans "s") xmail:subject-text)
		         (and (samepnamep ans "c") xmail:cc-text)
		         (and (samepnamep ans "b") xmail:bcc-text)
		         (and (samepnamep ans "r") xmail:reply-to-text)
		         (and (samepnamep ans "t") xmail:to-text))
		     (return ans))
		(force-minibuffer-print
		  "Incorrect entry. Try again or enter RETURN to stop."
		  )))))

;;; Top level function for changing a header field.
(defcom xmail:change-header
        &doc "This command allows you to change header information, i.e. the
subject, to, reply-to, cc, and bcc fields."
        (let ((which (xmail:which-field (xmail:make-prompt))))
	   (go-to-mark xmail:message-mark)
	   (go-to-or-create-buffer 'change-header)
	   (cond ((samepnamep which "s")
		(xmail:header "Subject" xmail:subject-text t))
	         ((samepnamep which "t")
;;		(xmail:header "To" xmail:to-text (reply-mode)))
		(xmail:header "To" xmail:to-text t))
	         ((samepnamep which "b")
		(xmail:header "bcc" xmail:bcc-text t))
	         ((samepnamep which "r")
		(xmail:header "Reply-To" xmail:reply-to-text t))
	         ((samepnamep which "c")
		(xmail:header "cc" xmail:cc-text t)))))

;;; Prepare header-editing buffer for changing.
(defun xmail:header (name old allow-blank)
       (destroy-buffer-contents)
       (insert-string old)
       (go-to-beginning-of-buffer)
       (setq xmail:header-info (cons name allow-blank))
       (xmail:instructions)
       (set-key "ESC-Q" 'xmail:finish-new-header))

;;; Finished editing header field.  Clean up and install.
(defcom xmail:finish-new-header
        &doc "Finishes the update of header and returns to message."
        (and minibufferp (command-quit))
        (go-to-beginning-of-buffer)
        (do () ((lastlinep))
	  (go-to-end-of-line)
            (insert-char ",")
	  (delete-char))
        (let ((name (car xmail:header-info))
	    (blank? (cdr xmail:header-info))
	    (contents (e_lap_$trim (curbuf-as-string))))
	   (cond ((samepnamep name "Subject"))
	         ((and blank? (nullstringp contents)))
	         ((nullstringp contents)
		(force-minibuffer-print
		  "At least one recipient must be specified.")
		(command-quit))
	         (t (minibuffer-clear-all)
		  (rdis-choose-echo-linex)
		  (setq contents
		        (xmail:correct (cond ((samepnamep name "To") "to")
				         ((samepnamep name "Reply-To") "reply-to")
				         ((samepnamep name "bcc") "bcc")
				         (t "cc"))
				   contents blank?))))
	   (cond ((samepnamep name "Subject") (setq xmail:subject-text contents))
	         ((samepnamep name "To") (setq xmail:to-text contents))
	         ((samepnamep name "bcc") (setq xmail:bcc-text contents))
	         ((samepnamep name "Reply-To") (setq xmail:reply-to-text contents))
	         ((samepnamep name "cc") (setq xmail:cc-text contents)))
	   (go-to-buffer previous-buffer)
	   (save-excursion
	     (xmail:replace-field (catenate name ":") contents)
	     (xmail:instructions))))

;;;Replace the title and contents of a field in the buffer.
(defun xmail:replace-field (title contents)
         (go-to-beginning-of-buffer)
         (forward-search title)
         (go-to-beginning-of-line)
         (do () (())
	   (cond ((looking-at title)
		(return t))
	         (t (next-line)
		  (forward-search title)
		  (go-to-beginning-of-line))))
	   
         (with-mark
	 here
	 (next-line)
	 (do () (())
	     (cond ((mark-on-current-line-p xmail:message-mark)
		  (go-to-beginning-of-line)
		  (backward-char) (insert-string NL)
		  (return t))
		 ((not (looking-at TAB)) (return t))
		 (t (next-line))))
	 (without-saving (wipe-point-mark here)))
         (xmail:insert-field title contents))









;;; Parse address list string into a list of addresses.
(defun xmail:parse-address-list (string)
       (mapcar
         'e_lap_$trim
         (do ((string string)
	    (l ())
	    (append "")
	    (inparen nil))
	   ((nullstringp string)
	    (nreverse (cond ((nullstringp append) l)
			(t (cons append l)))))
	   (let ((comma (index string ","))
	         (quotemark (index string DOUBLEQUOTE))
	         (paren (index string "("))
	         (closeparen (index string ")")))
	        (cond
		((and (not (zerop quotemark)) (< quotemark comma) (or(eq paren 0) (< quotemark paren))(or (eq closeparen 0)(< quotemark closeparen)))
		 (let ((closequote (+ (index (substr string (1+ quotemark))
					   DOUBLEQUOTE) quotemark)))
     		      (cond ((zerop closequote)
			   (setq l (cons (catenate append string) l)
			        string ""
			        append ""))
			  (t (setq append (catenate append (substr string 1 closequote))
				 string (substr string (1+ closequote))
				 ) ))))

		((zerop comma)
		 (setq l (cons (catenate append string) l)
		       append ""
		       string ""))

		((and (not inparen) (or (zerop paren) (< comma paren)))
		 (if (> comma 1)
		     (setq l (cons (catenate append (substr string 1 (1- comma))) l)
			 append ""
			 string (substr string (1+ comma)))
		     else (setq l (cons append l)
		                append ""
			      string (substr string (1+ comma)))))

		(t (cond ((zerop closeparen)
			(setq l (cons (catenate append string) l)
			     string ""
			     append ""))

		         ((not inparen)
			(if (and (> quotemark 0)(< quotemark closeparen))
			    (setq append (catenate append (substr string 1 (1- quotemark)))
				string (substr string quotemark)
				inparen t)
			    else
			    (setq append (catenate append (substr string 1 closeparen))
				string (substr string (1+ closeparen))
				inparen nil)) )

		         (t (setq append (catenate append (substr string 1 closeparen))
			        string (substr string (1+ closeparen))
			        inparen nil)))
		 )))))))
		     

;;; Returns full pathname of mailing list from mailing list name.
(defun xmail:mailing-list-path (name)
       (catenate (e_lap_$rtrim (user_info_$homedir)) ">"
	       (let ((name (status uname)))
		  (substr name 1 (1- (index name ".")))) ".mlsys>"
	       (e_lap_$rtrim name) ".mls"))

;;; Inserts the contents of a mailing list.
(defun xmail:expand-mailing-list (name)
       (let ((string (xmail:expand-mailing-list-internal name)))
	  (let ((sl (stringlength string)))
	       (cond ((zerop sl) string)
		   ((samepnamep (substr string (1- sl) 2) ", ")
		    (substr string 1 (- sl 2)))
		   (t string)))))
;;; Expands mailing list into standard mlsys address form.
;;;(defun xmail:expand-mailing-list (name)
;;;       (catenate "{list " (xmail:mailing-list-path name) "}"))

(defun xmail:expand-mailing-list-internal (name)
       (without-modifying
         (with-mark
	 begin
	 (file-insert (xmail:mailing-list-path name))
	 (with-mark
	   end
	   (go-to-mark begin)
	   (do () ((mark-on-current-line-p end))
	       (go-to-end-of-line)
	       (insert-string ", ")
	       (delete-char))
	   (go-to-mark end)
	   (prog1 (point-mark-to-string begin)
		(without-saving (wipe-point-mark begin)))))))

;;; Insert a string at point, remember it's there for remove-markers.
(defun xmail:insert-marker (string)
       (setq xmail:markers
	   (cons (list (set-mark) string (stringlength string))
	         xmail:markers))
       (insert-string string))

;;; Remove all such markers from the buffer.
(defun xmail:remove-markers ()
       (save-excursion
         (mapc 'xmail:remove-one-marker xmail:markers)
         (setq xmail:markers ())))

;;; Remove a single marker from the buffer.
(defun xmail:remove-one-marker (mark)
       (go-to-mark (car mark))
       (and (looking-at (cadr mark))
	  (do-times (caddr mark) (delete-char)))
       (release-mark (car mark)))

;;; Insert a marker saying that field is blank.
(defun xmail:insert-blank-info (title)
       (let ((insert
	     (cond ((samepnamep title "Subject:") "<No Subject>")
		 ((samepnamep title "To:") "<None>")
		 ((or (and (samepnamep title "To:") (reply-mode))
		      (samepnamep title "cc:") (samepnamep title "bcc:")) "<None>")
		 ((samepnamep title "Reply-To:") (catenate "<" (e_lap_$rtrim (user_info_)) ">"))
		 (t nil))))
	  (cond ((null insert))
	        ((edit-mode) (reply-mode)
	         (xmail:insert-marker insert))
	        (t (xmail:insert-marker (catenate " " insert))))))

;;; Fill in blanks when starting up EDIT mode.
(defun xmail:fill-in-blanks ()
       (and xmail:subject-text (nullstringp xmail:subject-text)
	  (xmail:fill-in-one "Subject:"))
       (and xmail:to-text (nullstringp xmail:to-text)
	  (xmail:fill-in-one "To:"))
       (and xmail:bcc-text (nullstringp xmail:bcc-text)
	  (xmail:fill-in-one "bcc:"))
       (and xmail:cc-text (nullstringp xmail:cc-text)
	  (xmail:fill-in-one "cc:")))

;;; Fix up one blank field.
(defun xmail:fill-in-one (title)
       (save-excursion
         (go-to-beginning-of-buffer)
         (forward-search title)
         (go-to-end-of-line)
         (xmail:insert-blank-info title)))

(defun xmail:minibuffer-response lexpr
       (prog1 (apply 'minibuffer-response (listify lexpr))
	    (xmail:set-minibuffer-line 1)
;;;	    (setq rdis-mbuf-transient-linex (+ 1 (car minibufwindow)))
))

;;; These three little horrible things make sure that all XMAIL buffers
;;; are flushed so that tasking Emacs will not encounter them again.

;;; This one handles plain old ESC Q.
(defun xmail:quit-force ()
       (xmail:hidey-hole-trick)
       (quit-force))

;;;This one handles normal exit via ESC Q
(defun xmail:exit ()
       (xmail:hidey-hole-trick)		;normal exit
       (signalquit))

;;; This one handles the BREAK key.
(defun xmail:quit-handler arg arg
       (cond (quit-handler-invoked)		;are we recursing?
	   ((not (zerop (xmail_window_manager_$reconnect_test)))
	    (xmail:redisplay-command)
	    (command-quit))	;reconnect
	   (t (cond (buffer-modified-flag	;break key
		    (setq quit-handler-invoked t)  ;prevent recursion
		    (cond ((xmail:yesp "Any pending work will be lost. Do you really want to quit?  ")
			 (xmail:hidey-hole-trick)	;yes, quit
			 (signalquit))
			(t (xmail:instructions)  ;no, not quitting
			   (setq quit-handler-invoked nil)
			   (command-quit))))
		  (t (signalquit))))))	     ;buffer not changed, quit	      

;;; This is the guy that really does the trick.
(defun xmail:hidey-hole-trick ()
       (or minibufferp
	 (go-to-or-create-buffer '|_<XMAIL_HIDEY_HOLE>_|))
       (do ((buffers known-buflist (cdr buffers)))
	 ((null buffers))
	 (or (and (eq current-buffer (car buffers))
		(not minibufferp))
	     (buffer-kill (car buffers)))))

;;; And this little piggy went to market.
(defun xmail:go-to-market ()
       (sstatus interrupt 16. 'xmail:quit-handler)
       (cond ((status feature debug)
	    (xmail:setup)
	    (minibuffer-print "Debug: (xmail:start) to start."))
	   (t (xmail:start))))

(defun xmail:yesp (prompt)
       (prog (response ret-value)
	   (minibuffer-clear-all)
	   (xmail:set-minibuffer-line 1)
;;;          (setq rdis-mbuf-transient-linex (+ 1 (car minibufwindow)))
	   (do-forever
	     (setq response (minibuf-response (catenate prompt "  ") NL))
	     (cond ((or (samepnamep response "yes")
		      (samepnamep response "y"))
		  (setq ret-value t)(stop-doing))
		 ((or (samepnamep response "no")
		      (samepnamep response "n"))
		  (setq ret-value nil)(stop-doing))
		 (t (force-minibuffer-print "Please answer ""yes"" or ""no"".")
		    (ring-tty-bell))))
	   (minibuffer-clear-all)
	   (return ret-value)))

;;; Debugging function.
(defcom xmail:debugger
        (minibuffer-print
	(eval
	  (read-from-string (minibuffer-response "XMAIL DBG> ")))))

;;; Patch decimal-rep, which is in e_macops_ (should be in e_basic_)
(defun decimal-rep (x)
       (let ((ibase 10.) (base 10.) (*nopoint t))
	  (maknam (exploden x))))

;;; Start the extension.
(xmail:go-to-market)
