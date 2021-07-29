;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1983 *
;;; *                                                         *
;;; ***********************************************************

;;; HISTORY COMMENTS:
;;;  1) change(85-01-21,Davids), approve(86-02-04,MCR7350),
;;;     audit(86-04-24,Gilcrease), install(86-04-24,MR12.0-1048):
;;;     Modified so that the table of commands uses "CTL" for the
;;;     control key instead of "C" and "ESC" for the escape key instead
;;;     of "E".  Changed the way the subject is determined for reply
;;;     mode.  It now looks for the character <ESC> which now separates
;;;     the subject from the comment.  It then deletes the <ESC>
;;;     character and takes that location as the end of the subject.
;;;     It does all this instead of just going to the end of the line
;;;     because the subject might be several lines long (i.e.  have
;;;     embedded <NL>s).
     
;;;     85-01-22 Davids: Modified xforum:finish so that it puts an ESC
;;;     character at the end of subject text.
     
;;;     85-03-04 Davids: Added the reply_by_subject mode.  This acts like
;;;     reply mode except when things are being set up in xforum:reply.
;;;     If its by_subject a string representing the transaction number is
;;;     obtained and a search is made to position the buffer to the
;;;     transaction.
;;;  2) change(85-06-25,Davids), approve(86-02-04,MCR7350),
;;;     audit(86-04-24,Gilcrease), install(86-04-24,MR12.0-1048):
;;;     Removed all references to xforum_help_line_$pop.  Replaced all
;;;     references of xforum_help_line_$push_general_message_only with
;;;     calls to xforum_help_line_$change_general_message_only.  This was
;;;     easier than trying to figure out all the places that pops would be
;;;     needed.  Without the pops you would get help_line stack overflows
;;;     after entering about 7 comments in a row.
;;;  3) change(86-02-06,LJAdams), approve(86-02-18,MCR7350),
;;;     audit(86-04-24,Gilcrease), install(86-04-24,MR12.0-1048):
;;;     Added command-quit to quit handler for reconnect.
;;;                                                      END HISTORY COMMENTS


;;; Emacs extension to implement Executive Forum functions.
;;; Deryk Barker December 1983
;;; A certain amount of "borrowing" from xmail stuff
;;; 84-04-03 Senft: changed size of mini-buffer from 5 to 3
;;; 84-06-12 Schimke: Modified for compatibility with xmail extension:
;;;          commented out many of the extended features which require the site
;;;          to have full emacs (these may be added back compatibly with xmail 
;;;          at a future date), improved quit handler to warn before destroying
;;;          unsaved text, installed improved yesp function.
;;;
;;; 84-08-01 Davids: Modified xforum:instructions for reply mode to make the
;;; instructions correspond to the xforum MTB. Also modified xforum:help so
;;; that a better prompt is issued to the user and so that the ? response
;;; gives the appropriate list of commands. This was the extended features that
;;; were commented out by Dave.
;;;
;;; 84-08-02 Davids:modified xforum:insert-file and xforum:modify-subject
;;; to call xforum_status_$push_help_line, pop_help_line, and update_help_line.
;;; This improves help message handling. Also replaced references to
;;; transaction with references to comment in the help text.
;;;
;;; 84-08-06 Davids: Modified go-to-beginning-of-buffer to check and see if
;;; the user is currently in the minibuffer and if so to just use the standard
;;; go-to-beginning-of-buffer
;;;
;;; 84-08-10 Davids: Added missing commands to the help table and created a 
;;; separate table for reply mode. Added the display-buffer-as-pintout function
;;; and deleted the loadlib of e_macops_. Loading e_macops_ was very expensive
;;; to get just one same function. Add documentation to some of the commands
;;; which were still missing it.
;;;
;;; 84-10-01 Davids: Changed calls to xforum_status_$push_help_line,
;;; pop_help_line and update_help_line to
;;; xforum_help_line$push_general_message_only, pop, and
;;; push_general_message_only.
;;;
;;; 84-10-16 Davids: Changed the help line for modifing the subject so that
;;; it has the same format as the help line when entering a new subject. That
;;; help line is displayed by xforum_attend_mtg_options.
;;;
;;; 84-11-19 Davids: Changed xforum_help_line calls to xforum_help_line_
;;; calls. 
;;;
;;; 84-11-26 Davids: Added a test to the xforum:refill function so that it
;;; calls xforum:beginning-of-paragraph only if it is not already at the
;;; the beginning of the paragraph. The beginning of paragraph function will
;;; move you to the beginning of the next paragraph if you are at the beginning
;;; of the current paragraph. 
;;;

;;; Load in the necessary include files.
(%include e-macros)  
(%include backquote)

;;; To use in debug mode, remove the semicolon at the beginning of this line:
;(sstatus feature debug)

(declare
  ;; These are functions defined elsewhere in Emacs.
  (*lexpr minibuffer-response xforum:minibuffer-response report-error-noabort
	xforum:quit-handler)
  (*expr e_lap_$rtrim emacs$get_info_ptr error-table eval-lisp-line
         file-insert fill-current-line fill-mode go-to-end-of-line
         describe-key kill-to-beginning-of-line kill-lines lowercase-ttp
         loadlib redisplay-current-window-relative
         mark-at-current-point-p minibuffer-clear-all signalquit buffer-kill
         prev-line-command quit-force redisplay-command save-same-file
         search-not-charset-forward set-minibuffer-size user_info_$homedir
         eval:internal exists-file set-permanent-key e_lap_$return-string
         set-the-mark beginning-of-paragraph runoff-fill-region get_pdir_
         end-of-paragraph expand-window-to-whole-screen rdis-choose-echo-linex
         create-new-window-and-go-there find-file-subr at-beginning-of-paragraph
         emacs$set_emacs_return_code twiddle-chars twiddle-words e_cline_
         key-prompt get-key-binding get-key-name describe-internal forward-search)
  ;; Global variables.
  (special xforum:subject-text 
	 xforum:transaction-mark xforum:header-info xforum:mode xforum:sleep
	 xforum:silent-instructions known-buflist paragraph-definition-type
	 whitespace-charactertbl minibufferp mode-line-hook fill-prefix
	 suppress-minibuffer no-minibuffer-<> suppress-remarks
	 quit-on-break buffer-creation-hook default-fill-column
	 rdis-splln-mark selected-window transaction trans_string 
	 good-word-charactertbl quit-handler-invoked
           rdis-mbuf-transient-linex minibufwindow)
;;; Test for reconnect.
  (defpl1 xforum$reconnect_test "" (return bit (1) aligned)))

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
		      '(eq xforum:mode ',(cadr form))))
	 ;; Define the known modes.
	 (defmode talk)
	 (defmode reply)
           (defmode reply_by_subject)
	 )

(defun xforum:setup ()
       (expand-window-to-whole-screen)

       ;;; Set the internal options of Emacs.
       (or (status feature debug)
	 (setq mode-line-hook 'xforum:mode-line	;Empty mode line.
	       paragraph-definition-type 2	;The "right" one.
	       suppress-minibuffer t		;No minibuffer output.
	       no-minibuffer-<> t		;No "<>" after mbuf input.
	       suppress-remarks t		;No Reading/Writing transactions.
	       default-fill-column 72.	;Fill column for all buffers.
	       fill-column 72.		;Fill column for this buffer.
	       quit-on-break t		;Quit Emacs on BREAK.
	       quit-handler-invoked nil         ;Not yet
	       buffer-creation-hook 'xforum:turn-on-fill  ;Turn on fill mode.
	       ))

       ;;; Set internal variables to xforum.
       (setq xforum:subject-text nil
	   xforum:silent-instructions t
	   xforum:sleep 3)

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
	   (""	rubout-char)
	   (""  	rubout-char)
	   ("#"		rubout-char)
	   ("^D"		delete-char)
	   ("@"		kill-to-beginning-of-line)
	   ("ESC-#"	rubout-word)
	   ("ESC-\177"	rubout-word)
	   ("ESC-"	rubout-word)
	   ("ESC-D"	delete-word)
	   ("\"		escape-char)
	   ("ESC-R"	xforum:redisplay-command)
	   ("ESC-Q"	xforum:finished)
	   ("^A"		go-to-beginning-of-line)
	   ("^E"		go-to-end-of-line)
	   ("ESC-<"	xforum:go-to-beginning-of-buffer)
	   ("ESC->"	go-to-end-of-buffer)
             ("ESC-?"         xforum:help)
	   ))

       ;; Load in help package 
       ;; This isn't necessary when e_self_documentor_ gets bound with emacs_ 
       (loadlib 'e_self_documentor_)

;;; If we are debugging, set ESC-ESC.

       (and (status feature debug)
	  (set-permanent-key "ESC-ESC" 'xforum:debugger)))

;;; Function to create empty mode line.
(defun xforum:mode-line () (list "" ""))

;;; ^L: redisplay full screen and print instructions.
(defcom xforum:redisplay-command
        &doc "Clears and then redisplays the text being worked on. The text
will be redisplayed so that the current line is centered in the window."
        (redisplay-command)
        (xforum:instructions))

;;; Turn on fill mode.
(defun xforum:turn-on-fill (n) n (fill-mode))

;;; Main function.  Do housekeeping, call correct mode function.
(defun xforum:start ()
       (xforum:setup)
       (fill-mode)
       (set-minibuffer-size 3)
       (setq xforum:mode
	   (make_atom
	     (e_lap_$rtrim
	       (e_lap_$return-string (emacs$get_info_ptr) 0 32.))))
       (or (reply-mode) (reply_by_subject-mode)
	 (destroy-buffer-contents))
       (go-to-beginning-of-buffer)
       (or (reply-mode)(talk-mode)(reply_by_subject-mode) (redisplay))
       (go-to-end-of-buffer)
       (setq xforum:silent-instructions t)
       (cond ((talk-mode) (xforum:talk))
	   ((reply-mode) (xforum:reply))
             ((reply_by_subject-mode) (xforum:reply)))
       (setq xforum:silent-instructions nil)
       (setq xforum:transaction-mark (set-mark))
       (set-minibuffer-size 2)
       (cond ((or (reply-mode) (reply_by_subject-mode))
	    (set-key "ESC-H"  'xforum:page-other-window)
	    (set-key "ESC-L"  'xforum:unpage-other-window))
	   ((talk-mode) 
	    (set-key "ESC-U"  'xforum:modify-subject)))
       
       (set-key "ESC-G"	'xforum:insert-file)
       (set-key "ESC-M"	'xforum:refill)
       (set-key "^B"	'xforum:backward-char)
       (set-key "^P"	'xforum:prev-line-command)
       (set-key "ESC-B"	'xforum:backward-word)
       (set-key "\177"	'xforum:rubout-char)
       (set-key ""        'xforum:rubout-char)
       (set-key "#"		'xforum:rubout-char)
       (set-key "ESC-#"	'xforum:rubout-word)
       (set-key "ESC-\177"	'xforum:rubout-word)
       (set-key "ESC-"	'xforum:rubout-word)
       (set-key "ESC-P"	'xforum:prev-screen)
       (set-key "ESC-<"	'xforum:go-to-beginning-of-buffer)
       (xforum:instructions))

;;; Main function for TALK mode.
(defun xforum:talk ()
       (prog nil
         (xforum:redisplay-command)
         (setq xforum:subject-text
	     (minibuffer-response "Enter Subject: "))
         again
         (cond ((samepnamep xforum:subject-text (substr " " 1 (stringlength xforum:subject-text)))
	      (ring-tty-bell)
	      (setq xforum:subject-text
		  (minibuffer-response "You must enter a Subject: "))
	      (go again)))
         (insert-string "Subject: ")
         (insert-string xforum:subject-text)
         (new-line)))

(defcom xforum:modify-subject &numeric-argument (&reject)
        &doc "Displays the current subject in the minibuffer and allows you
to edit it using normal control keys and escape sequences. When you have
finished modifying the subject , carriage return will change it in the
comment you are entering."
        (e_cline_ "xforum_help_line_$change_general_message_only ""Press  RETURN:enter new subject""")
        (let ((suppress-minibuffer nil)) suppress-minibuffer
	   (setq xforum:subject-text
	         (minibuffer-response "Enter new subject: " NL xforum:subject-text))
	   (prog nil
	         again
	         (cond ((samepnamep xforum:subject-text (substr " " 1 (stringlength xforum:subject-text)))
		      (ring-tty-bell)
		      (setq xforum:subject-text
			  (minibuffer-response "You must enter a Subject: "))
		      (go again))))
	   (save-excursion
		 (go-to-beginning-of-buffer)
		 (skip-to-whitespace)		;past Subject
		 (skip-over-whitespace)		;to subject-text
		 (without-saving
		   (kill-lines))
		 (insert-string xforum:subject-text)
		 (next-line)))
        (xforum:instructions))

;;; Main function for REPLY mode.
(defun xforum:reply ()
       (let ((this-buffer current-buffer))
	  (create-new-window-and-go-there)
	  (go-to-or-create-buffer this-buffer))
       (select-other-window)
       (find-file-subr
         (catenate (e_lap_$rtrim (get_pdir_)) ">xforum_view_seg"))
       (go-to-beginning-of-buffer)
       (new-line)				;looks nicer
       (next-line)				;past header blurb
       (skip-to-whitespace)			;past Subject:
       (skip-over-whitespace)			;to subject itself
       (if (looking-at "Re: ")
	 (skip-to-whitespace)
	 (skip-over-whitespace))		;past Re:
       (with-mark m
	        (forward-search "")
	        (rubout-char)
	        (setq xforum:subject-text (catenate "Re: "
					    (point-mark-to-string m)
					    )))
       (select-other-window)
       (go-to-beginning-of-buffer)
       (insert-string "Subject: ")
       (insert-string xforum:subject-text)
       (new-line)
       (redisplay)
       (cond ((reply_by_subject-mode)
            (select-other-window)
	  (go-to-beginning-of-buffer)
            (setq trans_string (e_lap_$rtrim (e_lap_$return-string (emacs$get_info_ptr) 32. 8.)))
            (forward-search trans_string)
            (redisplay-current-window-relative 1)
            (select-other-window))))


;;; These functions replace the normal Emacs functions to make sure
;;; that the user cannot touch the transaction subject.

;;; Replaces ^B command.
(defcom xforum:backward-char &numeric-argument (&repeat)
        &doc "Moves backwards one character in the buffer. Tabs and newline 
characters at the end of lines count as single characters.
$$$ will not allow you to stray backwards into the
comment subject"
        (or (mark-at-current-point-p xforum:transaction-mark)
	  (let ((numarg nil)) (backward-char))))

;;; Replaces ^P command.
(defcom xforum:prev-line-command &numeric-argument (&repeat)
        &doc "Move to previous line of buffer. $$$
will attempt to stay in the same horizontal position. It will not allow
you to stray back into the comment subject."
        (or (mark-on-current-line-p xforum:transaction-mark)
	  (let ((numarg nil)) (prev-line-command))))

;;; Replaces ESC-B command.
(defcom xforum:backward-word &numeric-argument (&repeat)
        &doc "Moves backwards one word in the buffer.
$$$ will not allow you to stray backwards into the
comment subject."
        (let ((numarg nil)) (backward-word))
        (or (point>markp xforum:transaction-mark)
	  (go-to-mark xforum:transaction-mark)))

;;; Replaces ESC-< command
(defcom xforum:go-to-beginning-of-buffer &numeric-argument (&reject)
        &doc "Moves back to the beginning of the comment - i.e.
to the beginning of the first line after the Subject: header.
If you are answering a prompt, the cursor will move to the first character of
your answer."
        (cond (minibufferp (go-to-beginning-of-buffer))
              (t (go-to-mark xforum:transaction-mark))))

;;; Replaces ^T command
(defcom xforum:twiddle-chars &numeric-argument (&reject)
        &doc "Twiddles (transposes, interchanges) the two characters 
immediately to the left of the cursor, unless this would involve disturbing 
the comment subject line."
       (with-mark m
	        (backward-char)
	        (cond ((point>markp xforum:transaction-mark)
		     (go-to-mark m)
		     (twiddle-chars))
		    (t
		      (go-to-mark m)))))

;;; Replaces ESC-T command
(defcom xforum:twiddle-words &numeric-argument (&reject)
        &doc "Twiddles (transposes, interchanges) the two words to the left of
the cursor. The cursor will first go to the end of a word if you are in the 
middle of it. If either word is a part of the Subject: line 
$$$ has no effect."
       (with-mark m
	        (and (charset-member (curchar) good-word-charactertbl)
		   (forward-word))
	        (do-times 2 (backward-word))
	        (forward-char)
	        (cond ((point>markp xforum:transaction-mark)
		     (go-to-mark m)
		     (twiddle-words))
		    (t
		      (go-to-mark m)))))

;;; Replaces ESC-V command.
(defcom xforum:prev-screen &numeric-argument (&repeat)
        &doc "Displays the previous screen (one back) of
this buffer, and leaves the cursor sitting either at the top of the screen
or immediately after the Subject: line, whichever is appropriate. A numeric 
argument (e.g. ESC 5 $$$) will move back that many screens."
        (cond ((mark-on-current-line-p xforum:transaction-mark))
	    ('else (prev-screen)
		 (or (point>markp xforum:transaction-mark)
		     (go-to-mark xforum:transaction-mark)))))

;;; ESC-^V
(defcom xforum:page-other-window &numeric-argument (&pass)
        &doc "Displays the next screen (one forward) of the comment
to which you are replying. A numeric  argument
(e.g. ESC 5 $$$) will move forward that many screens."
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
(defcom xforum:unpage-other-window &numeric-argument (&pass)
        &doc "Displays the previous screen (one backward) of the comment
to which you are replying. A numeric  argument
(e.g. ESC 5 $$$) will move backward that many
screens."
        (let ((numarg (- (or numarg 1))))
	   (xforum:page-other-window)))

;;; Replaces \177, # command.
(defcom xforum:rubout-char &numeric-argument (&repeat)
        &doc "Deletes the previous character - i.e. the one to the
left of the cursor. $$$ will not let you delete any of the 
Subject: line."
        (or (mark-at-current-point-p xforum:transaction-mark)
	  (let ((numarg nil))  (rubout-char))))

;;; Replaces ESC-\177, ESC-# command.
(defcom xforum:rubout-word &numeric-argument (&repeat)
        &doc "Deletes the word to the left of the cursor. More specifically
deletes characters backwards until the beginning of the word. 
$$$ will not allow you to delete back into the 
Subject: line. Successive $$$s are merged and may be
 retrieved with a single ^Y."
        (with-mark here
	         (let ((numarg nil)) (backward-word))
	         (cond ((point>markp xforum:transaction-mark)
		      (wipe-point-mark here))
		     (t
		       (go-to-mark here)
		       (wipe-point-mark xforum:transaction-mark)))))

;;; For re-filling a region.
(defcom xforum:refill &numeric-argument (&reject)
        &doc "Fills (reformats) the current paragraph, lining up left margin."
	   (save-excursion
	     (cond ((not (at-beginning-of-paragraph)) (xforum:beginning-of-paragraph)))
	     (set-the-mark)
	     (end-of-paragraph)
	     (without-saving (runoff-fill-region))))

;;; Go to beginning, but NOT INTO SUBJECT LINE!
(defcom xforum:beginning-of-paragraph &numeric-argument (&repeat)
        &doc "Moves the cursor to the beginning of the current paragraph,
or to the beginning of the previous paragraph if already at the beginning of the
current. The beginning of the first line of a paragraph is the beginning of 
the paragraph. A paragraph is deemed to be started by an indented line. 
$$$ will not move the cursor into the Subject:
line. A numeric argument (e.g. ESC 5 $$$) will
go back that many paragraphs."
       (beginning-of-paragraph)
       (or (point>markp xforum:transaction-mark)
	 (go-to-mark xforum:transaction-mark)))

;;; For inserting files into the buffer.
(defcom xforum:insert-file &numeric-argument (&reject)
        &doc "Will prompt you for the pathname of
a file which will be inserted into the current buffer at the point where the 
cursor is. The cursor will be returned to its current point after insertion."
        (save-excursion
          (minibuffer-clear-all)
	(e_cline_ "xforum_help_line_$change_general_message_only ""Enter file name followed by RETURN, just press RETURN to abort"""))
	(file-insert (xforum:get-good-file "Get comment from file: "))
	(xforum:instructions)))

;;; For giving the user help via describe-key
(defcom xforum:help &numeric-argument (&reject)
        &doc "Will prompt you for the command you want help with. Entering
a ""?"" will produce a table of all the valid commands with a very short
description."
        (cond (minibufferp (ring-tty-bell))
	    (t (let ((suppress-minibuffer nil)) suppress-minibuffer
		  (let ((key (key-prompt "Enter Command Key Sequence or ? for general help: ")))
		       (cond
		         ((not (and (= (car key) 0) (= (car (cdr key)) 77)))
			(let ((symbol (get-key-binding key))
			      (description (get-key-name key)))
			     (describe-internal description symbol "Press CTLg (control-g) when ready to continue")))
		         (t
			 (init-local-displays)
			 (local-display-generator-nnl "Press CTLg (control-g) when ready to continue")
			 (local-display-generator-nnl " ")
			 (local-display-generator-nnl "(ESCx = press escape key then press x;   BS = BACKSPACE = Ch;")
			 (local-display-generator-nnl " CTLx = hold CONTROL key down while pressing x)")
			 (cond
			   ((talk-mode)
			    (local-display-generator-nnl "CTLf: Forward Char    ESCn: Next Screen             @: Erase to Start of Line")
			    (local-display-generator-nnl "CTLb: Backward Char   ESCp: Previous Screen         CTLk: Erase to End of Line")
			    (local-display-generator-nnl "ESCf: Forward Word    ESC<: Start of Comment        CTLy: Retrieve Erased Text")
			    (local-display-generator-nnl "ESCb: Backward Word   ESC>: End of Comment          ESCg: Get File")
			    (local-display-generator-nnl "CTLa: Start of Line   CTLd: Erase Char              ESCm: Adjust Paragraph")
			    (local-display-generator-nnl "CTLe: End of Line     BS: Backward Erase Char       ESCr: Redisplay Screen")
			    (local-display-generator-nnl "CTLn: Next Line       ESCd: Erase Word              ESCq: Enter Comment")
			    (local-display-generator-nnl "CTLp: Previous Line   ESCBS: Backward Erase Word    ESCu: Change Subject")
			    (local-display-generator-nnl "                                                    ESC?: Editor Help"))
			   ((or (reply-mode) (reply_by_subject-mode))
			    (local-display-generator-nnl "CTLf: Forward Char   ESCp: Previous Screen       CTLy: Retrieve Erased Text")
			    (local-display-generator-nnl "CTLb: Backward Char  ESC<: Start of Reply        ESCg: Get File")
			    (local-display-generator-nnl "ESCf: Forward Word   ESC>: End of Reply          ESCm: Adjust Paragraph")
			    (local-display-generator-nnl "ESCb: Backward Word  CTLd: Erase Char            ESCr: Redisplay Screen")
			    (local-display-generator-nnl "CTLa: Start of Line  BS: Backward Erase Char     ESCl: Previous Page of Comment")
			    (local-display-generator-nnl "CTLe: End of Line    ESCd: Erase Word            ESCh: Next Page of Comment")
			    (local-display-generator-nnl "CTLn: Next Line      ESCBS: Backward Erase Word  ESCq: Enter Reply")
			    (local-display-generator-nnl "CTLp: Previous Line  @: Erase to Start of Line   ESC?: Editor Help")
			    (local-display-generator-nnl "ESCn: Next Screen    CTLk: Erase to End of Line")))
			 (end-local-displays))))
		  (xforum:instructions)))))

(defun display-buffer-as-printout ()
       (save-excursion
         (init-local-displays)
         (go-to-beginning-of-buffer)
         (do-forever
	 (local-display-generator (curline-as-string))
	 (if (lastlinep) (stop-doing))
	 (next-line))))

;;; Display set of instructions in the minibuffer.
(defun xforum:instructions ()
       (let ((suppress-minibuffer nil)) suppress-minibuffer
	  (cond ((or minibufferp xforum:silent-instructions))
;	        ((eq current-buffer 'change-header)
;	         (minibuffer-clear-all)
;	         (minibuffer-print
;		 "Entering or modifying the ""Subject"" text.")
;	         (minibuffer-print "Enter ESC q when done."))
	        ((talk-mode)
	         (minibuffer-clear-all)
                   (e_cline_ "xforum_help_line_$change_general_message_only ""Press  ESC?:help  BREAK:abort entry  ESCq:enter comment  ESCu:modify_subject"""))
	        ((or (reply-mode) (reply_by_subject-mode))
	         (minibuffer-clear-all)
                   (minibuffer-print " ")
	         (minibuffer-print "Press  ESC?:help  BREAK:abort entry  ESCq:enter reply in meeting"))
	        (t
		(minibuffer-clear-all)
		(minibuffer-print "enter ESC q when done."
;*ESC-G*			        "     Enter ESC g to insert a file."
			        ))
	        )))

(defun xforum:set-minibuffer-line (lineno)
       (setq rdis-mbuf-transient-linex (+ lineno (car minibufwindow))))       

;;; Finished entire transaction; write out and punt if all is OK.
(defcom xforum:finished
        &doc "Enters your comment into the meeting and returns you to the
menu."
        (emacs$set_emacs_return_code 0)
        (and minibufferp (command-quit))
        (go-to-beginning-of-buffer)
        (forward-search xforum:subject-text)
        (insert-string "")
        (go-to-mark xforum:transaction-mark)
        (cond ((search-not-charset-forward whitespace-charactertbl)
	     (save-same-file)
	     (xforum:quit-force))
	    ((xforum:yesp "There is no comment.  Quit? ")
	     (xforum:quit-handler nil))
	    (t (xforum:instructions)
	       (command-quit))))

;;; Get a filename of a file that exists and can be read.
(defun xforum:get-good-file (prompt)
       (do ((name (xforum:minibuffer-response prompt)
	        (xforum:minibuffer-response prompt)))
	 ((xforum:good-file? name) name)))

;;; Check a file for validity: can we read it?
(defun xforum:good-file? (name)
       (let ((suppress-minibuffer nil))
	  suppress-minibuffer
	  (setq name (e_lap_$trim name))
	  (cond ((nullstringp name)
	         (minibuffer-print "Aborting file read.")
	         (sleep 1)
                   (xforum:instructions)
	         (command-quit)))
	  (let ((exists? (catch (exists-file name 4) pgazonga)))
	       (cond ((null exists?)
		    (minibuffer-print "File not found.")
		    (ring-tty-bell)
		    nil)
		   ((atom exists?) nil)
		   (t t)))))

(declare (special rdis-mbuf-transient-linex minibufwindow))
(defun xforum:minibuffer-response lexpr
       (prog1 (apply 'minibuffer-response (listify lexpr))
	    (setq rdis-mbuf-transient-linex (1+ (car minibufwindow)))))

;;; These three little horrible things make sure that all XFORUM buffers
;;; are flushed so that tasking Emacs will not encounter them again.

;;; This one handles plain old ESC Q.
(defun xforum:quit-force ()
       (xforum:hidey-hole-trick)
       (quit-force))

;;; This one handles the BREAK key.
(defun xforum:quit-handler arg arg
       (cond (quit-handler-invoked)		;are we recursing?
	   ((not (zerop (xforum$reconnect_test)))
	    (xforum:redisplay-command)	;reconnect
	    (command-quit))
	   (t (cond (buffer-modified-flag	;break key
		    (setq quit-handler-invoked t)  ;prevent recursion
		    (cond ((xforum:yesp "Any pending work will be lost. Do you really want to quit?  ")
			 (xforum:hidey-hole-trick)	;yes, quit
			 (signalquit))
			(t (xforum:instructions)  ;no, not quitting
			   (setq quit-handler-invoked nil)
			   (command-quit))))
		  (t (signalquit))))))	     ;buffer not changed, quit	      

;;; This is the guy that really does the trick.
(defun xforum:hidey-hole-trick ()
       (or minibufferp
	 (go-to-or-create-buffer '|_<XFORUM_HIDEY_HOLE>_|))
       (do ((buffers known-buflist (cdr buffers)))
	 ((null buffers))
	 (or (and (eq current-buffer (car buffers))
		(not minibufferp))
	     (buffer-kill (car buffers)))))

;;; And this little piggy went to market.
(defun xforum:go-to-market ()
       (sstatus interrupt 16. 'xforum:quit-handler)
       (cond ((status feature debug)
	    (xforum:setup)
	    (minibuffer-print "Debug: (xforum:start) to start."))
	   (t (xforum:start))))

(defun xforum:yesp (prompt)
       (prog (response ret-value)
	   (minibuffer-clear-all)
	   (xforum:set-minibuffer-line 1)
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
(defcom xforum:debugger
        (minibuffer-print
	(eval
	  (read-from-string (minibuffer-response "XFORUM DBG> ")))))

;;; Patch decimal-rep, which is in e_macops_ (should be in e_basic_)
(defun decimal-rep (x)
       (let ((ibase 10.) (base 10.) (*nopoint t))
	  (maknam (exploden x))))

;;; Start the extension.
(xforum:go-to-market)
