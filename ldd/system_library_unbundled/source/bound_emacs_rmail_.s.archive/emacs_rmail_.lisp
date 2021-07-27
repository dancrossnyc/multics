;;; ******************************************************
;;; *                                                    *
;;; * Copyright, (C) Honeywell Bull Inc., 1988           *
;;; *                                                    *
;;; * Copyright (c) 1978 by Massachusetts Institute of   *
;;; * Technology and Honeywell Information Systems, Inc. *
;;; *                                                    *
;;; ******************************************************


;;; HISTORY COMMENTS:
;;;  1) change(88-03-22,Blair), approve(88-03-22,MCR7842),
;;;     audit(88-06-29,Lippard), install(88-07-26,MR12.2-1069):
;;;     Change the expand_pathname_ to a call to
;;;     e_mail_pl1_$emacs_expand_svbx_pathname on a copy request so that we
;;;     can search the mlsys searchlist to locate the savebox.
;;;  2) change(89-02-02,Flegel), approve(89-02-28,MCR8066),
;;;     audit(89-03-30,Lee), install(89-04-24,MR12.3-1035):
;;;     phx20937, phx21049 - Changed "create-file" calls to "open-file" calls.
;;;     phx17317 - rmail-reply to change the Ack minor mode to the appropriate
;;;     value when reusing a reply buffer.
;;;  3) change(89-02-13,Flegel), approve(89-03-16,MCR8076),
;;;     audit(89-03-30,Lee), install(89-04-24,MR12.3-1035):
;;;     phx18781 - addition of rmail-help bound to the "?" key in order to
;;;     provide internal help to rmail.
;;;     phx16782 - addition of rmail-forward bound to the <N>"L" key in order
;;;     to provide an interface for mail forwarding
;;;     phx19704 - addition of support for rmail-quit so that if a rmail
;;;     command has has been entered after new mail has arrived, then do not
;;;     query.
;;;     phx21262 - addition of rmail-toggle-seen bound to "X" key in order
;;;     to provide an interface into changing the "seen" switch.
;;;                                                      END HISTORY COMMENTS


;;;
;;;  Emacs nuntio, BSG 10/10/78, 12/15/78, 3/17/79
;;;  Major rewrite of whole emacs_rmail_ 10/19/80 BSG for real header parse
;;;  To CAH's mlsys, BSG 10/31/82, reformat headers BSG 11/6/82
;;;  For MR10.2 mlsys by Jon Rochlis, July 1983 (for psr)
;;;  To comment out register-option forms, and move them to e_option_defaults_, 1/19/84 Barmar.
;;;  To make send-mail-beargumented trim the buffer name, 1/19/84 Barmar.
;;;  To remove "rmail-check-if-in-rmail" from rmail-logger-*pend, as they
;;;  are callable from send-mail, too.  2/15/85 Barmar

;;;  Here begins Emacs send mail (SMAIL?)

(defprop rtrim e_lap_$rtrim expr)

(declare
  (*expr e$get_temporary_seg e$release_temporary_seg  uppercase
         e_lap_$make-dat-ol-black-magic-string open-file
         execute-command exists-file fill-mode indent-rigidly
         save-same-file untabify exists-buffer delete-white-sides
         get-key-binding get-key-name describe))

(declare (*lexpr send-mail-beargumented))

(declare (special rmail-original-buffer mail-header-buffer fill-column
	        mail-original-message-ptr mail-mark mail-request-ack
	        screenlinelen rmail-request-acknowledgement null-pointer
	        mail-address-ptr rmail-commands-since-new-mail))

(%include e-macros)
(%include defun)  ; for send-mail-beargumented

(defvar mail-mode-hook nil)
(defvar rmail-initialized nil)


;;; defpl1's for the send mail side 

(declare (defpl1 mail_system_$set_message_switch ""
	          (ptr)         (char (4) aligned)    (bit (1) aligned)
	       ;; message-ptr   switch-name ("SEEN")    switch-value
		(return fixed bin (35.))))
	       ;; code

(declare (defpl1 e_mail_pl1_$emacs_redistribute_message ""
	          (ptr)            (char (*))    (ptr)     (bit (1))
	       ;; message-pointer   comments   recipients     ack
		(return fixed bin (35.))))
	       ;;     code

(declare (defpl1 e_mail_pl1_$emacs_get_address_list ""
	           (char (*))      (return ptr)   (return ptr)
	       ;; address list   address-pointer    list of address errors
		 (return fixed bin (35.))))
	       ;;     code

(declare (defpl1 e_mail_pl1_$emacs_parse_message_text ""  
	          (char (*))    (return ptr)       (return ptr)
                 ;;  message     message-pointer     parse error pointer
		(return fixed bin(35.))))
     	       ;;     code

(declare (defpl1 e_mail_pl1_$display_parse_errors ""
	          (ptr)                   (char (*))))
                 ;; parse error list ptr    message text

(declare (defpl1 e_mail_pl1_$emacs_deliver_message ""
	          (ptr)        (bit (1))    (return fixed bin(35.))))
                 ;; message-ptr   request-ack         code

(declare (defpl1 e_mail_pl1_$emacs_create_message ""
	          (char (*))     (return ptr)     (return fixed bin(35.))))
                 ;;  subject        message-ptr        code

(declare (defpl1 e_mail_pl1_$emacs_expand_svbx_pathname ""
                    (char (*)) (return char (168.)) (return char (32.)) (return fixed bin (35.))))
                 ;; pathname    dirname           entryname         code

(declare (defpl1 e_mail_pl1_$emacs_format_header ""
	          (ptr)      (fixed bin)            (ptr)
	       ;; message ptr  line length   temp seg/bmstring
		(return fixed bin(35.)))))
                 ;; code

(declare (defpl1 e_mail_pl1_$free_parse_text_error_list ""
	          (ptr)))
	       ;; parse_error_list ptr

(declare (defpl1 mail_system_$free_message "" (ptr) (return fixed bin(35.))))

(declare (defpl1 mail_system_$free_address_list "" (ptr) (return fixed bin (35.))))

(declare (defpl1 mail_system_$add_reply_reference "" 
	          (update ptr)        (ptr)             (fixed bin)
                 ;; new-message-ptr  original-message-ptr  position
		(return fixed bin(35.))))
                 ;;       code
		      
					
(defcom send-mail
        (send-mail-beargumented (trim-minibuffer-response "Mail subject: ")))

;;; Here we make life easy for any applications which may wish to call us
;;; We can be called with a subject string, addresses string, and even some
;;; initial text (any or which may be omited and default to "").
;;; The application can ignore mail_system_ totally ... we do all of the work

(defun send-mail-beargumented (&optional (subject "")
				 (additional-to-recipients "")
				 (initial-text ""))
       (let ((result (e_mail_pl1_$emacs_create_message subject)))
	  (let ((msg (car result))
	        (code (cadr result)))
	       (if (not (= code 0))
		 (display-com-error code "While creating new message.")
		 else
		 (setq rmail-original-buffer nil)  ;go back to most recent buffer when done
		 (find-buffer-in-window
		   (make_atom (rtrim (catenate "Mail about " subject))))
		 (send-mail-beargumented-1 msg 
				       additional-to-recipients
				       initial-text)))))

(defun send-mail-beargumented-1 (message-ptr 
			    additional-to-recipients
			    initial-text)
       (if (not rmail-initialized)
	 (mail-init))
       (if (empty-buffer-p current-buffer) 
	 (let ((code (e_mail_pl1_$emacs_format_header
		     message-ptr screenlinelen mail-header-buffer))
	       (free-code (mail_system_$free_message message-ptr)))
	      (if (not (= code 0))
		(display-com-error code "While formating header.")
		else
		(if (not (= free-code 0))
		    (display-com-error-noabort "While freeing message."))
		(insert-string (e_lap_$make-dat-ol-black-magic-string 
			       mail-header-buffer))))
	 (mapc '(lambda (x)(set-key (car x)(cadr x)))
	       '((^X^S	send-the-mail)
	         (ESC-^B	backward-mail-field)
	         (ESC-^F	forward-mail-field)
	         (ESC-^D	delete-mail-field)
	         (ESC-CR	continue-mail-field)
	         (^XF	mail-from)
	         (^XT	mail-to)
	         (^XA	mail-append)
	         (^X^A      mail-toggle-ack)
	         (^ZB       mail-bcc)		; everything else is taken
	         (^XC	mail-cc)
	         (^XJ	mail-subject)
	         (^XL	rmail-logger-append)
	         (^XP	rmail-logger-prepend)
	         (^XY	mail-reply-to)))
	 (setq current-buffer-mode  'Mail)
	 (fill-mode)
	 (setq fill-column 72.)		;protocol
	 (register-local-var 'mail-mark)
	 (register-local-var 'mail-request-ack)
	 (setq mail-request-ack rmail-request-acknowledgement)
	 (if mail-request-ack (assert-minor-mode 'Ack)
	     else (negate-minor-mode 'Ack))     ;phx17317
	 (mail-clean-up-from)
	 (go-to-beginning-of-buffer)
	 (if (not (forward-search (catenate NL "To:")))
	     (go-to-end-of-buffer)
	     (mail-init-header-field "To:" additional-to-recipients)
	     (setq mail-mark (set-mark))
	     (mail-to)
	     else (go-to-end-of-buffer)
	     (setq mail-mark (set-mark)))
	 (if mail-mode-hook (funcall mail-mode-hook))
	 (insert-string initial-text)
	 else
	 (mail-append)))

(defun mail-clean-up-from ()  ; let's be pretty
       (save-excursion
         (go-to-beginning-of-buffer)
         (if (forward-search "From:")
	   (delete-white-sides)
	   (insert-string TAB))))

(defun mail-init ()				;Once per loading
       (setq mail-header-buffer (e$get_temporary_seg))
       (set-emacs-epilogue-handler '(rmail-cleanup) nil)
       (setq rmail-initialized t))

(defun mail-find-first-blank-line ()
       (go-to-beginning-of-buffer)
       (do-forever
         (if (or (lastlinep)(line-is-blank))(stop-doing))
         (next-line)))

(defcom continue-mail-field
        (go-to-end-of-line)
        (delete-white-sides)
        (if (not (back-at ","))
	  (insert-char ","))
        (insert-char NL)
        (insert-string "    ")
        (save-excursion
	(mail-find-header-end)
	(backward-char)
	(set-mark-here mail-mark)))
	        
(defcom mail-to
        (mail-header-field-finder "To:"))

(defcom mail-reply-to
        (mail-header-field-finder "Reply-To:"))

(defcom mail-from
        (mail-header-field-finder "From:"))

(defcom mail-cc
        (mail-header-field-finder "cc:"))

(defcom mail-bcc
        (mail-header-field-finder "bcc:"))

(defcom mail-subject
        (mail-header-field-finder "Subject:"))

(defun mail-header-field-finder (arg &aux uc-arg arg-len)
       (setq uc-arg (uppercase arg)
	   arg-len (stringlength arg))
       (go-to-beginning-of-buffer)
       (do-forever
         (cond ((or (looking-at arg)
		(samepnamep uc-arg		;case insensitive header match
			  (uppercase
			    (substr (curline-as-string) 1 arg-len))))
	      (go-to-end-of-line)
	      (stop-doing))
	     ((or (lastlinep) (point>markp mail-mark))
	      (mail-header-add-new-line arg)
	      (stop-doing))
	     (t (next-line)))))

(defun look-for-mail-header-field (arg)
       (go-to-beginning-of-buffer)
       (do-forever
         (if (looking-at arg)
	   (if (point>markp mail-mark)(return nil)
	       else (forward-search arg)(return t)))
         (if (lastlinep)(return nil))
         (next-line)))

(defun mail-header-add-new-line (arg)
       (mail-find-header-end)
       (insert-string arg)
       (insert-string TAB)
       (insert-string NL)
       (set-mark-here mail-mark)
       (backward-char))

(defun mail-init-header-field (key value)
       (insert-string key)
       (insert-string TAB)
       (insert-string value)
       (new-line))

(defcom mail-append
        (mail-find-header-end)
        (if (lastlinep) (new-line)
	  else (go-to-end-of-buffer)))

(defun mail-find-header-end ()
       (go-to-beginning-of-buffer)
       (if (lastlinep) nil			;close enuf
	 else
	 (do-forever
	   (if (lastlinep) (return nil))	;global punt
	   (if (line-is-blank)(next-line)
	       else (stop-doing)))
	 (do-forever			;At header start?
	   (if (forward-search-in-line ":")
	       (do-forever
	         (if (lastlinep)		;only headers?
		   (stop-doing)
		   else
		   (next-line)
		   (if (or (not (at-white-char))   ;Another header field
			 (line-is-blank))	;end of header
		       (stop-doing))))
	       (if (or (lastlinep) (line-is-blank))
		 (stop-doing))
	       else
	       (stop-doing)))		;not a header field, maybe generate error in the future?
	 (if (not (line-is-blank))
	     (go-to-end-of-line)
	     (insert-char NL))))

(defcom delete-mail-field
        (do-forever
	(if (or (back-at ":") (back-at ",") (bolp))
	    (stop-doing))
	(backward-char))
        (with-mark bof
	         (delete-white-sides)
	         (if (forward-search-in-line ",")
		   (rubout-char)
		   else
		   (go-to-end-of-line))
	         (delete-white-sides)
	         (wipe-point-mark bof)
	         (if (eolp)(delete-white-sides)
		   (if (back-at ",")
		       (rubout-char)
		       (delete-white-sides))
		   else
		   (insert-string " ")))
        (if (< (cur-hpos) 10.)
	  (delete-white-sides)
	  (whitespace-to-hpos 10.)))

(defcom forward-mail-field
        (if (eolp) (go-to-beginning-of-line))
        (if (bolp) (forward-search-in-line ":")
	  else (if (forward-search-in-line ",")
		 else 
		 (go-to-beginning-of-line)
		 (forward-mail-field)))
        (skip-over-whitespace-in-line))

(defcom backward-mail-field
        (if (back-at ",") (backward-char))
        (if (reverse-search-in-line ",")
	  (forward-char)
	  else
	  (if (back-at ":")
	      (go-to-end-of-line)
	      (if (save-excursion (reverse-search-in-line ","))
		(backward-mail-field)
		else
		(reverse-search-in-line ":")
		(forward-char))
	      else
	      (reverse-search-in-line ":")
	      (forward-char)))
        (skip-over-whitespace-in-line))

(defcom mail-toggle-ack
        (setq mail-request-ack (not mail-request-ack))
        (minibuffer-print (catenate
		        "An acknowledgement will"
		        (if mail-request-ack "" else " not")
		        " be requested for this message."))
        (if mail-request-ack
	  (assert-minor-mode 'Ack)
	  else 
	  (negate-minor-mode 'Ack)))
 

;;; Now its time to send the message

(defcom send-the-mail
        (let ((message-text (rmail-buffer-to-string)))
	   (let ((msg-ptr nil))
	        (protect
		(setq msg-ptr (rmail-parse-message-text message-text))
		(if msg-ptr		;nil if there were parse errors
		    (rmail-deliver-message msg-ptr))
		&always
		(if msg-ptr (mail_system_$free_message msg-ptr))))))

(defun rmail-buffer-to-string ()
       (save-excursion
         (go-to-beginning-of-buffer)
         (with-mark start-of-buffer
		(go-to-end-of-buffer)
		(point-mark-to-string
		  start-of-buffer))))

;;; Turn the current buffer into a message (puf, like magic)

;;; Note: we hack adding the reply reference here if we are a reply message,
;;; because mail_system_ can't parse In-Reply-To: fields. (grumble, grumble)

(defun rmail-parse-message-text (message-text
			    &aux error-ptr message-ptr code)	;initialized to nil
       (protect
         (let ((result (e_mail_pl1_$emacs_parse_message_text message-text)))
	    (setq message-ptr (first result)
		error-ptr (second result)
		code (third result))
	    (if (not (= code 0))
	        (if (not (= code (error-table 'mlsys_et_ 'text_parse_failed)))
		  (display-com-error code "While parsing message.")
		  else
		  (display-as-printout 
		    (rmail-display-parse-errors
		      error-ptr message-text)))
	        (setq message-ptr nil)	;it will be returned as the function value
	        else
	        (if mail-original-message-ptr
		  (let ((result (mail_system_$add_reply_reference
			        message-ptr mail-original-message-ptr -1)))
		       (let ((new-message-ptr (car result))
			   (code (cadr result)))
			  (if (= code 0)
			      (setq message-ptr new-message-ptr)))))))
         &always
         (and error-ptr
	    (not (= error-ptr null-pointer))
	    (e_mail_pl1_$free_parse_text_error_list error-ptr)))
       message-ptr)

(defun rmail-display-parse-errors (error-ptr message-text)
       (protect 
         (rmail-set-up-file-output)
         (e_mail_pl1_$display_parse_errors error-ptr message-text)
         &always (rmail-clean-up-file-output))
       (insert-string  "Message will not be delivered.")	; this will appear at the top of the buffer
       (new-line)
       (new-line))

;;; And now we deliver the message ...

(defun rmail-deliver-message (message-ptr)
       (let ((request-ack-flag mail-request-ack)	; make sure we are in the right buffer
	   (code 0))
	  (display-as-printout (setq code 
			         (rmail-display-delivery-results
				 message-ptr 
				 request-ack-flag)))
	  (if (= code 0)			;happy?
	      (rmail-restore-original-buffer))))

(defun rmail-display-delivery-results (message-ptr request-ack-flag &aux code)
       (protect 
         (rmail-set-up-file-output)
         (setq code (e_mail_pl1_$emacs_deliver_message 
		  message-ptr (if request-ack-flag -1 else 0)))
         &always (rmail-clean-up-file-output))
       code)

(defun rmail-restore-original-buffer ()
       (setq buffer-modified-flag nil)	;unmodify the buffer
       (cond ((null rmail-original-buffer)
	    (find-buffer-in-window previous-buffer))
	   ((exists-buffer rmail-original-buffer)
	    (find-buffer-in-window rmail-original-buffer))))

(defcom forward-the-mail
        (let ((message-ptr mail-original-message-ptr)
	    (comment-text (rmail-buffer-to-string))
	    (address-ptr mail-address-ptr)
	    (request-ack-flag mail-request-ack)
	    (code 0))
	   (protect
	     (display-as-printout
	       (setq code (rmail-display-redistrib-results
			message-ptr comment-text
			address-ptr request-ack-flag)))
	     (if (= code 0) (rmail-restore-original-buffer))
	     &always (and address-ptr
		        (mail_system_$free_address_list address-ptr)))))

(defun rmail-display-redistrib-results (message-ptr comments-text address-ptr request-ack-flag &aux code)
       (protect
         (rmail-set-up-file-output)
         (setq code (e_mail_pl1_$emacs_redistribute_message 
		  message-ptr (if comments-text comments-text else "")
		  address-ptr (if request-ack-flag -1 else 0 )))
         (if (not (= code 0))
	   (display-com-error code "Forwarding mail."))
         &always (rmail-clean-up-file-output))
       code)


;;;	Emacs merry mailman
;;;	BSG 12/16-17/78, 3/17-18/79
;;;	Dedicated to Ray Heatherton of old.

;;; Here starts Emacs read mail (RMAIL)

(declare (special error-table fpathname rmail-message-buffer FF
	        rmail-msgx rmail-mailbox-path read-only-flag rmail-seen
	        rmail-deleteq rmail-msgcount last-input-char
	        rmail-original-yank-indent mail-original-message
	        rmail-open-state rmail-buffer-to-go-back-to
	        rmail-mailbox-ptr rmail-send-acknowledgement 
	        rmail-reply-include-authors rmail-reply-include-recipients
	        rmail-reply-include-self rmail-header-format start-up/.rmail))

(defvar rmail-mode-hook nil)

;;; (register-option 'rmail-original-yank-indent 4) ;moved to e_option_defaults_

;;; (register-option 'rmail-send-acknowledgement t) ;moved to e_option_defaults_
;;; (register-option 'rmail-request-acknowledgement nil) ;moved to e_option_defaults_

;;; (register-option 'rmail-reply-include-authors t) ;moved to e_option_defaults_
;;; (register-option 'rmail-reply-include-recipients nil) ;moved to e_option_defaults_
;;; (register-option 'rmail-reply-include-self nil) ;moved to e_option_defaults_

;;; (register-option 'rmail-header-format 'default-formatting-mode) ;moved to e_option_defaults_

;;; the MCR boards felt this was too complicated, so we won't make them
;;; offical options --- JR 09/02/83

(defvar envelope-format-var nil)
(defvar header-format-var nil)
(defvar redistributions-list-format-var nil)

;;; defpl1's for the read mail side 

(declare (defpl1 e_mail_pl1_$emacs_mailbox_open ""
	          (char (*)) (char (*)) (return ptr) (return bit (1)) (return fixed bin)
	       ;; directory  entry       mailbox ptr   salvaged       count
		(return fixed bin (35.))))
                 ;;        code

(declare (defpl1 e_mail_pl1_$emacs_read_message ""
	          (ptr)         (ptr)
	       ;; message ptr   temp seg ptr
	          (fixed bin) (bit (1))     
	       ;; line length  acknowledge   
		(char (*))        (char (*))        (char (*))
	       ;;  envelope-format   header-format   redistributions-format
		(return fixed bin (21.))  (return bit (1))
                 ;;   number of lines in body    seen switch
		(return fixed bin (35.))))
                 ;;        code

(declare (defpl1 e_mail_pl1_$emacs_get_message_ptr "" 
                    (ptr)        (fixed bin)       (return ptr)
                 ;; mailbox-ptr   message-number     message-ptr
		(return fixed bin(35.)))))
                 ;;          code 

(declare (defpl1 e_mail_pl1_$get_user_default_mbx_address ""
	       (return ptr)))

(declare (defpl1 e_mail_pl1_$emacs_mailbox_close "" (ptr) (return fixed bin)))

(declare (defpl1 e_mail_pl1_$emacs_create_reply_message ""
	          (ptr)       (bit (1))       (bit (1))        (bit (1))
	       ;; orig msg  include-authors  include-recip   include self
                    (return ptr)        (return fixed bin(35.))))
                 ;;   new msg               code

(declare (defpl1 mail_system_$read_new_messages ""
                    (update ptr)  (return fixed bin)   (return fixed bin)
                 ;;  mailbox-ptr    new-messages        new-regular messages
                    (return fixed bin)        (return fixed bin(35.))))
                 ;;  new-interactive-messages      code

(declare (defpl1 mail_system_$get_address_pathname "" 
	          (ptr) (return char (168.)) (return char (32.)) 
                 ;; address    directory	entryname
		(return char (32.)) (return fixed bin (35.))))
	       ;; componentname	code

(declare (defpl1 mail_system_$mark_message_for_deletion ""
	          (ptr)   (return fixed bin(35.))))
                 ;; message ptr     code

(declare (defpl1 mail_system_$unmark_message_for_deletion ""
	          (ptr)   (return fixed bin(35.))))
                 ;; message ptr     code

(declare (defpl1 mail_system_$save_message ""
	          (ptr)       (char (*))   (char (*))    (bit (1)) 
	       ;; message ptr  dir name    entry name   create flag
		(return fixed bin(35.))))
                 ;;  code (surprise, surprise)

(declare (defpl1 mlsys_utils_$parse_address_text ""
	          (char (*))   (return ptr)  (return fixed bin (35.))))
                 ;; text-address   mlsys-address     code

(declare (defpl1 mlsys_utils_$parse_mailbox_text ""
	          (char (*))   (return char (168)) (return char (32))
                 ;; mbx-name	   mbx-path-dir	   mbx-path-entry
		(return fixed bin (35.))))
	       ;; code

(declare (defpl1 mlsys_utils_$print_message_summary_header ""
	          (fixed bin)  (ptr)        (return fixed bin (35.))))
                 ;; line length  output iocb    code

(declare (defpl1 mlsys_utils_$print_message_summary ""
	          (ptr)         (fixed bin)      (bit (1) aligned)
	       ;; message ptr  message number   current message flag
		(fixed bin)   (ptr)      (return fixed bin (35.))))
                 ;; line length  output iocb     code  

(declare (defpl1 mail_system_$free_address ""
		(ptr)	(return fixed bin (35.))))
	       ;; address   code

(declare (defpl1 expand_pathname_ "" (char (*)) (return char(168.)) (return char(32.)) (return fixed bin(35.))))

(declare (defpl1 pathname_ "" (char (*)) (char (*)) (return char (168))))


(defun rmail-init ()
       (setq rmail-open-state nil)		;Signifies initted
       (if (not rmail-initialized)
	 (mail-init))
       (setq rmail-message-buffer nil))

(defun rmail-cleanup ()			;In case he leaves without closing
       (if rmail-open-state
	 (e_mail_pl1_$emacs_mailbox_close rmail-mailbox-ptr)))


(defcom rmail &numarg &pass
       (if (not (boundp 'rmail-open-state))(rmail-init))
       (if rmail-open-state (go-to-buffer 'Incoming/ mail)
	 else
	 (rmail-open-mbx
	   (if numarg
	       (rmail-get-address-pathname-from-user)
	       else
	       (rmail-get-address-pathname (rmail-get-default-address))))
	 (setq rmail-message-buffer (e$get_temporary_seg))
	 (setq rmail-buffer-to-go-back-to current-buffer)
	 (go-to-or-create-buffer 'Incoming/ mail)
	 (setq current-buffer-mode 'RMAIL)
	 (setq fpathname rmail-mailbox-path)
	 (setq buffer-modified-flag t rmail-open-state t)
	 (minibuffer-clear)
	 (mapc '(lambda (x)(set-key (car x)(cadr x)))
	       '((/0	rmail-argument)
	         (/1	rmail-argument)
	         (/2	rmail-argument)
	         (/3	rmail-argument)
	         (/4	rmail-argument)
	         (/5	rmail-argument)
	         (/6	rmail-argument)
	         (/7	rmail-argument)
	         (/8	rmail-argument)
	         (/9	rmail-argument)
	         (/-	rmail-minus)
	         (?	rmail-help)
	         (m	send-mail-from-rmail)
	         (c	rmail-copy)
	         (D	rmail-queue-delete-backward)
	         (d	rmail-queue-delete-forward)
	         (j	rmail-go-command)
	         (g	rmail-go-command)
	         (l	rmail-go-last-msg)
	         (n	rmail-go-forward)
	         (p	rmail-go-backward)
	         (x	rmail-toggle-seen)
	         (q	rmail-quit)
	         (^X^Q	rmail-quit)
	         (r	rmail-reply)
	         (f	rmail-forward)
	         (s	rmail-summarize)
	         (u	rmail-undelete)
	         (^XL	rmail-logger-append)
	         (^XP	rmail-logger-prepend)))
	 (rmail-unbind-word-commands-kludge)
	 (without-modifying (destroy-buffer-contents))
	 (setq read-only-flag t buffer-modified-flag nil)
	 (if (= rmail-msgcount 0)
	     (rmail-quit)
	     (display-error "No messages in " rmail-mailbox-path))
	 (setq rmail-msgx 1 rmail-deleteq nil)
	 (if (boundp 'start-up/.rmail)
	     (mapc 'eval start-up/.rmail))
	 (if (and (boundp 'rmail-mode-hook)
		rmail-mode-hook)
	     (funcall rmail-mode-hook))
	 (rmail-display-current-msg)
	 (select-buffer-window current-buffer 'cursize))
       (minibuffer-print "Type ""?"" for a list of rmail commands.")
       &doc "$$$ is an emacs interface into the mail system.  By default,
mail is read from your personal default mailbox.  With a positive numeric
argument (e.g. ^U), $$$ prompts for the ""mailbox name"".  Commands available
to $$$ are:

 $$rmail-help$	Displays this information.
 $$rmail-go-forward$	Moves on to the next message.
 $$rmail-go-backward$	Moves back to the previous message.
 $$rmail-go-last-msg$	Moves to the last message in your mailbox.
 $$rmail-go-command$	Moves to the message number specified by the numeric argument.
 $$rmail-queue-delete-forward$	Queue the current message for deletion - move to next message.
 $$rmail-queue-delete-backward$	Same as d, but moves backward.
 $$rmail-undelete$	Brings back the last (stacked) deleted message.
 $$rmail-copy$	Copies the message to some other mailbox.
 $$rmail-quit$	Quits out of rmail returning to the original buffer.
 $$rmail-summarize$	Summarizes (in a local display) all undeleted messages.
 $$rmail-toggle-seen$	Toggles the ""Seen"" switch of the current message.
 $$rmail-logger-append$	Log the message to an ASCII file, placing it at the end of the file.
 $$rmail-logger-prepend$	Same as ^XL, but ""prepends"" to the front.
 $$rmail-forward$	Forward the current message to a list of addresses.
 $$rmail-reply$	Formats a MAIL mode buffer to reply to the current message.
 $$send-mail-from-rmail$	Sends mail that is not necessarily a reply (see $$rmail-reply$).
")

(defcom rmail-help
        &prologue rmail-check-if-in-rmail
        &na (&reject)
        (if (samepnamep current-buffer-mode 'RMAIL)
	  (minibuffer-remark "Please wait...")
	  (describe 'rmail)
	  (minibuffer-clear)
	  else
	  (display-error "Not in RMAIL mode")))

(defun rmail-unbind-word-commands-kludge ()	;they forced me
       (rmail-unbind-if-bound '(1  125 nil) 'upper-case-word) ;esc-U
       (rmail-unbind-if-bound '(1  114 nil) 'lower-case-word) ;esc-L
       (rmail-unbind-if-bound '(1  103 nil) 'capitalize-initial-word))     ;esc-C

(defun rmail-unbind-if-bound (key-list function)	;unbind key-list if bound to function
       (if (eq (get-key-binding key-list) function)
	 (set-key (get-key-name key-list) 'undefined-command)))


(defun rmail-open-mbx (mbxpath)		;mbxpath is (dir . entry)
        (let ((result (e_mail_pl1_$emacs_mailbox_open (car mbxpath) (cdr mbxpath))))
	  (let ((mailbox-pointer (car result))
	        (salvaged (cadr result))
	        (msgcount (caddr result))
	        (code (cadddr result)))
	       (if (not (= 0 code))
		 (display-com-error code (rtrim (pathname_ (car mbxpath) (cdr mbxpath)))))
	       (if (not (= 0 salvaged))
		 (minibuffer-print "Mailbox has been salvaged."))
	       (setq rmail-mailbox-path (rtrim (pathname_ (car mbxpath) (cdr mbxpath))))
	       (setq rmail-msgcount msgcount)
	       (setq rmail-mailbox-ptr mailbox-pointer)
	       1)))


;;; This gets a string from the user turns it into a pathname
;;; using the mail system address to parse it if needed

(defun rmail-get-address-pathname-from-user ()
       (let ((answer (trim-minibuf-response "Mailbox: " NL)))
	  (cond ((nullstringp answer) (display-error "No mailbox name given."))
	        ((and (= 1 (index answer "{"))	; {thing}
		    (samepnamep (substr answer (stringlength answer))
			      "}"))
	         (let ((result (mlsys_utils_$parse_address_text answer))
		     (address nil)
		     (code 0)
		     (path ""))
		    (protect
		      (setq address (car result)
			  code (cadr result))
		      (if (not (= code 0))
			(display-com-error
			  code "Getting mail address for " answer)
			else
			(setq path (rmail-get-address-pathname address))
			(let ((entry (rtrim (cdr path))))
			     (if (not (samepnamep
				      ".mbx"   ;catch .forum/.mls
				      (substr entry
					    (- (stringlength entry)
					       3))))
			         (display-error "The specified address does not identify a mailbox. " answer))))
		      &always (and address
			         (mail_system_$free_address address)))
		    path))
	        (t (let ((result (mlsys_utils_$parse_mailbox_text answer)))
		      (let ((code (third result)))
			 (if (not (zerop code))
			     (display-com-error code answer))
			 (rplacd result (second result))))))))	;return (dir . entry)

(defun rmail-get-default-address ()
       (e_mail_pl1_$get_user_default_mbx_address))

(defun rmail-get-address-pathname (address)
       (let ((result (mail_system_$get_address_pathname address)))
	  (let ((dir (first result))
	        (entry (second result))
	        (code (fourth result)))
	       (cond ((not (= code 0))
		    (display-com-error
		      code "Getting pathname of mailbox address"))
		   (t (cons dir entry))))))

;;; Get an address list from the user, parse and verify the addressees into
;;; an address-list structure to be used by the mail_system.  Note that 
;;; addressees must be separated by ","s as that is what it appears the
;;; mail_system wants.

(defun rmail-get-address-list-from-user (&aux error-ptr address-ptr code)
       (let ((answer (trim-minibuf-response "Forward to: ")))
	  (cond ((nullstringp answer)
	         (display-error "No addresses given.")
	         nil)
	        (t
		(protect
		  (let ((result (e_mail_pl1_$emacs_get_address_list answer)))
		       (setq address-ptr (first  result)
			   error-ptr   (second result)
			   code	     (third  result))
		       (if (not (= code 0))
			 (if (not (= code (error-table 'mlsys_et_ 'text_parse_failed)))
			     (display-com-error code "While parsing address list.")
			     else
			     (display-as-printout
			       (rmail-display-parse-errors
			         error-ptr answer)))
			 (setq address-ptr nil)))
		  &always (and error-ptr
			     (not (= error-ptr null-pointer))
			     (e_mail_pl1_$free_parse_text_error_list error-ptr))
		  &failure (and address-ptr
			      (mail_system_$free_address_list address-ptr))))))
       address-ptr)

;;; Display the current (i.e. rmail-msgx) message.  

(defun rmail-display-current-msg ()
       (if (not (eq current-buffer 'Incoming/ mail))
	 (display-error "Error: not in RMAIL"))
       (if (> rmail-msgx rmail-msgcount)
	 (display-error-noabort "No next msg."))
       (setq fpathname rmail-mailbox-path)
       (without-modifying
         (destroy-buffer-contents)
         (let ((msg (rmail-read-message rmail-msgx)))

;;; Set the seen switch as we are now looking at it, so it really is seen

	    (rmail-set-seen t)

;;; Display the message size and the message

	    (insert-string (catenate
			 "(" (decimal-rep rmail-msgx)
			 ") -- " (decimal-rep (car msg))
			 " line" (if (= (car msg) 1) "" else "s")))
	    (new-line)
	    (new-line)
	    (insert-string (cdr msg))
	    (go-to-beginning-of-buffer))))

;;; This calls the mail system to actually format the message and 
;;; returns (number of lines in text; a black magic string containing the
;;; entire message)

(defun rmail-read-message (message-number)
       (let ((message-ptr (rmail-get-message-ptr
		        rmail-mailbox-ptr message-number)))
	  (let ((result (e_mail_pl1_$emacs_read_message 
		        message-ptr rmail-message-buffer screenlinelen
		        (if rmail-send-acknowledgement -1 else 0)

;;; if the user has given values to various formatting option variable,
;;; then we use them, otherwise we use the documented rmail-header-format 
;;; option

		        (or envelope-format-var
			  (cond ((eq rmail-header-format 'brief-formatting-mode) 'none-formatting-mode)
			        (t rmail-header-format)))
		        (or header-format-var
			  rmail-header-format)
		        (or redistributions-list-format-var
			  rmail-header-format))))
	       (let ((lines-in-body (first result))      ;MF
		   (code	        (third result)))     ;MF
		  (setq rmail-seen (if (= (second result) 0) nil else t))
		  (cond ((= code -1)	; e_mail_pl1_ special case
		         (display-com-error 0 "envelope-format (rmail-header-format) is invalid."))
		        ((= code -2)
		         (display-com-error 0 "rmail-header-format is invalid."))
		        ((= code -3)
		         (display-com-error 0 "redistributions-list-format (rmail-header-format) is invalid."))
		        ((not (= code 0))
		         (display-com-error-noabort code "Reading message " 
					      (decimal-rep message-number)))
		        (t 
			(cons lines-in-body 
			      (e_lap_$make-dat-ol-black-magic-string
			        rmail-message-buffer))))))))


;;; Given a mailbox and a message number get a pointer to a mail system 
;;; message structure.  e_mail_pl1_ will read the message if it hasn't 
;;; already done so.

(defun rmail-get-message-ptr (mailbox-ptr message-number)
       (let ((result (e_mail_pl1_$emacs_get_message_ptr
		   mailbox-ptr message-number)))
	  (let ((message-ptr (car result))
	        (code (cadr result)))
	       (if (not (= code 0))
		 (display-com-error code "While getting message "
				(decimal-rep (message-number)) "."))
	       message-ptr)))

(defcom rmail-quit
        &prologue rmail-check-if-in-rmail-buffer
        (let ((message-to-goto (1+ rmail-msgcount)))
	   (if (and (rmail-check-for-new-messages)       ;new messages and
		  (= rmail-commands-since-new-mail 0)) ;no new commands
	       (if (yesp "Do you still want to quit?")
		 (rmail-quit-1)
		 else
		 (rmail-go message-to-goto 'forward))
	       else
	       (rmail-quit-1))))

(defun rmail-quit-1 ()
       (if rmail-message-buffer (e$release_temporary_seg rmail-message-buffer))
       (let ((code (e_mail_pl1_$emacs_mailbox_close rmail-mailbox-ptr)))
	  (if (not (= code 0))
	      (display-com-error code "closing" rmail-mailbox-path)))
       (set-buffer-self-destruct current-buffer)
       (rmail-echo "")
       (setq rmail-open-state nil)
       (go-to-or-create-buffer rmail-buffer-to-go-back-to))

(defun rmail-check-if-in-rmail-buffer ()	    ;check rmail buffer
       (cond ((eq current-buffer-mode 'RMAIL)
	    t)
	   (t
	     (display-error "Error: not in RMAIL")
	     nil)))

(defun rmail-check-if-in-rmail ()
       (cond ((rmail-check-if-in-rmail-buffer)	    ;check rmail buffer
	    (setq rmail-commands-since-new-mail	    ;increment command count
		(+ rmail-commands-since-new-mail 1))
	    (rmail-check-for-new-messages))))	    ;check for new mail


(defun rmail-check-for-new-messages ()
       (let ((result (mail_system_$read_new_messages rmail-mailbox-ptr)))
	  (setq rmail-mailbox-ptr (car result))
	  (let ((number-of-new-messages (cadr result))
	        (code 0))
;	        (code (cadddr (cdr result))))  ; this won't run compiled
	       (setq code (cadddr (cdr result))) ; but this appears to work
	       (if (and (not (= code 0))
		      (not (= code 
			    (error-table 'mlsys_et_ 'no_more_messages))))
		 (display-com-error-noabort 
		   code "Trying to determine if new messages had arrived.")
		 nil
		 else
		 (if (= number-of-new-messages 0)
		     nil
		     else
		     (setq rmail-msgcount
			 (+ rmail-msgcount number-of-new-messages))
		     (setq rmail-commands-since-new-mail 0) ;reset count
		     (if (= 1 number-of-new-messages)
		         (minibuffer-print "One new message has arrived.")
		         else
		         (minibuffer-print (decimal-rep 
				         number-of-new-messages)
				       " new messages have arrived."))
		     t)))))


(defcom rmail-argument
        &prologue rmail-check-if-in-rmail       
        &numeric-argument (&pass)
        (rmail-echo last-input-char)
        (let ((digit (- (CtoI last-input-char) (CtoI "0"))))
	   (if (null numarg)
	       (setq numarg digit)
	       else
	       (setq numarg (if (< numarg 0) (- (* numarg 10.) digit)
			    else (+ (* numarg 10.) digit)))))
        (process-char (get-char)))

(defcom rmail-minus
        &prologue rmail-check-if-in-rmail       
        (rmail-echo last-input-char)
        (let ((c (get-char)))
	   (if (or (< c (CtoI "0")) (> c (CtoI "9")))
	       (setq numarg -1)
	       (process-char c)
	       else
	       (setq numarg (- (CtoI "0") c))
	       (rmail-echo (ItoC c))
	       (process-char (get-char)))))


(defcom rmail-go-forward ()
        &prologue rmail-check-if-in-rmail
        &numeric-argument (&pass)
        &negative-function rmail-go-backward
        (rmail-echo last-input-char)
        (rmail-go (+ rmail-msgx (or numarg 1)) 'forward))

(defcom rmail-go-backward
        &prologue rmail-check-if-in-rmail
        &numeric-argument (&pass)
        &negative-function rmail-go-forward
        (rmail-echo last-input-char)
        (rmail-go (- rmail-msgx (or numarg 1)) 'backward))

(defcom rmail-go-last-msg 
        &prologue rmail-check-if-in-rmail
        (rmail-check-for-new-messages)
        (rmail-echo 'l)
        (rmail-go rmail-msgcount 'backward))

(defun rmail-not-deleted-message (message-number)
       (not (memq message-number rmail-deleteq)))

(defun rmail-queue-deletion (message-index)
        (if (rmail-not-deleted-message message-index)
	  (let ((message-ptr (rmail-get-message-ptr
			   rmail-mailbox-ptr message-index)))
	       (let ((code (mail_system_$mark_message_for_deletion
			 message-ptr)))
		  (if (not (= 0 code))
		      (display-com-error code "deleting message " 
				     (decimal-rep message-index))
		      else
		      (setq rmail-deleteq
			  (cons message-index rmail-deleteq)))))
	  else
	  (display-error "Message " (decimal-rep message-index)
		       "has already been deleted.")))

(defcom rmail-queue-delete-forward
        &prologue rmail-check-if-in-rmail       
        (rmail-echo 'd)
        (rmail-queue-deletion rmail-msgx)
        (cond ((let ((mno (rmail-find-message-forward (1+ rmail-msgx))))
		(if mno (rmail-go mno 'forward) t else nil)))
	    ((let ((mno (rmail-find-message-backward (1- rmail-msgx))))
		(if mno (rmail-go mno 'backward) t else nil)))
	    (t (rmail-delete-desperator))))

(defcom rmail-queue-delete-backward
       &prologue rmail-check-if-in-rmail
       (rmail-echo 'D)
       (rmail-queue-deletion rmail-msgx)
       (cond ((let ((mno (rmail-find-message-backward (1- rmail-msgx))))
	         (if mno (rmail-go mno 'backward) t else nil)))
	   ((let ((mno (rmail-find-message-forward (1+ rmail-msgx))))
	         (if mno (rmail-go mno 'forward) t else nil)))
	   (t (rmail-delete-desperator))))

(defun rmail-delete-desperator ()
       (cond ((yesp "All messages deleted. Quit rmail?")
	    (rmail-quit))
	   (t (minibuffer-print "Undeleting this message.")
	      (rmail-undelete))))

(defcom rmail-undelete
       &prologue rmail-check-if-in-rmail
       (rmail-echo 'u)
       (if rmail-deleteq
	 (let ((last-deleted-message (car rmail-deleteq)))
	      (let ((message-ptr (rmail-get-message-ptr
			       rmail-mailbox-ptr last-deleted-message)))
		 (let ((code (mail_system_$unmark_message_for_deletion
			     message-ptr)))
		      (if (not (= code 0))
			(display-com-error code "Un-deleting message " 
				         (decimal-rep last-deleted-message))
			else
			(setq rmail-deleteq (cdr rmail-deleteq))
			(rmail-go last-deleted-message 'forward)))))
	 else
	 (display-error-noabort "No pending deletions.")
	 (ring-tty-bell)))

;;; Toggle the seen switch on the current message, update the minor mode to
;;; reflect the new status.

(defcom rmail-toggle-seen ()
        &prologue rmail-check-if-in-rmail
        (rmail-echo 'x)
        (rmail-set-seen (not rmail-seen)))

(defun rmail-set-seen (mode)
       (let ((code (mail_system_$set_message_switch
		 (rmail-get-message-ptr rmail-mailbox-ptr rmail-msgx)
		 "SEEN" (if mode -1 else 0))))
	  (cond ((= code 0)
	         (setq rmail-seen mode)
	         (if rmail-seen (assert-minor-mode 'Seen)
		   else (negate-minor-mode 'Seen)))
	        (t
		(display-com-error code "While setting seen switch.")))))

(declare (special tty-no-upmotionp))

(defun rmail-echo (x)
       (if (not tty-no-upmotionp)
	 (minibuffer-print-noclear x)))

(defun rmail-go (msgno direction)
       (if (> msgno rmail-msgcount)(rmail-check-for-new-messages))
       (cond ((and (> msgno rmail-msgcount)(eq direction 'forward))
	    (display-error "No more messages forward."))
	   ((or (< msgno 1)(> msgno rmail-msgcount))
	    (display-error "Invalid message number: " (decimal-rep msgno)))
	   ((rmail-not-deleted-message msgno)
	    (setq rmail-msgx msgno)
	    (rmail-display-current-msg))
	   ((eq direction 'forward)
	    (let ((fno (rmail-find-message-forward msgno)))
	         (if fno (rmail-go fno 'forward)
		   else (display-error "No more messages forward."))))
	   (t (let ((fno (rmail-find-message-backward msgno)))
		 (if fno (rmail-go fno 'backward)
		   else (display-error "No more messages backward."))))))

(defun rmail-find-message-forward (msgno)
       (do ((tentative msgno (1+ tentative))
	  (checkedflag))
	 ((> tentative rmail-msgcount) nil)
	 (if (rmail-not-deleted-message tentative)
	     (return tentative))
	 (if (and (= tentative rmail-msgcount)(not checkedflag))
	     (setq checkedflag t)
	     (rmail-check-for-new-messages))))

(defun rmail-find-message-backward (msgno)
       (do ((tentative msgno (1- tentative)))
	 ((< tentative 1) nil)
	 (if (rmail-not-deleted-message tentative)
	     (return tentative))))

(defcom rmail-go-command
        &prologue rmail-check-if-in-rmail
        &numarg &pass
        (let ((here rmail-msgx))
	   (if numarg (rmail-go numarg 'forward))
	   (if (not (= here rmail-msgx))
	       (rmail-echo (catenate "(" (decimal-rep rmail-msgx) ")")))))

(defcom rmail-summarize
        &prologue rmail-check-if-in-rmail
        (rmail-check-for-new-messages)
        (display-as-printout (rmail-create-summary-buffer)))

(defun rmail-create-summary-buffer ()
       (protect 
         (rmail-set-up-file-output)
         (rmail-get-summary-header)
         (do message-number 1 (1+ message-number)
	   (> message-number rmail-msgcount)
	   (if (rmail-not-deleted-message message-number)
	       (rmail-summarize-one message-number)))
         &always (rmail-clean-up-file-output)))

(defun rmail-get-summary-header ()
       (let ((code (mlsys_utils_$print_message_summary_header
		 screenlinelen null-pointer)))
	  (if (not (= code 0))
	      (go-to-buffer 'Incoming/ mail)
	      (rmail-clean-up-file-output)
	      (display-com-error code
			     "While getting message summary header."))))
						  

(defun rmail-summarize-one (message-number)
       (let ((message-ptr (rmail-get-message-ptr
		        rmail-mailbox-ptr message-number))
	   (current-msg-flag 0)) ;"0"b
	  (if (= rmail-msgx message-number) (setq current-msg-flag -1));"1"b
	  (let ((code (mlsys_utils_$print_message_summary
		      message-ptr message-number current-msg-flag
		      screenlinelen null-pointer)))
	       (if (not (= code 0))
		 (rmail-clean-up-file-output)
		 (go-to-buffer 'Incoming/ mail)
		 (display-com-error code
				"While getting summary for message"
				(decimal-rep message-number))))))

; Maybe there is too much internal knowledge here, and this should
; be done some other way

; delcare's for file-output hacking

(declare (special pdir-temp-ename pdir-temp-pathname))
(declare (*lexpr ncline e_pl1_$get_iocb hcs_$truncate_file hcs_$set_bc))
(declare
  (defpl1 iox_$control "" (ptr) (char (*)) (ptr) (return fixed bin (35.))))
       
(defun rmail-set-up-file-output ()
       (hcs_$set_bc process-dir pdir-temp-ename 0)
       (ncline 'file_output pdir-temp-pathname '-ssw 'user_output
	     '-ssw 'error_output))

(defun rmail-clean-up-file-output ()
       (e_cline_ "revert_output -ssw user_output -ssw error_output")
       (let ((fpathname nil))
	  (read-in-file pdir-temp-pathname))
        (hcs_$truncate_file process-dir pdir-temp-ename 0)
       (iox_$control (e_pl1_$get_iocb) "reset_more" null-pointer))


;;; Reply

(defcom rmail-reply
        &prologue rmail-check-if-in-rmail
        &numarg &pass
        (rmail-echo 'r)
        (setq rmail-original-buffer current-buffer)
        (let ((original-message-text (rmail-collect-orig))
	    (include-authors (if (or 
			       rmail-reply-include-authors
			       numarg)
			     -1 else 0))
	    (include-recipients (if (or 
				rmail-reply-include-recipients
				numarg)
			        -1 else 0))
	    (include-self (if rmail-reply-include-self -1 else 0))
	    (original-message-ptr (rmail-get-message-ptr
			        rmail-mailbox-ptr rmail-msgx)))
	   (let ((result (e_mail_pl1_$emacs_create_reply_message
		         original-message-ptr include-authors
		         include-recipients include-self)))
	        (let ((new-message-ptr (car result))
		    (code (cadr result)))
		   (if (not (= code 0))
		       (display-com-error code "While creating reply message."))
		   (find-buffer-in-window
		     (make_atom (catenate "Reply to message "
				      (decimal-rep rmail-msgx))));;; ugh, ugh, ugh
		   (without-saving (destroy-buffer-contents))
		   (send-mail-beargumented-1 new-message-ptr "" "")
		   (mail-append)
		   (setq current-buffer-mode 'RMAIL/ reply)
		   (rmail-prepare-response-environment
		     original-message-text original-message-ptr)))))

(defun rmail-collect-orig ()
       (save-excursion
         (go-to-beginning-of-buffer)
         (next-line)
         (with-mark start-of-buffer
		(go-to-end-of-buffer)
		(point-mark-to-string
		  start-of-buffer))))

;;; 10/15/80

(defcom send-mail-from-rmail
        &prologue rmail-check-if-in-rmail        
        (let ((msg (rmail-collect-orig)))
	   (execute-command 'send-mail nil nil)
	   (rmail-prepare-response-environment msg nil)))

;;; We must keep track of the original message and add the reply reference 
;;; ourselves because mail_system_ can't hack parsing In-Reply-To: fields
;;; (grumble, grumble)

(defun rmail-prepare-response-environment (original-msg-text original-msg-ptr)
       (register-local-var 'mail-original-message)
       (register-local-var 'rmail-original-buffer)  ; buffer to return to if successful
       (setq mail-original-message original-msg-text)
       (setq rmail-original-buffer 'Incoming/ mail)
       (if original-msg-ptr ; non-nil implies we are a reply message
	 (register-local-var 'mail-original-message-ptr)
	 (setq mail-original-message-ptr original-msg-ptr))
       (set-key 'ESC-^Y 'rmail-yank-mail)
       (set-key '^X^Q 'return-to-rmail))

(defcom return-to-rmail
        (minibuffer-clear)
        (find-buffer-in-window 'Incoming/ mail))

(defcom rmail-yank-mail
        &numeric-argument (&reject)
        (set-the-mark)
        (insert-string mail-original-message)
        (let ((numarg rmail-original-yank-indent))
	   (indent-rigidly))
        (if (not (back-at NL))(new-line)))

;;; 02/07/89
;;; Forward the current piece of mail to a list of addresses.  A numeric
;;; argument allows the user to add redistribution comments to the mail
;;; by creating a comments buffer.

(defcom rmail-forward
        &prologue rmail-check-if-in-rmail
        &numarg &pass
        (rmail-echo 'f)
        (setq rmail-original-buffer current-buffer)
        (let ((original-message-ptr
	      (rmail-get-message-ptr rmail-mailbox-ptr rmail-msgx))
	    (address-ptr nil))
	   (protect
	     (cond ((setq address-ptr (rmail-get-address-list-from-user))
		  (if numarg
		      (find-buffer-in-window
		        (make_atom
			(catenate "Forwarding comments for message "
				(decimal-rep rmail-msgx))))
		      (rmail-prepare-comment-environment
		        original-message-ptr address-ptr)
		      else
		      (display-as-printout
		        (rmail-display-redistrib-results
			original-message-ptr nil address-ptr
			mail-request-ack))
		      (and address-ptr
			 (mail_system_$free_address_list address-ptr)))))
	     &failure (and address-ptr
		       (mail_system_$free_address_list address-ptr)))))

;;; Prepare the current buffer as a comment buffer for sending redistribution
;;; comments

(defun rmail-prepare-comment-environment (original-msg-ptr address-ptr)

;;; Local variables used

       (register-local-var 'rmail-original-buffer)    ; return buffer
       (register-local-var 'mail-original-message-ptr); mail to forward
       (register-local-var 'mail-address-ptr)	    ; destination addresses
       (register-local-var 'mail-request-ack)	    ; if an ack is required
       (setq rmail-original-buffer 'Incoming/ mail)
       (setq mail-original-message-ptr original-msg-ptr)
       (setq mail-address-ptr address-ptr)
       (setq mail-request-ack rmail-request-acknowledgement)

;;; Set up the buffer display

       (without-saving (destroy-buffer-contents))
       (setq current-buffer-mode 'RMAIL/ comments)
       (if mail-request-ack (assert-minor-mode 'Ack)
	 else (negate-minor-mode 'Ack))

;;; Set up buffer characteristics

       (fill-mode)
       (setq fill-column 61.)			    ;72.-11. for indentation
       (set-key '^X^A 'mail-toggle-ack)
       (set-key '^X^Q 'return-to-rmail)
       (set-key '^X^S 'forward-the-mail))

;;;
;;; RMAIL correspondence recorder
;;;

(declare (special known-buflist))
(defcom rmail-logger-append
        &prologue rmail-check-if-in-rmail
        &numarg (&pass)
        (rmail-guts-of-logger 'append))

(defcom rmail-logger-prepend ()
        &prologue rmail-check-if-in-rmail
        &numarg (&pass)
       (rmail-guts-of-logger 'prepend))

(defun rmail-guts-of-logger (whichway)
       (if (not (and (memq 'Mail/ log  known-buflist) (null numarg)))
	 (let ((new-pathname (trim-minibuf-response "RMAIL Log file: " NL)))
	      (if (nullstringp new-pathname)
		(display-error "You must supply a pathname."))
	      (open-file new-pathname 'write)	;;; phx20937/21049
	      (save-excursion-buffer
	        (go-to-or-create-buffer 'Mail/ log)
	        (read-in-file new-pathname))))
       (save-excursion
         (save-excursion-buffer
	 (let ((z (curbuf-as-string)))
	      (go-to-buffer 'Mail/ log)
	      (if (eq whichway 'append)
		(go-to-end-of-buffer)
		else
		(go-to-beginning-of-buffer))
	      (insert-char FF)
	      (insert-string z))
	 (save-same-file))))


;;; Rmail copy request --- this is the same as read_mail's save request

(defcom rmail-copy
        &prologue rmail-check-if-in-rmail
        (let ((result (e_mail_pl1_$emacs_expand_svbx_pathname 
		    (minibuf-response "Save Mailbox: " NL))))
	   (let ((rmail-sv-dname (car result))
	         (rmail-sv-ename (cadr result))
	         (code (caddr result)))
	        (if (not (= code 0))
		  (display-com-error
		    code "Expanding save mailbox pathname.")
		  else
		  (rmail-copy-1 rmail-sv-dname rmail-sv-ename 0)))))

(defun rmail-copy-1 (rmail-sv-dname rmail-sv-ename create-flag)
       (let ((code (mail_system_$save_message 
		 (rmail-get-message-ptr rmail-mailbox-ptr rmail-msgx)
		 rmail-sv-dname rmail-sv-ename create-flag)))
	  (if (and (= code (error-table 'mlsys_et_ 'no_savebox))
		 (yesp "Save mailbox doesn't exist.  Create it?"))
	      (rmail-copy-1 rmail-sv-dname rmail-sv-ename -1)
	      else
	      (if (and (not (= code 0))
		     (not (= code
			   (error-table 'mlsys_et_ 'savebox_created))))
		(display-com-error code "Trying to save message.")))))
       
