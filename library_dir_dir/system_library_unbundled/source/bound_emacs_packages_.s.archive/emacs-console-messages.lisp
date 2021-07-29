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
;;;	Interactive Message Handling for Multics EMACS
;;;
;;;	Coded in a peek of phit 1978.08.07-11
;;;			    by Richard S. Lamson
;;;	Patterned after similar code by bsg.
;;;	Qsends by bsg, 4/15/79, to enable better communications
;;;	   with persons at MIT-AI.
;;;
;;;	Extensively re-written 10-23 January 1980 by Richard Mark Soley
;;;	to allow multiple mailboxes, automatic checking for qsend-ok-flag,
;;;	really use emacs interrupt system, and clean up this nasty code.
;;;
;;;	Modified 7 February 1980 by Richard Mark Soley to create
;;;	'fill-messages option; i.e., whether or not to fill messages.
;;;
;;;	Modified 1 March 1980 by R. M. Soley to add hook, take away
;;;	auto check on qsend-ok (sniffle - it doesn't really work), fix
;;;	bug in filling messages, add short-message-accept option.
;;;
;;;	Modified 15 March 1980 for repeat-last-message and send-a-message.
;;;	repeat-last-message: locally display last message sent to you.
;;;	send-a-message: prompts for name/message, sends to any random,
;;;	creating message buffer etc.  (by Soley)
;;;
;;;	All of Soley's improvements integrated/installed 10/4/80 by BSG
;;;
;;;	Modified 19 January 1984 - Barmar - to comment out register-option
;;;	forms, as they were moved to e_option_defaults_.
;;;
;;;	Modified 31 July 1984 - K. P. Fleming - quick fix to use new message
;;;	facility (will be rewritten later).
;;;	Modified 5 October 1984 - B. Margolin - quick fixes to KPF's fixes.

(%include e-macros)
(declare (special known-buflist accept-messages-environment-initp)
         (*lexpr expand-pathname-relative))
(declare (*expr absolute_pathname_ e_lap_$rtrim display-com-error
                exch-point-mark expand_pathname_ runoff-fill-region
	      set-emacs-interrupt-handler user_info_$homedir
	      trim-minibuffer-response e_pl1_$retrieve_olc_message))
(defvar message-mode-hook nil)
(remprop 'accept-messages 'autoload)

(declare (defpl1 e_pl1_$set_message_cleanup "")
         (defpl1 user_info_$whoami ""
	       (return char (32.))
	       (return char (32.)))
         (defpl1 e_pl1_$set_message_handler ""
	       (char (*))
	       (fixed bin(17.))
	       (return fixed bin(35.)))
         (defpl1 e_pl1_$retrieve_message ""
	       (return char (64.) varying)
	       (return char (32.) varying)
	       (return char (2000.) varying))
         (defpl1 e_pl1_$send_message ""
	       (char (*))
	       (char (*))
	       (char (*))
	       (return fixed binary (35.)))
         (defpl1 host_id_$check_id ""
	       (char (*))
	       (bit (36.))
	       (bit (36.))
	       (return fixed bin (32.))
	       (return fixed bin (35.)))
         (defpl1 host_id_$symbol ""
	       (fixed bin (32.))
	       (return char (32.))
	       (return fixed bin (35.)))
         (defpl1 qsend$qsend ""
	       (char (*))
	       (char (*))
	       (char (*)))
         (defpl1 absolute_pathname_$add_suffix ""
	       (char (*))
	       (char (*))
	       (return char (168.))
	       (return fixed bin (35.)))
         (special current-buffer current-buffer-mode conversations
	        last-message-sender-display-variable
	        tty-no-upmotionp last-message-sender last-message-time
	        last-message-mark last-message-error-code fill-prefix
	        short-message-accept message-hook last-message
	        qsend-ok-flag daemon-mbx-dir fill-messages))
;;;

(setq qsend-ok-flag nil			;the default
      daemon-mbx-dir ">user_dir_dir>Daemon>mailboxes"
      conversations nil
      last-message "No last message."
      last-message-sender nil)

;;; What do these options MEAN??? Well...
;;;
;;; If short-message-accept is t, messages will not be put on the
;;; screen in local display; a message of the form "Messages recieved from
;;; Foo.BAR" will appear under the mode line instead.  The default is nil.
;;;
;;; If fill-messages is nil, messages will NOT be filled with
;;; runoff-fill-region.  The default is nil (NOT to fill).
;;;
;;; If message-hook is non-null, it will be funcalled with the following
;;; arguments: sender, time, message, mailbox message was received in.
;;; (The last item will be nil if the message was received in the
;;; default mbx).  There is more to it, though, so be careful.  If the
;;; called function returns nil, NO OTHER ACTION WILL BE PERFORMED ON THE
;;; MESSAGE.  So, if you want it in the buffer or somesuch, do it yourself
;;; of call the routine below that does it.

;;; (register-option 'short-message-accept nil)	;default is long ;moved to e_option_defaults_
;;; (register-option 'fill-messages nil)		;default is to not fill ;moved to e_option_defaults_
;;; (register-option 'message-hook nil)		;default is normal acceptor ;moved to e_option_defaults_

(defun accept-messages-make-sense-of-mbxname (mbx)
       (let ((dots (am-find-chars mbx "."))
	   (greaters (am-find-chars mbx ">")))
	  (let ((inter (cond ((memq 1 greaters) mbx)
			 ((or (> 0 (length greaters))
			      (not (= 0 (index mbx "<"))))
			  (expand-pathname-relative mbx
					        "working_dir"))
			 ((= 0 (length dots))
			  (catenate daemon-mbx-dir ">" mbx))
			 ((samepnamep (substr mbx
					  (- (stringlength mbx)
					     3))
				    ".mbx")
			  (expand-pathname-relative mbx "home_dir"))
			 ((> (length dots) 0)
			  (let ((Name (substr mbx 1 (1- (car dots))))
			        (Project
				(substr mbx (1+ (car dots)))))
			       (catenate ">user_dir_dir>"
				       Project
				       ">"
				       Name
				       ">"
				       Name)))
			 (t nil))))
	       (cond ((null inter) nil)
		   (t (let ((answer
			    (e_lap_$trim
			      (car
			        (absolute_pathname_$add_suffix
				inter
				"mbx")))))
			 (substr answer
			         1 
			         (- (stringlength answer) 4))))))))

(defun am-find-chars (string char)
       (am-find-chars_guts string char 0))

(defun am-find-chars_guts (string char before)
       (let ((where (index string char)))
	    (cond ((= 0 where) nil)
		  (t (cons (+ before where)
			   (am-find-chars_guts (substr string (1+ where))
					       char
					       where))))))

(defun accept-messages-environment ()
       (cond ((or (not (boundp 'accept-messages-environment-initp))
	        (not accept-messages-environment-initp))
	    (e_pl1_$set_message_cleanup)
	    (set-perm-key '^X: 'message-response-command)
	    (set-perm-key '^X/' 'go-to-new-message-buffer)
	    (set-perm-key '^X/` 'send-a-message)
	    (set-perm-key '^X/~ 'repeat-last-message)
	    (setq accept-messages-environment-initp t))))

(defun accept-messages n
       (cond ((= n 0) (accept-messages-default-mbx))
	   (t (mapc 'accept-messages-path (listify n)))))

(defprop accept-msgs accept-messages expr)

;;; Accept messages on a certain path.

(defun accept-messages-path (message-pathname)
       (let ((pathname-of-mbx
	     (accept-messages-make-sense-of-mbxname message-pathname)))
	  (and (null pathname-of-mbx)
	       (display-error (catenate
			    "Invalid mailbox pathname: "
			    message-pathname)))
	  (accept-messages-environment)
	  (let ((error-code
		(e_pl1_$set_message_handler
		  pathname-of-mbx (set-emacs-interrupt-handler
				'console-messages-interrupt-handler
				pathname-of-mbx))))
	       (or (= 0 error-code)
		 (display-com-error error-code
				(catenate "While accepting messages on "
					pathname-of-mbx))))))


;;; Accept messages on default mbx.

(defun accept-messages-default-mbx ()
       (accept-messages-environment)
       (let ((pathname-of-mbx
	     (let ((id (user_info_$whoami)))
		(let ((proj (e_lap_$rtrim (second id)))
		      (pers (e_lap_$rtrim (first id))))
		     (catenate ">udd>" proj ">" pers ">" pers)))))
	  (let ((error-code
		(e_pl1_$set_message_handler
		  pathname-of-mbx
		  (set-emacs-interrupt-handler
		    'console-message-interrupt-handler nil))))
	       (or (= 0 error-code)
		 (display-com-error error-code "While accepting messages."
				)))))


;;; Lisp side of OLC stuff -- see e_pl1_ olc stuff.
;;;	RMSoley 10 April 1980

(declare (defpl1 olcn$olcn2 "" (char (*)) (char (*)))
         (special olc-messages))

;;;(register-option 'olc-messages nil)
(defvar olc-messages nil)			;made invis. option BSG 10/11/80
(defvar keep-unresponded-buffers-modified nil)

(defun console-message-interrupt-handler (intno mbx arg)
       intno				; compiler gets bummed
					; out otherwise
       (do-forever
	 (setq arg (e_pl1_$retrieve_message))
	 (and (= 0 (stringlength (car arg)))
	      (return nil))
	 (console-message-processor (car arg)
			        (cadr arg)
			        (caddr arg)
			        mbx))
       (and olc-messages
	  (do-forever
	      (setq arg (e_pl1_$retrieve_olc_message))
	      (and (= 0 (stringlength (car arg)))
		 (return nil))
	      (console-message-processor (car arg)
				   (cadr arg)
				   (caddr arg)
				   'OLC))))

(defun console-message-processor (sender time message mbx)
       (let ((msender (massage-message-sender sender)))
	  (setq last-message-sender-display-variable
	        (get-message-sender-display sender))
	  (cond ((or (null message-hook)
		   (and message-hook
		        (funcall message-hook msender time message mbx)))
	         (or (cnsmsg-make-qsend-sense msender time message mbx)
		   (cnsmsg-make-mail-sense msender time message mbx)
		   (process-the-message msender time message mbx))))))

(defun process-the-message (msender time message mbx)
       (ring-tty-bell)
       (setq last-message message)
       (let ((buffer-in-progress current-buffer))
	  (save-excursion-buffer
	    (go-to-or-create-message-buffer msender)
	    (let ((display-time
		  (massage-message-time time last-message-time)))
	         (insert-message-into-message-buffer display-time
					     time
					     message
					     mbx)
	         (cond ((or tty-no-upmotionp
			(buffer-on-display-in-window current-buffer)
			(eq buffer-in-progress current-buffer)))
		     (short-message-accept
		       (minibuffer-print "Message received from "
				     msender
				     "."))
		     (t (local-display-message display-time
					 message
					 mbx)))))))

(defun local-display-message (time message mbx)
       (init-local-displays)
       (setq last-message-sender-display-variable
	   (get-message-sender-display last-message-sender))
       (let ((display-time (cond ((< (stringlength time) 4) ":")
			   (t (catenate " (" time "):")))))
	  (local-display-generator-nnl
	    (catenate "Message from " last-message-sender-display-variable
		    display-time))
	  (cond ((null mbx))
	        (t (local-display-generator-nnl
		   (catenate "(Received in mailbox " mbx ")"))))
	  (local-display-generator-nnl message))
       (end-local-displays))
		       
(defun insert-message-into-message-buffer (display-time time message mbx)
       (without-modifying
         (go-to-mark last-message-mark)
         (set-the-mark)
         (insert-string display-time)
         (insert-string ": ")
         (insert-string message)
         (if fill-messages
	   (let ((fill-prefix "   "))
	        (without-saving (runoff-fill-region)))
	   (without-saving (runoff-fill-region)))
         (new-line)
         (if (not (null mbx))
	   (insert-string "(Rec'd. in mbx. ")
	   (insert-string mbx)
	   (insert-string ")")
	   (new-line)))
       (putprop current-buffer "<=" 'message-direction)	;reply necessary
       (if keep-unresponded-buffers-modified
	 (setq buffer-modified-flag t))
       (set-mark-here last-message-mark)
       (go-to-end-of-buffer)
       (setq last-message-time time))

(defun cnsmsg-make-mail-sense (msender time message mbx)
       time		;; goddam lcp
       (cond ((samepnamep (substr message 1 (stringlength "You have mail"))
		      "You have mail")
	    (ring-tty-bell)
	    (minibuffer-print "You have mail from "
			  msender
			  (cond (mbx (catenate " in mailbox " mbx))
			        (t ""))
			  ".")
	    t)
	   (t nil)))

;;;

;;;
;;;	Character munching functions for message beastie.
;;;

(defun massage-message-sender (sender)    ; remove "(from) at system_high"
       (setq sender (massage-message-sender1 sender " ("))
       (setq sender (massage-message-sender1 sender " at")))

(defun massage-message-sender1 (sender string-to-look-for)	; aux function
       (prog (position)
	   (or (= 0 (setq position (index sender string-to-look-for)))
	       (setq sender (substr sender 1 (1- position))))
	   (return sender)))

(defun massage-message-time (new-time old-time)	; make shortest unambiguous
					; time string 
       (prog (date-string)
	   (cond ((samepnamep (substr new-time 1 14.)
			  (substr old-time 1 14.))
		(return "="))
	         (t (cond ((samepnamep (substr new-time 1 8.)
				 (substr old-time 1 8.))
		         (setq date-string ""))
		        (t (setq date-string (substr new-time 1 9.))))))
	   (return (catenate date-string (substr new-time 11. 4)))))

(defun get-message-sender-display (sender)
       (let ((lparen-pos (index sender "(")))
	  (let ((rparen-pos (index (substr sender lparen-pos) ")")))
	       (if (zerop (* lparen-pos rparen-pos))
		 (massage-message-sender1 sender ".")
		 else (catenate (substr sender (1+ lparen-pos)
				    (- rparen-pos 2))
			      " (" (massage-message-sender1 sender ".")
			      ")")))))

;;;
;;; Create message buffer, based on the name of the sender of the message.
;;;

(defun go-to-or-create-message-buffer (sender)
       (prog (person project qspr)
	   (and (setq qspr (cnsmsg-qsend-parse-to sender))
	       (let ((hidr (host_id_$check_id (cadr qspr) 0 0)))
		  (cond ((= 0 (cadr hidr))
		         (putprop
			 (setq sender (car qspr))
			 (e_lap_$trim (car (host_id_$symbol (car hidr))))
			 'net-site))
		        (t
			 (display-com-error (cadr hidr) (cadr qspr))))))
	   (cond ((setq project
		      (get (setq person (make_atom sender)) 'net-site))
		(setq project (cons 'net-host project)))
	         (t (setq person (massage-message-sender1 sender "."))
		  (or (= (stringlength person) (stringlength sender))
		      (setq project
			  (substr sender
				(+ 2 (stringlength person)))))))
	   (go-to-or-create-buffer
	     (implode (append (explodec "Messages from ")
			  (explodec person))))
	   (setq last-message-sender person)
	   (go-to-end-of-buffer)
	   (cond ((empty-buffer-p current-buffer)  ;S.O.B.  may have killed
					; the buffer, eh!?
	       (cond ((not project)
		    (display-error-noabort
		      "User name must include project. "
		      sender)
		    (return nil)))
	       (putprop current-buffer person 'message-person)
	       (putprop current-buffer project 'message-project)
	       (putprop current-buffer "" 'message-direction)
	       (register-local-var 'last-message-time)
	       (register-local-var 'last-message-error-code)
	       (register-local-var 'last-message-mark)
	       (setq last-message-time "01/01/01  0000.0 GMT Tue" 
		   last-message-error-code 0)
	       (setq conversations (cons last-message-sender conversations))
	       (setq current-buffer-mode 'Message)
	       (set-key 'CR 'respond-from-buffer)
	       (without-modifying
	         (insert-string current-buffer)
	         (insert-string ":")
	         (new-line)
	         (new-line))
	       (setq last-message-mark (set-mark))
	       (and message-mode-hook (funcall message-mode-hook))))
	   (return t)))

(defun message-buffer-prompter ()
       (let ((completion-list conversations))
	  (let ((ans (trim-minibuffer-response
		     (cond (last-message-sender
			   (catenate "Messages to/from ("
				   last-message-sender "): "))
			 ('else "Messages to/from: "))
		     NL)))
	       (cond ((not (nullstringp ans)) ans)
		   (last-message-sender last-message-sender)
		   ('else (display-error "No message buffers."))))))

;;; ^X-' -- prompt for message buffer name.
(defun go-to-new-message-buffer ()
       (cond (numarg
	     (list-message-buffers))
	   (t
	     (let ((message-name (message-buffer-prompter))
		 (prev current-buffer))
		(go-to-or-create-message-buffer message-name)
		(select-buffer-window current-buffer 4)
		(setq previous-buffer prev)))))

(defun list-message-buffers ()
       (let ((msg-buffers-info nil)
	   (original-buffer current-buffer)
	   (previous-buffer previous-buffer))
	  (mapc
	    (function
	      (lambda (bufname)
		    (go-to-buffer bufname)
		    (cond ((eq current-buffer-mode 'Message)
			 (setq msg-buffers-info
			       (cons (list (get bufname 'message-person)
				         (get bufname 'message-direction)
				         (get bufname 'message-project))
				   msg-buffers-info))))))
	    known-buflist)
	  (go-to-or-create-buffer original-buffer)
	  (and (null msg-buffers-info)
	       (display-error "No message buffers."))
	  (init-local-displays)
	  (mapc 'local-display-generator-nnl
	        '("Listing of Current Message Buffers"
		 ""
		 "Direction 	Person"
		 ""))
	  (mapc '(lambda (info)
		       (local-display-generator-nnl
		         (catenate
			 "    "
			 (cadr info)
			 TAB
			 TAB
			 (car info)
			 (cond ((atom (caddr info))
			        (catenate "." (caddr info)))
			       (t ""))
			 (cond ((let ((site
				      (get (make_atom (car info)) 'net-site)))
				   (and site (catenate " @ " site))))
			       (t "")))))
	        msg-buffers-info)
	  (local-display-generator-nnl "")
	  (end-local-displays)))


;;;

;;;
;;;	Message sending commands.
;;;	 Send line in message buffer to other end of conversation.
;;;

(defun respond-from-buffer ()			; ^M in Message mode.
       (prog (error-code)
	   (cond ((not (atom (get current-buffer 'message-project)))
		(cnsmsg-qsend (cdr (get current-buffer 'message-project))
			    (get current-buffer 'message-person)
			    (curline-as-string-nnl))
		(go send-done)))
	   (cond ((= last-message-error-code
		   (setq error-code
		         (e_pl1_$send_message (get current-buffer 'message-person)
				    (get current-buffer 'message-project)
				    (curline-as-string-nnl)))))
	         (t
		 (setq last-message-error-code error-code)
		 (cond ((= 0 error-code)
		        (display-error-noabort
			"Message sent successfully to "
			(get current-buffer 'message-person)
			"."
			(get current-buffer 'message-project)))
		       (t
		         (display-com-error-noabort
			 error-code
			 (get current-buffer 'message-person)
			 "."
			 (get current-buffer 'message-project))))))
	   send-done)
       (without-modifying
         (set-the-mark)
         (go-to-beginning-of-line)
         (insert-string "Reply: ")
         (exch-point-mark der-wahrer-mark)
         (and fill-messages ((lambda (fill-prefix)
			       (setq fill-prefix fill-prefix)
			       (without-saving (runoff-fill-region)))
		         "-> ")))
       (new-line)
       (putprop current-buffer "=>" 'message-direction)
       (set-mark-here last-message-mark)
       (setq buffer-modified-flag nil))		; rather than fighting it.

(defun message-response-command ()		; ^X: strikes again.
       (cond (numarg
	     (cond (last-message-sender
		   (go-to-or-create-message-buffer last-message-sender))
		 (t
		   (display-error "No message buffers."))))
	   (t
	     (cond (last-message-sender
		   ((lambda (message)
			  (save-excursion-buffer
			    (cond ((not (= 0 (stringlength message)))
				 (go-to-or-create-message-buffer
				   last-message-sender)
				 (go-to-mark last-message-mark)
				 (without-modifying
				   (insert-string message)
				   (new-line)
				   (backward-char))
				 (respond-from-buffer)))))
		    (minibuf-response
		      (catenate "To " last-message-sender ": ") NL)))
		 (t
		   (display-error "No one to respond to."))))))

(defun curline-as-string-nnl ()		; remove extra newline 'cause
       ((lambda (str)			; send_message won't.
	      (substr str 1 (1- (stringlength str))))
        (curline-as-string)))

;;;
;;;
;;;	Qsend cruft 4/15/79
;;;

(defun cnsmsg-make-qsend-sense (sender time msg mbx)
       (prog (tox hdr)
	   (or (samepnamep
	         (substr sender 1 (stringlength "Network_Server"))
	         "Network_Server")
	       (return nil))
	   (cond ((samepnamep (substr msg 1 (stringlength "You have mail"))
			  "You have mail")
		(ring-tty-bell)
		(display-error-noabort "You have network mail.")
		(return t)))		;Don't process any further.
	   (setq tox (index msg "To:"))
	   (cond ((= tox 0)
		(setq tox (index msg "to:"))
		(cond ((= tox 0)
		       (setq tox (index msg "TO:"))
		       (and (= tox 0)
			  (return nil))))))	;no sense made.
	   (setq hdr (e_lap_$trim (substr msg 1 (1- tox)))
	         msg (substr msg tox))
	   (and (member (substr hdr 1 5) '("From:" "FROM:" "from:"))
	        (setq hdr (e_lap_$trim (substr hdr 6))))
	   (setq hdr (cnsmsg-qsend-parse-to hdr))
	   (or hdr (return nil))
	   (console-message-processor (car hdr) time msg mbx)
	   (return t)))

(defun qsend-ok () (setq qsend-ok-flag t))

(defun cnsmsg-qsend (host person msg)
       (if (not qsend-ok-flag)
	 (display-error "You have not the right to send interactive net mail."))
       (minibuffer-print "Qsending to " person " at " host ".")
       (qsend$qsend host person msg)
       (minibuffer-clear))

(defun cnsmsg-qsend-parse-to (x)
       (prog (sender site tox)
	   (or (= 0 (index x TAB))
	       (setq x (maknam
		       (mapcar '(lambda (y)(cond ((= y 11) 40)(t y))) x))))
	   (setq tox (index x "@"))
	   (cond ((> tox 0)
		(setq sender (e_lap_$trim (substr x 1 (1- tox)))
		      x (e_lap_$trim (substr x (1+ tox)))))
	         (t
		 (setq tox (index x " at "))
		 (and (= tox 0)(setq tox (index x " AT ")))
		 (and (= tox 0)(setq tox (index x " At ")))
		 (and (= tox 0)(setq tox (index x " -at")))
		 (and (= tox 0)(return nil))
		 (setq sender (e_lap_$trim (substr x 1 (1- tox)))
		       x (e_lap_$trim (substr x (+ tox 4))))))
	   (setq tox (index x " "))
	   (and (= tox 0)(setq tox (1+ (stringlength x))))
	   (setq site (substr x 1 (1- tox)))
	   (setq site (make_atom site) sender (make_atom sender))
	   (putprop sender site 'net-site)
	   (return (list sender site))))

;;;
;;; More additions!!!
;;;

(defcom repeat-last-message
        &doc "Repeats via local display the last message received."
        (and (null last-message-sender)
	   (display-error "No last message."))
        (local-display-message last-message-time
			 last-message
			 nil))

(defcom send-a-message
        &doc "Prompts for a name and message, and sets up a message
buffer for that recipient and sends the message without leaving the
current buffer."
        (save-excursion-buffer
	(and (eq (go-to-new-message-buffer) 'couldnt-get-it)
	     (command-quit))
	(insert-string
	  (minibuf-response (catenate "To "
				(get current-buffer 'message-person)
				": ")
			NL))
	(respond-from-buffer)))

(defun message-sender-internal (to message)
       (save-excursion-buffer
         (cond ((not (= 0 (stringlength message)))
	      (go-to-or-create-message-buffer to)
	      (go-to-mark last-message-mark)
	      (without-modifying
	        (insert-string message)
	        (new-line)
	        (backward-char))
	      (respond-from-buffer)))))




;;;
;;; Soley's pathname hack .. at sometime, may make standard, but
;;; for now, leave in console-messages.... -BSG
;;;


;;; Pathname expander for start up emacs
;;;	RMSoley 10 January 1980
;;;	         5 March   1980 to lexprize epr, add default_working_dir key
;;;	        13 March   1980 for zero arguments to epr (path = ""),
;;;			     remove non-working dwdir (since dwd doesn't
;;;			     return right thing)
;;;

;;; expand-pathname-relative is a way to expand a path relative to anywhere
;;; A bit better than e_pl1_$pathname_util, since it accepts "<foo".
;;; Basically hands back a pathname equal to 'path relative to 'relative,
;;; unless relative is one of the following keys:

;;;	nil, "", or "working_dir" . . . expand relative to working dir
;;; 	"home_dir"  . . . . . . . . . . expand relative to home dir
;;;	"process_dir" . . . . . . . . . expand relative to process dir

;;; If path begins with a >, relative is ignored (path taken to be absolute)
;;; If relative is missing, assumes working directory
;;; If path & relative are both missing, returns wdir.
;;; If path = "" and relative is non-null, returns the directory without
;;;	a trailing ">"

(defun expand-pathname-relative lexpr
       (and (> lexpr 2)
	  (display-error
	    "expand-pathname-relative: "
	    "Wrong number of arguments supplied."))
       (let ((path (cond ((< lexpr 1) "")
		     (t (arg 1))))
	   (relative (cond ((< lexpr 2) nil)
		         (t (arg 2)))))
	  (and (not (= 0 (caddr (expand_pathname_ relative))))
	       (display-error
	         "expand-pathname-relative: Error in syntax of relative."))
	  (and (not (= 0 (caddr (expand_pathname_ path))))
	       (display-error
	         "expand-pathname-relative: Error in syntax of pathname."))
	  (let ((rel (e_lap_$rtrim
		     (cond ((null relative) (absolute_pathname_ ""))
			 ((nullstringp relative) (absolute_pathname_ ""))
			 ((samepnamep relative "working_dir")
			  (absolute_pathname_ ""))
			 ((samepnamep relative "home_dir")
			  (user_info_$homedir))  ;used user_info BSG 10/4/80
			 ((samepnamep relative "process_dir")
			  process-dir)	;used vbl BSG 10/4/80
			 (t (expand-pathname-relative relative
						"working_dir"))))))
	       (cond ((samepnamep (substr path 1 1) ">") path) ;absolute
		   ((= lexpr 0) rel)	;return wdir
		   ((null path) rel)
		   ((nullstringp path) rel)
		   (t (expand-pathname-fix-ups
		        (catenate rel	;really expand
			        ">"
			        path)))))))

;;; The guts.  Takes care of those nasty less thans.
;;; ">udd>foo<bar" and ">udd>foo><bar" both => ">udd>bar"

(defun expand-pathname-fix-ups (path)
       (let ((where-up (index path "<")))
	  (cond ((zerop where-up) path)
	        (t (expand-pathname-fix-ups
		   (catenate
		     (substr path
			   1
			   (- where-up
			      (index (implode
				     (reverse
				       (explodec
				         (substr path 1 (- where-up 2)))))
				   ">")
			      1))
		     (substr path (1+ where-up))))))))

