;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Bull Inc., 1988                *
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;
;;;
;;;	e_interact_
;;;
;;;	All that of the basic editor which deals with being interactive
;;;	commands, prefixes, etc., as opposed to being an editor.


;;; HISTORY COMMENTS:
;;;  1) change(84-01-30,Margolin), approve(), audit(), install():
;;;     pre-hcom history:
;;;               Split off from e_ when he grew too gravid.
;;;               BSG 8/4/78
     
;;;               Modified 1978.11.27-29 to reorganize interrupt stuff, etc. by rsl.
;;;               Macro facility redone 2/11/79 by BSG.
;;;               Modified 06/20/79 by GMP for CTL prologue/epilogue handlers.
;;;               Modified 08/21/79 by GMP for negative arguments.
;;;               Modified: August 1979 by GMP for new command invocation mechanism.
;;;               Modified: June 1981 by RMSoley for understanding of emacs_ call.
;;;               Modified: July 1981 RMSoley for pl1 argument parsing, and support
;;;                         of multiple emacs's, tasking.
;;;               Modified: March 1982 RMSoley for undo.
;;;               Modified: June 1982 B Margolin - get-top-level-char-innards nulls
;;;                         out previous-command after echo-negotiation.  Also, last-input-char
;;;                         is maintained by get-a-char, not process-char, so it is
;;;                         more correct.  Added JSL's new command executor stuff.
;;;                         Set up the &undo property on more commands.
;;;               Modified: 25 November 1983 B. Margolin to add "^[" as a valid
;;;                         escape prefix in parse-key-description.
;;;               Modified: 19 January 1984 B. Margolin to comment out register-option
;;;                         forms, as they were moved to e_option_defaults_.
;;;               Modified: 19 January 1984 Barmar to reject esc-<number> in genset-key.
;;;               Modified: 30 January 1984 Barmar to fix kmacro-display-interpret to
;;;               properly interpret "ESC +NUM" and meta characters.
;;;  2) change(84-12-25,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to fix wrong-type-arg error
;;;     in multiplier command, change lambda into let, use defmacro.
;;;  3) change(84-12-26,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to fix bug in the rewrite of permanently-set.
;;;  4) change(84-12-30,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to remove top-level setq of
;;;     suppress-remarks, as it has been set in e_option_defaults_; changed
;;;     set-emacs-interrupt to grow the handler and handle arrays if
;;;     necessary, changed extended-command to ignore null command lines,
;;;     fixed some problems in key binding management, changed special
;;;     declaration to defvar, moved %include's to before declarations.
;;;  5) change(88-01-07,Schroth), approve(88-02-29,MCR7851),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Implement 8-bit extended ASCII I/O.  Used 'new' macros in various
;;;     places to improve readibility.
;;;  6) change(88-01-07,Schroth), approve(88-02-29,MCR7852),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Added support for split-screen display: revert to one split on
;;;     exit and restore screen splits when later restarted.
;;;                                                      END HISTORY COMMENTS


(declare (genprefix /!eia_))

(%include e-macros)
(%include backquote)
(%include defmacro)
(declare (macros nil))
(%include e-define-command)
(%include other_other)
(%include sharpsign)

(declare (*lexpr display-error display-com-error display-error-noabort
	       display-com-error-noabort minibuffer-print
	       minibuffer-print-noclear minibuffer-remark))
(declare (*expr DCTL-epilogue DCTL-prologue assert-minor-mode clear-the-screen
	      convert_status_code_ cur-hpos decimal-rep display-init
	      e_argument_parse_$get_one_path e_pl1_$init
	      e_argument_parse_$get_startup_info
	      e_argument_parse_$new_arguments e_lap_$rtrim e_lap_$trim
	      e_pl1_$assign_channel e_pl1_$dump_output_buffer
	      e_pl1_$echo_negotiate_get_char e_pl1_$get_char
	      e_pl1_$get_editing_chars e_pl1_$get_emacs_interrupt 
	      e_pl1_$get_emacs_interrupt_array e_pl1_$real_have_chars
	      e_pl1_$set_break_char e_pl1_$set_emacs_tty_modes 
	      e_pl1_$set_multics_tty_modes e_tasking_$destroy_me
	      e_pl1_$set_extended_ascii e_pl1_$get_output_conv_table
	      e_tasking_$get_tasking_flag e_tasking_$quit echo-buffer-clear
	      echo-buffer-clear-all echo-buffer-outprint echo-buffer-print
	      echo-buffer-rubout echo-buffer-utter editor-main-init
	      emacs$set_emacs_return_code empty-buffer-p end-local-displays
	      error_table_ exists-file find-file-subr full-redisplay
	      get-buffer-state go-to-or-create-buffer init-local-displays
	      intern-minibuf-response jetteur-des-gazongues
 	      lisp-quit-function loadfile local-display-generator-nnl
	      map-over-emacs-buffers minibuf-response negate-minor-mode
	      nullstringp randomize-redisplay rdis-find-non-displayable
	      redisplay ring-tty-bell
	      set-autoload-lib set-lisp-rdis-meters user_info_$homedir
 	      user_info_$whoami yesp))
(declare (array* (notype (key-bindings 256. 2))))
(declare (array* (notype (saved-command-history 50.))))
(array saved-command-history t 50.)

;;; The key binding arrays.
;;; These are created and initialized at dump time so that the
;;; user needn't wait through them at startup time.

(array key-bindings t 256. 2)
(fillarray 'key-bindings '(undefined-command))

(array alternate-key-bindings t 256. 2)
(fillarray 'alternate-key-bindings '(undefined-command))

(setq permanize-key-bindings t)

(defvar (

	 network-flag		;tty is a TELNET connection
	 *transparent-commands*	;never becomes previous-command
	 last-time-sample		;for command bell
	 command-bell		;option for command bell
	 command-bell-count		;also
	 meter-commands		;option for command metering
	 completion-list		;for complete-command, ESC-SPACE
	 tty-type			;Terminal type.
	 NowDumpingEmacs		;t at dump time
	 tasking-emacs		;t if we are running tasked.
	 tasking-restarted		;t if we've been restarted.
	 emacs-name		;called name of this incarnation:
				;emacs/new_emacs/emacs_
	 suppress-remarks		;suppress utterances by mbuf-remark
	 quit-on-break		;whether or not to quit on typed BREAK
				;during emacs_ invocation.
	 documentation-dir		;for help command
	 history-next-index		;for command history
	 next-multics-argno
	 buffer-minor-modes		;used for checking macro hack
	 buffer-modified-flag
	 suppress-redisplay-flag	;controls whether redisplay is enabled
	 e-quit-transparency	;Allows quits not to screen-hack
	 DCTL-prologue-availablep	;terminal needs hacking on setting Emacs tty modes
	 DCTL-epilogue-availablep	;terminal needs hacking on setting Multics tty modes
	 delayed-interrupt		;interrupt went off in minibuffer
	 damaged-flag		;redisplay, do all the work!
	 minibufferp		;in Minny Buffer.
	 known-buflist		;list of known buffers
	 kparse-list		;used in key parsing.
	 numarg			;numeric argument to current function, or nil if none
	 undo			;whether or not to undo; like numarg
	 MCS-editing-characters	;list of MCS escape (\), erase (#), and kill (@) characters
	 MCS-escape-character	;MCS escape character (\)
	 emacs-epilogue-action-list	;things to be done on exit
	 last-input-char		;current char, for self-inserts
	 last-command-triplet-1	;current/last command being executed
	 last-command-triplet-mpfxk	;continuation of above, encoded.
	 current-buffer		;symbol name of current buffer
	 list-of-known-options	;for option mechanism, list thereof.
	 macrostack		;format is (macro restart count)
	 macro-collection-in-progress
	 last-macro-definition
	 macro-execution-in-progress	;pointer on current xec list
	 nobreak-functions		;functions that don't break echnego
	 (per-buffer-key-bindings nil)	;as it says, assq list
	 permanize-key-bindings	;on at init time, until start-up over
	 previous-buffer		;buffer we came from
	 current-command		;command being executed
	 previous-command		;last command invoked in this Emacs
	 previous-argument-list	;argument list used to invoke last cmd
	 user-display-variable	;for user opinion on mode line
	 locechnego-ctr		;meters
	 locechnego-meter	
	 next-interrupt-channel	;lowest unused entry in interrupt handler table
	 recursion-level		;records when interrupt handler may cause a redisplay
	 e-lisp-error-mode		;how to treat lisp errors
	 inhibit-default-start-up-execution
	 emacs-start-ups-need-running
	 NLCHARSTRING		;a newline as string object
	 TAB			;ascii TAB
	 ESC			;ascii escape
	 CRET			;carriage return symbol
	 CR			;that too
	 NL
	 FF			;Formfeed
	 VT			;Vertical Tab
	 pi-handler-minibuffer-print	;put in mbuf by pi-handler
	 args:apply-arg
	 args:ns
	 args:paths
	 args:ll
	 args:pl
	 tasking-arg
	(DCTL-extended-ascii nil)		;terminal can do 8bit ASCII
	 char-input-mask			;177 normally or 377 if 8-bit
	 ))

(defvar (
	split-mode-p			;on if split screens
	))
(declare (*expr rdis-restore-screen-to-one-split
	      rdis-recreate-splits-on-screen))

(defun display-load-time-error n
       (princ (apply 'catenate (listify n)))
       (terpri)
       (break load-time-error t))

(putprop 'display-error
         '(lambda n (apply 'display-load-time-error (listify n))) 'expr)
(putprop 'minibuffer-print
         '(lambda n (apply 'display-load-time-error (listify n))) 'expr)




;;; Macros to test bits in left-half of a fixnum: (tlnX value mask)
(defmacro tlnn (value mask)			;t if any bit is on
	`(not (tlne ,value ,mask)))

(defmacro tlne (value mask)			;t if all bits are off
	`(zerop (logand ,value (lsh ,mask 18.))))
;;;

;;;
;;;	Character function binding generators.
;;;


(defmacro permanently-set (&body forms)
	`(let ((permanize-key-bindings t))
	      .,forms))


(defcom set-perm-key
        &arguments ((key &symbol &prompt "Key: ")
		(function &symbol &prompt "Function: "))
        &numeric-argument (&reject)
        (permanently-set (set-key key function)))

(defcom-synonym set-permanent-key set-perm-key)


(defcom set-key
        &arguments ((key &symbol &prompt "Key: ")
		(function &symbol &prompt "Function: "))
        &numeric-argument (&reject)
        (let ((result (parse-key-description key)))
	   (genset-key (caddr result) (car result) (cadr result) function)))


;;;
;;;	This is the setter of all keys.
;;;

(defvar permit-setting-esc-number nil)

(defun genset-key (prefix metap key function)
       (or permit-setting-esc-number
	 (= metap 0)
	 (and (not (= key (CtoI "+")))
	      (not (= key (CtoI "-")))
	      (or (< key (CtoI "0"))
		(> key (CtoI "9"))))
	 ;; esc-<number>
	 (display-error "esc-<number> may not be bound."))
       (and (or prefix (= metap 1)) (> key (1- (CtoI "a"))) (< key (1+ (CtoI "z")))
	  (setq key (- key 40)))
       (cond (prefix
	     (or (not (symbolp (key-bindings prefix 0)))
	         (genset-key nil 0 prefix
			 (let ((x (fillarray (*array (gensym) t 256.) '(undefined-command))))
			      (store (x 7) 'ignore-prefix)
			      x)))	;make ^G punt prefix only
	     (setq metap (key-bindings prefix 0))    ; this is array.
	     (cond (permanize-key-bindings
		   (remove-local-key-binding 0 key prefix))  ;override
		 ((arraycall t metap key)     ;one there already
		  (update-perm-key-bindings 0 key prefix (arraycall t metap key))
		  (update-local-key-bindings 0 key prefix function)))
	     (store (arraycall t metap key) function))
	   (t (cond (permanize-key-bindings
		    (remove-local-key-binding metap key nil))
		  ((key-bindings key metap)
		   (update-perm-key-bindings metap key nil (key-bindings key metap))
		   (update-local-key-bindings metap key nil function)))
	      (or NowDumpingEmacs
		(cond ((memq (key-bindings key metap) nobreak-functions)
		       (e_pl1_$set_break_char key 1)))
		(cond ((memq function nobreak-functions)
		       (e_pl1_$set_break_char key 0))))
	      (store (key-bindings key metap) function))))

(defun update-perm-key-bindings (metap key prefix function)
       (let ((keyrep (key-total-printed-symbol metap key prefix)))
	  (or (and (not permanize-key-bindings)  ;this redundant clause is a way out
		 (assq keyrep per-buffer-key-bindings))  ;dont overpush
	      (putprop keyrep function 'perm-key-function))))

(defun update-local-key-bindings (metap key prefix function)
       (let ((keyrep (key-total-printed-symbol metap key prefix))
	   (listrep (key-fixnum-rep-encode metap key prefix)))
	  (let ((assq-answer (assq keyrep per-buffer-key-bindings)))
	       (cond (assq-answer (rplaca (cdr assq-answer) function))
		   (t (setq per-buffer-key-bindings
			  (cons (cons keyrep (cons function listrep))
			        per-buffer-key-bindings)))))))

(defun remove-local-key-binding (metap key prefix)
       (let ((key-symbol (key-total-printed-symbol metap key prefix)))
	  (let ((assq-answer
		(assq key-symbol per-buffer-key-bindings)))
	       (if assq-answer
		 (setq per-buffer-key-bindings
		       (delq assq-answer per-buffer-key-bindings))))))

(defun key-total-printed-symbol (metap key prefix)
       (intern (make_atom (cond (prefix (catenate (printable prefix)(printable key)))
			  ((= 0 metap)(printable key))
			  (t (catenate "esc-" (printable key)))))))


;;; Get printable name of a key
(defun get-key-name (key-list)
       (apply 'key-total-printed-symbol key-list))


(defun key-fixnum-rep-encode (metap key prefix)
       (list metap key prefix))


(defun reorganize-local-key-bindings (revert)
       (let ((permanize-key-bindings t)
	   (saved-local-bindings (append per-buffer-key-bindings nil)))  ;copy list
	  (unwind-protect
	    (progn (mapc '(lambda (x)
			      (prog (y)
				  (setq y (cond (revert (get (car x) 'perm-key-function))
					      (t (cadr x))))
				  (or (caddr (cddr x))   ;-non-prefix first
				      (genset-key nil (car (cddr x)) (cadr (cddr x)) y))))
		       per-buffer-key-bindings)
		 (mapc '(lambda (x)
			      (prog (y)
				  (setq y (cond (revert (get (car x) 'perm-key-function))
					      (t (cadr x))))
				  (and (caddr (cddr x))  ; prefixed ones
				       (genset-key (caddr (cddr x)) 0 (cadr (cddr x)) y))))
		       per-buffer-key-bindings))
	    (setq per-buffer-key-bindings saved-local-bindings))))

(defun revert-local-key-bindings ()(reorganize-local-key-bindings t))

(defun instate-local-key-bindings ()(reorganize-local-key-bindings nil))


(defun printable (x)
       (let ((y (cond ((numberp x) x)
		  (t (getcharn x 1)))))
	  (cond ((bit-test 200 y) (printable-8-bit-char y))    ;8-bit or META
	        ((= y 33) "ESC")
	        ((= y 15) "CR")
	        ((= y 177) "\177")
	        ((= y 40) "SPACE")
	        ((< y 40) (catenate "^" (ascii (bit-set 100 y))))
	        ((numberp x)(ascii x))
	        (t x))))

(defun printable-8-bit-char (ch-num)
       ;; the display rep of char that is either an 8-bit ASCII or a meta char
       (cond (DCTL-extended-ascii (printable-extended-ascii ch-num))
	   (t (printable-meta-char ch-num))))

(defun printable-extended-ascii (ch-num)
       ;; returns the display representation of an 8-bit ASCII code
       (let ((ch (ascii ch-num)))
	  (cond ((> (rdis-find-non-displayable ch 1 0) 0) ch)  ;displayable
	        (t (catenate "ext-" (printable (bit-clear 200 ch-num)))))))	;not displayable

(defun printable-meta-char (ch-num)
       ;; returns the display rep of a meta-char
       ;; For R11/ITS telnet ^_l
       (catenate "meta-" (printable (bit-clear 200 ch-num))))

(defun prinkeyrep-to-num (x) (cadr (parse-key-description x)))   ;compatibility

;;; Swaps "alternate-key-bindings" (the emacs_ table) with "key-bindings,"
;;; the standard full-emacs table.
(defun swap-binding-tables ()
       (do a 0 (1+ a) (= a 2)
	 (do b 0 (1+ b) (= b 256.)
	     (store (key-bindings b a)
		  (prog1 (alternate-key-bindings b a)
		         (store (alternate-key-bindings b a)
			      (key-bindings b a)))))))

;;;
;;;	Full-hog key parser,
;;;	BSG 8/5/78 Saturday morning.
;;;


(defun parse-key-description (desc)		;returns  (m k p)
       (prog (prefix metap key)
	   (setq kparse-list (exploden desc))	;char-by-char
	   (cond ((or (parse-key-match-list '(e s c a p e -))
		    (parse-key-match-list '(e s c -))
		    (parse-key-match-list '(m e t a -))
		    (parse-key-match-list '(m -))
		    (parse-key-match-list '(^ [ -))
		    (parse-key-match-list '(^ [)))
		(setq metap 1))
	         (t (setq metap 0)
		  (setq prefix (parse-key-description-syllable desc))  ;try for 1 frob.
		  (or kparse-list (return (list 0 prefix nil)))	;non-meta, non-prefix
		  (parse-key-match-list '(-))	;rip out minus
		  (or kparse-list (kparse-error desc))
		  (or (< prefix (CtoI " "))
		      (kparse-error (catenate (printable prefix)
					" may not be a prefix character.")))))

	   (setq key (parse-key-description-syllable desc))
	   (and (or (= 1 metap) prefix) (> key (1- (CtoI "a")))
	        (< key (1+ (CtoI "z")))(setq key (- key 40)))
	   (and kparse-list (kparse-error desc))
	   (return (list metap key prefix))))


(defun parse-key-description-syllable (desc)
       (cond ((not kparse-list)(kparse-error desc))
	   ((= (car kparse-list) 136)		;control frob, = "^"
	    (setq kparse-list (cdr kparse-list))
	    (cond ((not kparse-list) 136)	;plain old hat
		(t (parse-key-controllify))))
	   ((or (parse-key-match-list '(c -))
	        (parse-key-match-list '(c t l -))
	        (parse-key-match-list '(c o n t r o l -)))
	    (parse-key-controllify))
	   ((and DCTL-extended-ascii		;added Dec 84 EDSchroth
	         (or (parse-key-match-list '(x -))
		   (parse-key-match-list '(e x t -))
		   (parse-key-match-list '(e x t e n d e d -))))
	    (parse-key-extendify desc))	;make it an extended ASCII
	   ((parse-key-match-list '(e s c)) 33)
	   ((parse-key-match-list '(c r)) 15)
	   ((parse-key-match-list '(\ /1 /7 /7)) 177)
	   ((parse-key-match-list '(t a b)) 11)
	   ((parse-key-match-list '(s p a c e)) 40)
	   ((parse-key-match-list '(s p)) 40)
	   (t (prog1 (car kparse-list)
		   (setq kparse-list (cdr kparse-list))))))


(defun parse-key-controllify ()
       (or kparse-list (kparse-error "Unspecified control character."))
       (let ((kdesc (car kparse-list)))
	  (and (> kdesc (1- (CtoI "a")))
	       (< kdesc (1+ (CtoI "z")))
	       (setq kdesc (- kdesc 40)))
	  (or (and (< kdesc (1+ (CtoI "_")))
		 (> kdesc (1- (CtoI "@"))))
	      (kparse-error (catenate "^" (ascii kdesc) " is not ASCII.")))
	  (setq kparse-list (cdr kparse-list))
	  (- kdesc 100)))

;;; Handles extended ascii key descriptions. Dec 84 EDSchroth
(defun parse-key-extendify (desc)
       (or kparse-list (kparse-error "Unspecified extended character."))
       (bit-set 200 (parse-key-description-syllable desc))) ;make 8-bit ASCII

(defun parse-key-match-list (matchee)
       (do ((data kparse-list (cdr data))
	  (pattern matchee (cdr pattern))
	  (chara)(charb)(chardata))
	 ((null pattern)(setq kparse-list data) t)
	 (or data (return nil))		;nothing more
	 (setq chardata  (car data))
	 (setq chara (getcharn (car pattern) 1))
	 (setq charb (cond ((and (< chara (1+ (CtoI "z")))
			     (> chara (1- (CtoI "a"))))
			(- chara 40))
		         (t chara)))
	 (or (= chardata chara)(= chardata charb)(return nil))))


(defun kparse-error (desc)
       (display-error "Invalid key description: " desc))


;;;
;;;  Randomness
;;;


(setq NLCHARSTRING (ItoC 012) ESC (ascii 033))
(setq TAB (ascii 011) BACKSPACE (ascii 010) SPACE (ascii 040) CR (ascii 15)
      CRET (ascii 015) NL (ascii 012) FF (ascii 14) VT (ascii 13))

;;;

;;;
;;;	Initialize the option mechanism first.
;;;


(setq list-of-known-options nil)

(defun require-symbol (putative-symbol)
       (cond ((not (symbolp putative-symbol))
	    (display-error "This function requires a symbol."))))


(defun register-option (sym val)
       (require-symbol sym)
       (or (memq sym list-of-known-options)
	 (setq list-of-known-options
	       (sort (cons sym list-of-known-options) 'alphalessp)))
       (remprop sym 'value-must-be-numeric)
       (remprop sym 'value-ok-true-false)
       (or (boundp sym)(set sym val)))


;;;(register-option 'eval:eval t) ; Unfortunately ;;; moved to e_option_defaults_
;;;(register-option 'eval:assume-atom nil) ;;; moved to e_option_defaults_
;;;(register-option 'eval:correct-errors nil) ;;; moved to e_option_defaults_
;;;(register-option 'eval:prinlevel 3) ;;; moved to e_option_defaults_
;;;(register-option 'eval:prinlength 6) ;;; moved to e_option_defaults_

;;;
;;;	Listener
;;;	and toplevels.
;;;


(defun listener-level () (start) (std-yggdrasil))

(defun start ()
       (setq emacs-start-ups-need-running nil)	;11/3/80
       (setq e-quit-transparency nil)
       (setq e-lisp-error-mode nil)		;Lisp errors to minibuf
       (and (eq emacs-name 'emacs_) (swap-binding-tables))
       (get-editing-characters)		;read erase, kill, escape chars
       (editor-main-init)			;init the guts of the editor.
       (or (boundp 'next-multics-argno) (setq next-multics-argno 1))
       (setq history-next-index 0)
       (setq macro-collection-in-progress nil previous-command nil
	   last-macro-definition nil)
       (setq emacs-epilogue-action-list nil)
       (sstatus cleanup '((run-emacs-epilogue-actions)))
       (*rset nil)
       (e_pl1_$set_emacs_tty_modes)
       (sstatus mulpi t)
       (sstatus interrupt 16. 'emacs-quit-handler)
       (sstatus mulquit 16.)
       (init-echnego-bittab)
       (setq errlist '((pi-handler)))		;CTRL/g escapes lossages
       (setq user-display-variable nil)		;init rsl's hack.
       (display-init)			;initialize the redisplay
       (init-extended-ascii-land)		;fix up for possible 8-bit
       (setq locechnego-ctr 0 locechnego-meter 0) ;And the PL/I redisplay.
       (interrupt-init)			;And the interrupt scheme.
       (setq permanize-key-bindings nil)
       (reset-top-level-editor-state)
       (setq emacs-start-ups-need-running t))

;;; Initialize the 8-bit printing character scan table at dump time
;;; This should be in e_redisplay_ but is done here as the per invocation
;;; stuff is done here.

(declare
  (array* (fixnum (7bit-tabscan-table 128.)))	;7bit non-printing
  (array* (fixnum (8bit-tabscan-table 128.))))	;8bit non-printing
(defvar 7bit-tabscan-table
        (fillarray (*array '7bit-tabscan-table 'fixnum 128.) '(-1)))
(defvar 8bit-tabscan-table
        (fillarray (*array '8bit-tabscan-table 'fixnum 128.) '(-1)))

(do ((i 8. (1+ i)))				;040...173 print nicely
    ((= i 31.))
    (store (arraycall fixnum 7bit-tabscan-table i) 0)
    (store (arraycall fixnum 8bit-tabscan-table i) 0))
(store (arraycall fixnum 7bit-tabscan-table 31.) 777)	;nix 177
(store (arraycall fixnum 8bit-tabscan-table 31.) 777)	;nix 177

;;; Takes care of per invocation set-up for extended ASCII
;;; Dec 1984 EDSchroth
(defun init-extended-ascii-land ()
       (setq char-input-mask 177)
       (cond (DCTL-extended-ascii		;the ctl knows about 8-bit!
	     (setq char-input-mask 377)
	     (e_pl1_$set_extended_ascii 1)
	     ;; add 8-bit self-inserts based on TTT output conversion table
	     ;; Also, define 8-bit non-printing scan table
	     (let ((convtab (*array nil 'fixnum 64.)))
		(e_pl1_$get_output_conv_table convtab)
		(do ((i 128. (1+ i))	;do 8-bit chars only
		     (next-byte-of (rot 777 -9.) (rot next-byte-of -9.)))	;successive bytes
		    ((= i 256.))		;stop after #o377
		    (or (bit-test next-byte-of (arraycall fixnum convtab (// i 4)))	;pick a byte
		        (set-perm-key (ascii i) 'self-insert)))	;if zero
		(do ((i 32. (1+ i)))	;copy entries for 200...377
		    ((= i 64.))		;to scan table
		    (store (arraycall fixnum 8bit-tabscan-table i)
			 (arraycall fixnum convtab i))))
	     (e_pl1_$set_emacs_tty_modes))))	;fix-up modes
				   
(defvar emacs-start-up-error-message)

(defun run-emacs-start-up-error (arg)
       arg
       (display-error-noabort emacs-start-up-error-message)
       (throw () emacs-start-up-tag))

(defun run-emacs-start-up-actions ()
       (setq inhibit-default-start-up-execution nil)
       (or (eq emacs-name 'emacs_)
	 (run-user-start-up (catenate (e_lap_$trim (user_info_$homedir))
				">start_up.emacs"))
	 (run-user-start-up (catenate (user-project-dir)
				">start_up.emacs"))
	 (run-user-start-up ">site>start_up.emacs"))
       (and (eq emacs-name 'emacs_) (go-to-or-create-buffer 'main))
       (or inhibit-default-start-up-execution (default-emacs-start-up))
       (cond ((eq current-buffer '|<start_up_emacs_buffer>|)
	    (go-to-or-create-buffer 'main)
	    (setq previous-buffer 'main))
	   ((eq previous-buffer '|<start_up_emacs_buffer>|)
	    (setq previous-buffer current-buffer)))
       (setq known-buflist (delq '|<start_up_emacs_buffer>| known-buflist)))

(defun user-project-dir ()
       (catenate ">user_dir_dir>"
	       (e_lap_$trim (cadr (user_info_$whoami)))))

(defun run-user-start-up (filename)
       (cond (args:ns 't)
	   ((exists-file filename 4)
	    (setq emacs-start-up-error-message "Error in start_up.emacs")
	    (catch
	      (let ((e-lisp-error-mode 'run-emacs-start-up-error))
		 (loadfile filename))
	      emacs-start-up-tag) 't)
	   ('else nil)))

;;; Re-written by GMP, 9/4/78.
;;; Re-written by RMSoley, 21 July 1981
(defun default-emacs-start-up ()
       (setq inhibit-default-start-up-execution t)
       ;; File-file the pathnames and macro files.
       (do ((paths args:paths (1- paths)))
	 ((zerop paths))
	 (let ((info (e_argument_parse_$get_one_path)))
	      (cond ((zerop (cadr info))
		   (setq emacs-start-up-error-message
		         (catenate "Can't load file " (car info)))
		   (catch
		     (let ((e-lisp-error-mode 'run-emacs-start-up-error))
			(load (e_lap_$trim (car info))))
		     emacs-start-up-tag))
		  (t
		    (catch
		      (find-file-subr (e_lap_$trim (car info)))
		      pgazonga)))))
       ;; Do -apply arguments.
       (cond ((> args:apply-arg -1)
	    (setq emacs-start-up-error-message "Can't do -apply.")
	    (catch
	      (let ((e-lisp-error-mode 'run-emacs-start-up-error))
		 (apply (make_atom (status arg args:apply-arg))
		        (multics-args-as-list (1+ args:apply-arg))))
	      emacs-start-up-tag)))
       (and tasking-restarted (full-redisplay))
       (setq tasking-restarted nil))

(defun multics-args-as-list (first-argno)
       (do ((count first-argno (1+ count))
	  (l))
	 ((not (status arg count)) (nreverse l))
	 (setq l (cons (status arg count) l))))

(setq pi-handler-minibuffer-print nil tasking-restarted nil)

(defun pi-handler ()
       (e_pl1_$set_emacs_tty_modes)
       (randomize-redisplay)
       (and DCTL-prologue-availablep (DCTL-prologue))
       (and split-mode-p (rdis-recreate-splits-on-screen))
       (reset-top-level-editor-state)
       (cond ((zerop (e_argument_parse_$new_arguments))
	    (full-redisplay))
	   (t (tasking-restart-internal)))
       (cond (pi-handler-minibuffer-print
	     (minibuffer-print pi-handler-minibuffer-print)
	     (setq pi-handler-minibuffer-print nil)))
       (std-yggdrasil))

(defun reset-top-level-editor-state ()
       (or minibufferp (instate-local-key-bindings))
       (setq suppress-redisplay-flag nil)	;restart redisplay if stopped
       (cond ((memq 'Macro/ Learn buffer-minor-modes)
	    (negate-minor-mode 'Macro/ Learn)))
       (setq damaged-flag t			;force redisplay to work on it
	   numarg nil
	   undo nil
	   macro-execution-in-progress nil
	   macro-collection-in-progress nil
	   macrostack nil)
       (or minibufferp (setq recursion-level 0)))

;;; Modified 28 June 1981 RMSoley to use set_break_sequence
;;; Modified Dec 1984 EDSchroth for 8bit ASCII.
;;;    Extended chars break echonego by default
(defun init-echnego-bittab ()
       (do ((char 0 (1+ char))
	  (number 0)
	  (nlist ())
	  (count 0 (1+ count)))
	 ((= char 256.)
	  (apply 'e_pl1_$set_break_sequence
	         (nreverse (cons number nlist))))
	 (and (not (zerop char))
	      (zerop (\ count 32.))
	      (setq nlist (cons number nlist)
		  count 0
		  number 0))
	 (setq number (lsh number 1))
	 (or (and (> char 31.) (< char 127.)
		(memq (key-bindings char 0) nobreak-functions))
	     (setq number (1+ number)))))

(defcom debug-e
        &numeric-argument (&reject)
        (*rset t) (nouuo t) (sstatus uuolinks nil)     ;et in saecula saeculorum amen
        (setq e-lisp-error-mode 'lisp-break)
        (sstatus mulpi 1)			;pi -> ^b interrupt
        (sstatus interrupt 2 '(lambda (z)(pi-handler) z)))	;CTRL/a -> reenter


(defun get-editing-characters ()
       (let ((editing-chars (e_pl1_$get_editing_chars)))
	  (setq MCS-editing-characters (mapcar 'CtoI editing-chars)
	        MCS-escape-character (car editing-chars))
	  (set-editing-key (car editing-chars) 'escape-char)
	  (set-editing-key (cadr editing-chars) 'rubout-char)
	  (set-editing-key (caddr editing-chars) 'kill-to-beginning-of-line)))


(defun set-editing-key (character function)
       (cond ((eq (get-key-binding (parse-key-description character))
	        'self-insert)
	    (set-perm-key character function))))

;;;
;;;	Following is all of Multics EMACS.
;;;

(defun std-yggdrasil ()			;Root tree of universe
       (do ()(nil)
	 (catch (charlisten) gazongue-a-l/'yggdrasil) ;ceci est jet'e
					;seulement par ^G
	 (redisplay)  ;gratuitous
           (ring-tty-bell)
	 (reset-top-level-editor-state)))


(defun charlisten ()
       (let ((recursion-level recursion-level))
	  (do nil (nil)
	      (or macro-execution-in-progress
		emacs-start-ups-need-running
		(redisplay))
	      (catch
	        (errset (let ((fail-act 'e-lisp-lossage-handler)
			  (pdl-overflow 'e-lisp-lossage-handler)
			  (wrng-type-arg 'e-lisp-lossage-handler)
			  (*rset-trap 'e-lisp-lossage-handler)
			  (unbnd-vrbl 'e-lisp-lossage-handler)
			  (undf-fnctn 'e-lisp-lossage-handler)
			  (unseen-go-tag 'e-lisp-lossage-handler)
			  (wrng-no-args 'e-lisp-lossage-handler)
			  (errset 'e-lisp-lossage-handler))

			 (cond
			   ((eq emacs-start-ups-need-running t)
			    (setq emacs-start-ups-need-running nil)
			    (run-emacs-start-up-actions))
			   (emacs-start-ups-need-running
			     (funcall
			       (prog1 emacs-start-ups-need-running
				    (setq emacs-start-ups-need-running
					nil)))))

			 (do ((numarg nil nil) (undo nil nil)) (nil)
			     (process-char (get-top-level-char))))
		      nil)
	        pgazonga)
	      (reset-top-level-editor-state))))

(defun e-lisp-lossage-handler (arg)
       (setq arg (caddr (errframe nil)))
       (cond (e-quit-transparency (errprint nil))
	   (t (minibuffer-print
	        (car arg) " " (maknam (explodec (cadr arg))))))
       (cond ((eq e-lisp-error-mode 'lisp-break)
	    (let ((e-quit-transparency 'transparent))
	         (e_pl1_$set_multics_tty_modes)
	         (terpri)(terpri)
	         (princ
		 (catenate "Lisp error in buffer " current-buffer))
	         (terpri)
	         (setq arg (eval (list 'break (caddr arg) t)))
	         (e_pl1_$set_emacs_tty_modes)
	         (full-redisplay))
	    (cond (arg)(t (command-prompt-abort))))
	   ((null e-lisp-error-mode)(command-quit))
	   (t (funcall e-lisp-error-mode arg))))


(defcom lisp-error-mode
        &arguments ((mode &symbol &prompt "Mode: "   ;&valid on off, when ready
		      &default off))	;default to "normal"
        &numeric-argument (&reject)
        (cond ((memq mode '(nil reset off 0))	;ick
	     (setq e-lisp-error-mode nil))
	    ((memq mode '(t set on 1 lisp-break))
	     (setq e-lisp-error-mode 'lisp-break))
	    (t (display-error "Unknown lisp error mode: " mode))))

;;;
;;;
;;;	Character readers
;;;

(declare (array* (notype (emacs-interrupt-handlers ?)(emacs-interrupt-handles ?))
	       (fixnum (emacs-interrupt-array ?))))


(defun get-top-level-char ()
       (get-a-char 'toplevel-char 'get-top-level-char-innards))

(defun get-char ()
       (get-a-char 'input-char 'e_pl1_$get_char))

(defun get-a-char (type get-char-function)
       (let ((new-ch
	     (cond ((and macro-execution-in-progress (kmacro-get-one-cmd type)))
	   (t (do ((ch (funcall get-char-function) (funcall get-char-function)))
		(nil)
		(or (= 0 (emacs-interrupt-array 0)) (setq delayed-interrupt t))
		(store (emacs-interrupt-array 0) 0)
		(and (not minibufferp) delayed-interrupt (emacs-interrupt-processor))
		(or (= ch -1)
		    (progn (store (saved-command-history history-next-index) ch)
			 (setq history-next-index
			       (cond ((= history-next-index 49.) 0)
				   (t (1+ history-next-index))))
			 (and macro-collection-in-progress
			      (kmacro-record-input ch type))
			 (return ch))))))))
	  (setq last-input-char (ascii (logand char-input-mask new-ch)))	;last-input-char = char without META
	  new-ch))

;;;
;;;Highly local specials for gran kludge redisplay (echo negotiation).
;;; Goddamn backpanel wires to every board in the machine.
(defvar (X howmuchigot-sym rdis-upd-locecho-flg screenlinelen touchy-damaged-flag rdis-suppress-redisplay))
(defvar (rdis-multiwindowed-buflist rdis-inhibit-echnego))
(defvar (curlinel curstuff work-string curpointpos hard-enforce-fill-column fill-column))

(defun get-top-level-char-innards ()

       (let ((ordi rdis-suppress-redisplay)
	   (chpos 0))

	  
;;; THIS NEXT STATEMENT IS PERHAPS THE MOST IMPORTANT IN MULTICS EMACS
;;; IT CAUSES REDISPLAY TO OCCUR WHEN THERE IS NO PENDING INPUT.
;;; ALMOST ALL REDISPLAY IN THE WORLD IS INVOKED RIGHT HERE.


	  (and (= 0 (e_pl1_$real_have_chars))(redisplay))


;;; Attempt PL/I (and poss. better) echo negotiation.

	  (cond ((and			;try super-opt
		 (eq curstuff work-string)	;line gotta be open
		 (= curpointpos (1- curlinel))     ;gotta be at eol
		 (not macro-collection-in-progress)
		 (not ordi)		;old rdis-suppr-rdis
		 (not suppress-redisplay-flag)
		 (not (and hard-enforce-fill-column
			 (not (< (setq chpos (cur-hpos)) fill-column))))
		 (not rdis-inhibit-echnego)
		 (prog2 (redisplay)		;update all parms
		        (not (and minibufferp (> X (- screenlinelen 10.)))))
		 (or (not (memq current-buffer rdis-multiwindowed-buflist))
		     minibufferp))		;echnego ok minibuf even so
	         (setq locechnego-ctr (1+ locechnego-ctr))
	         (prog2 (set 'howmuchigot-sym 0)
		      (e_pl1_$echo_negotiate_get_char
		        work-string
		        'howmuchigot-sym
		        (cond (hard-enforce-fill-column
			      (min (- screenlinelen X)
				 (- fill-column chpos)))
			    (minibufferp
			      (- screenlinelen X 7))
			    (t (- screenlinelen X))))
		      (cond ((> howmuchigot-sym 0)
			   (store (saved-command-history history-next-index)
				(substr work-string (1+ curpointpos) howmuchigot-sym))
			   (setq history-next-index
			         (cond ((= history-next-index 49.) 0)
				     (t (1+ history-next-index))))
			   (setq X (+ X howmuchigot-sym))
			   (setq locechnego-meter (+ locechnego-meter howmuchigot-sym))
			   (setq curpointpos (+ curpointpos howmuchigot-sym))
			   (setq curlinel (+ curlinel howmuchigot-sym))
			   (setq touchy-damaged-flag t)
			   (setq previous-command nil)     ;since we never actually execute a command
			   (let ((rdis-upd-locecho-flg t))
			        (redisplay))))))
	        (t (e_pl1_$get_char)))))

;;;
;;;
;;;	interrupt handling integrated into e_interact_ 1978.11.21 by Richard S. Lamson
;;;

;;;
;;;	how it works:
;;;
;;;		There are two types of interrupt numbers, namely 
;;;		internal and external.  Internal numbers are assigned 
;;;		sequentially from the variable next-interrupt-channel.
;;;		Internal numbers are used to index into the array 
;;;		emacs-interrupt-handlers, and are returned by 
;;;		e_pl1_$get_emacs_interrupt.  External numbers are
;;;		assigned by e_pl1_$assign_channel, and are computed
;;;		as 64*emacs_recursion_level + internal_number.  It is 
;;;		these external numbers which must be passed to 
;;;		e_pl1_$set_emacs_interrupt, and therefore it is these
;;;		which set-emacs-interrupt-handler returns.
;;;


(defun emacs-interrupt-processor ()
       (setq delayed-interrupt nil)
       (do ((int-info (e_pl1_$get_emacs_interrupt) (e_pl1_$get_emacs_interrupt)))
	 ((< (car int-info) 0) (and (= recursion-level 0)
			        (not rdis-suppress-redisplay)	; don't destroy local display
			        (redisplay)))
	 (let ((intno (car int-info)))
	      (cond ((emacs-interrupt-handlers intno)
		   (funcall (emacs-interrupt-handlers intno)
			  intno
			  (emacs-interrupt-handles intno)
			  (cadr int-info)))))))

(defvar max-emacs-interrupt-channel 64.)

(defun set-emacs-interrupt-handler (handler handle)    ; returns interrupt channel number
       (setq next-interrupt-channel (1+ next-interrupt-channel))
       (if (= next-interrupt-channel max-emacs-interrupt-channel)	;ran out of channels
	 (setq max-emacs-interrupt-channel (* 2 max-emacs-interrupt-channel))
	 (*rearray 'emacs-interrupt-handlers t max-emacs-interrupt-channel)
	 (*rearray 'emacs-interrupt-handles t max-emacs-interrupt-channel))
       (store (emacs-interrupt-handlers next-interrupt-channel) handler)
       (store (emacs-interrupt-handles next-interrupt-channel) handle)
       (e_pl1_$assign_channel next-interrupt-channel))

(defun interrupt-init ()
       (*array 'emacs-interrupt-array 'external (e_pl1_$get_emacs_interrupt_array) 2)
       (*array 'emacs-interrupt-handlers t max-emacs-interrupt-channel)
       (*array 'emacs-interrupt-handles  t max-emacs-interrupt-channel)
       (setq delayed-interrupt nil)
       (setq next-interrupt-channel -1))

;;; 

;;;
;;;	Functions to print errors/messages in the minibuffer
;;;

(defvar (suppress-minibuffer))
;;;(register-option 'suppress-minibuffer nil) ;;; moved to e_option_defaults_

;;; Print an error message.
(defun display-error-noabort n
       (or suppress-minibuffer
	 (echo-buffer-print (apply 'catenate (listify n)))))

;;; Print an error message and abort.
(defun display-error n
       (or suppress-minibuffer
	 (apply 'display-error-noabort (listify n)))
       (command-quit))

;;; Print an error message: first argument is Multics error code.
(defun display-com-error-noabort n
       (or suppress-minibuffer
	 (let ((prefix
	         (cond ((= 0 (arg 1)) "")
		     (t (catenate
			(e_lap_$rtrim
			  (cadr (convert_status_code_ (arg 1))))
			(cond ((> n 1) "  ")
			      (t ""))))))
	       (message (cond ((> n 1)
			   (apply 'catenate (listify (- 1 n))))
			  (t ""))))
	      (echo-buffer-print (catenate prefix message)))))

;;; Print an error message and abort: first argument is Multics error code.
(defun display-com-error n
       (apply 'display-com-error-noabort (listify n))
       (command-quit))

;;; Clear out the minibuffer.
(defun minibuffer-clear-all ()
       (echo-buffer-clear-all))

;;; Print a message in the minibuffer.
(defun minibuffer-print n
       (or macro-execution-in-progress suppress-minibuffer
	 (echo-buffer-print (apply 'catenate (listify n)))))

;;; Print a message in the minibuffer without clearing current contents.
(defun minibuffer-print-noclear n
       (or macro-execution-in-progress suppress-minibuffer
	 (echo-buffer-outprint (apply 'catenate (listify n)))))

;;; Delete the last N characters from the minibuffer.
(defun minibuffer-rubout (n)
       (or macro-execution-in-progress
	 (echo-buffer-rubout n)))

;;; Make a very transient remark.
(defun minibuffer-remark n
       (or macro-execution-in-progress suppress-remarks suppress-minibuffer
	 (echo-buffer-utter (apply 'catenate (listify n)))))

(defun display-error-remark n
       (or suppress-minibuffer
	 (echo-buffer-utter (apply 'catenate (listify n)))))

;;; Clear the last minibuffer statement.
(defun minibuffer-clear ()(echo-buffer-clear))

;;;
;;;	Self-documentation primitives - see e_self_documentor_.
;;;


(defun get-cmd-symbol-3args (metap key prefix)
       (cond ((and (= metap 1) prefix) nil)
	   ((not prefix)
	    (cond ((subrp (key-bindings key metap)) nil)
		(t (key-bindings key metap))))
	   (t (cond ((not (subrp (key-bindings prefix 0))) nil)
		  (t (arraycall t (key-bindings prefix 0) key))))))


;;; Get the function bound to a key
(defun get-key-binding (key-list)
       (apply 'get-cmd-symbol-3args key-list))


;;; Read the name of key
(defun key-prompt (prompt)
       (prog (ch1)
	   (minibuffer-print prompt)
	   (setq ch1 (get-char))
	   (return (cond ((= ch1 377)
		        (setq ch1 (get-char))
		        (cond ((= ch1 377)
			     (minibuffer-print-noclear "esc-" (printable 177))
			     '(1 177 nil))
			    (t (return (telnet-loser ch1)))))
		       ((> ch1 char-input-mask)
		        (minibuffer-print-noclear "esc-")
		        (key-prompt-1 1 (bit-clear 200 ch1) nil))
		       (t (key-prompt-1 0 ch1 nil))))))

(defun key-prompt-1 (metap key prefix)
       (prog (mf1)
	   (and (or prefix (= metap 1))
	        (< key (1+ (CtoI "z")))(> key (1- (CtoI "a")))
	        (setq key (- key 40)))
	   (setq mf1 (cond (prefix (arraycall t (key-bindings prefix 0) key))
		         (t (key-bindings key metap))))
	   (cond ((eq mf1 'escape)
		(minibuffer-print-noclear "esc-")
		(return (key-prompt-1 1 (get-char) nil)))
	         ((not (symbolp mf1))
		(minibuffer-print-noclear (printable key)
				      " (prefix char): ")
		(return (key-prompt-1 0 (get-char) key)))
	         (t (minibuffer-print-noclear (printable key))
		  (return (list metap key prefix))))))


;;; Compatability
(defun key-prompt-3args ()
       (key-prompt "?: "))


;;; Execute supplied function on all keys defined in current buffer
(defun map-over-emacs-commands (fun arg)
       (do i 0 (1+ i) (= i 256.)		;i hated fortran as a child
	 (do j 0 (1+ j) (= j 2)		;and i hate it now as a programmer.
	     (let ((element (key-bindings i j)))
		(cond ((not (symbolp element))
		       (do k 0 (1+ k)(= k 256.)
			 (or (not (arraycall t element k))
			     (eq (arraycall t element k) 'undefined-command)
			     (funcall fun (key-total-printed-symbol 0 k i)
				    (arraycall t element k) arg))))
		      ((eq element 'undefined-command))
		      (element
		        (funcall fun (key-total-printed-symbol j i nil) element arg)))))))


;;;

;;;
;;;	ESC Processing and Numeric Argument Readers
;;;


;;; Command to quit to editor top level
(defcom command-quit
        &numeric-argument (&ignore)
        &undo &ignore
        (ring-tty-bell)
        (throw 'les-petites-gazongues pgazonga))


;;; Command to "ignore" a prefix character, by default on prefix-^G
(defcom ignore-prefix
        &undo &ignore
        &numeric-argument (&ignore)
        (ring-tty-bell))

;;; Command to throw to top level or nearest yggdrasil (ldebug, multics mode
;;; are the only others beside top level)
(defcom command-prompt-abort
        &numeric-argument (&ignore)
        &undo &ignore
        (throw nil gazongue-a-l/'yggdrasil))

;;; Command bound to ESC key
(defcom escape
        &undo-function &pass
        &numeric-argument (&pass)
        (and (eq minibufferp ESC) (jetteur-des-gazongues))
        (escape-dont-exit-minibuf))

(defprop throw-to-toplevel jetteur-des-gazongues expr)

(defcom-synonym escape-dont-exit-minibuffer escape-dont-exit-minibuf)

;;; Set the undo switch.
(defcom undo-prefix
        &numeric-argument &pass
        &undo &pass
        (setq undo (not undo))
        (process-char (get-char)))

;;; Command that does real work of ESC
(defcom escape-dont-exit-minibuf
        &numeric-argument (&pass)
        &undo &pass
        (prog (nxcn numf negate)
a 	    (setq nxcn (get-char))
	    (cond ((and (> nxcn (1- (CtoI "0"))) (< nxcn (1+ (CtoI "9"))))	;number
		 (or numarg (setq numarg 0))
		 (setq numarg (+ (- nxcn (CtoI "0")) (* 10. numarg)))
		 (setq numf t)
		 (go a))
		((and (not numf) (= nxcn (CtoI "-")))	;want negative argument
		 (setq negate t numf t) (go a))
		((and (not numf) (= nxcn (CtoI "+")))	;want positive argument
		 (setq numf t) (go a))
		(t (and numf negate		;negative argument (default -1)
		        (setq numarg (- (or numarg 1))))
		   (cond (numf (process-char nxcn))
		         (t (execute-key 1 nxcn nil)))))))


;;; Command to collect numeric argument or use powers of 4
(defcom multiplier
        &undo &pass
        &numeric-argument (&pass)
        (prog (nxcn numf multf negate plus-given my-char)
	    (setq my-char last-input-char)	;character used to invoke this
a 	    (setq nxcn (get-char))
	    (cond ((and (> nxcn (1- (CtoI "0"))) (< nxcn (1+ (CtoI "9"))))	;number
		 (or numf (setq numf 0))
		 (setq numf (+ (- nxcn (CtoI "0"))(* 10. numf)))
		 (go a))
		((and (not numf) (= nxcn (CtoI "-")))	;negative argument
		 (setq numf 0 negate t) (go a))
		((and (not numf) (= nxcn (CtoI "+")))	;positive argument
		 (setq numf 0 plus-given t) (go a))
		((and (< nxcn 200) (eq (ascii nxcn) my-char))  ;NOTE- this code is buggy
		 (cond ((and (not numf) (not multf))
		        (setq multf 4))
		       ((not numf) (setq multf (* multf 4)))
		       (numf (setq numf nil))
		       (t (setq multf nil numf nil)))
		 (go a))
		(t (and (or negate plus-given) (= numf 0)
		        (setq numf 1))	;default number if only + given
		   (and negate (setq numf (- numf)))	;negate number (with -1 as default)
		   (setq numarg (cond ((and numf multf) (* numf multf))
				  (numf)
				  (multf (* 4 multf))
				  (t 4)))
		   (process-char nxcn)))))


;;; Read a "metazied" number (from Network mostly)
(defcom read-meta-argument
        &undo &pass
        &numeric-argument (&ignore)
        (prog (negate nxcn plus-given)
	    (setq nxcn (CtoI last-input-char))	;get charater invoked by (without meta-bit)
	    (setq numarg 0)
	    (cond ((= nxcn (CtoI "+")) (setq plus-given t))
		((= nxcn (CtoI "-")) (setq negate t))
		(t			;assume a digit
		  (setq numarg (- nxcn (CtoI "0")))))
a 	    (setq nxcn (get-char))
	    (cond ((and (> nxcn (1- (+ 200 (CtoI "0")))) (< nxcn (1+ (+ 200 (CtoI "9")))))
		 (setq numarg (+ (- nxcn (+ 200 (CtoI "0"))) (* 10. numarg)))
		 (go a))
		(t			;have character to execute
		  (and (= numarg 0) (or negate plus-given)
		       (setq numarg 1))	;a sign given, set default
		  (and negate (setq numarg (- numarg)))
		  (process-char nxcn)))))

;;; 

;;;
;;;	Character/Key/Command Execution
;;;

;;; Process a character: determine if it is a "meta" character and then
;;;  execute the key corresponding to the character
(defun process-char (ch)
       (or (fixp ch)
	 (setq ch (CtoI ch)))
       (let ((recursion-level (1+ recursion-level)))
	  (cond ((and (not (zerop network-flag))
		    (= ch 377))		;TELNET IAC
	         (setq ch (get-char))
	         (cond ((= ch 377)
		      (execute-key 1 177 nil))
		     (t (telnet-loser ch))))
	        ((> ch char-input-mask)	;meta-foo
	         (setq ch (logand char-input-mask ch))	;non-meta foo
	         (execute-key 1 ch nil))
	        (t (execute-key 0 ch nil)))))


;;; Execute a "key" as an Emacs command:  A "key" is the triplet consisting
;;;  of a character, "meta"-bit, and prefix character used to determine the
;;;  exact command to be executed.
(defun execute-key (metap ch prefix)
       (let ((command))			;the command to execute
	  (and (or (= metap 1) prefix)
	       (and (< ch (1+ (CtoI "z")))
		  (> ch (1- (CtoI "a")))
		  (setq ch (- ch 40))))
	  (cond ((not prefix) (setq command (key-bindings ch metap)))
	        (t (setq command (arraycall t (key-bindings prefix 0) ch))))
	  (cond ((symbolp command)		;normal command
	         (setq last-command-triplet-mpfxk (cond ((= metap 1) 'meta)
					        (t prefix))
		     last-command-triplet-1 ch)
	         (execute-command command (last-command-triplet) nil))
	        (t			;a prefix character
		(execute-key 0 (get-char) ch)))))

(defvar (autoload-inform))
;;;(register-option 'autoload-inform nil) ;;; moved to e_option_defaults_

;;; Ensure that autoloads are done early in the execution phase.
(defun ensure-autoload (command)
       (cond ((getl command '(editor-macro subr expr)))
	   ((not (get command 'autoload)))
	   ('else
	     (if autoload-inform
	         (minibuffer-print "Autoloading " command " ... "))
	     (protect (loadfile (get command 'autoload))
		    &success
		    (if autoload-inform
		        (minibuffer-print-noclear "done."))
		    &failure
		    (if autoload-inform
		        (minibuffer-print-noclear "failed."))))))

(setq last-time-sample nil)

;;; Execute an Emacs command
(defun execute-command (command key argument-list)
       (ensure-autoload command)
       (setq current-command command)
       (or last-time-sample (setq last-time-sample (time)))
       (let  ((last-time-sample 'dont-sample))
	   (cond ((get command 'editor-macro)	;keyboard macro
		(or (null argument-list)
		    (display-error (ed-get-name command key)
			         " does not accept arguments."))
		(push-editor-macro-level (get command 'editor-macro)
				     (editor-macro-arg-interp numarg))
		(setq previous-command command
		      previous-argument-list nil))
	         ((get command 'editor-command)	;new-style command
		(execute-new-command command key argument-list))
	         (t		;old-style command
		 (or (null argument-list)
		     (display-error (ed-get-name command key)
				" does not accept arguments."))
		 (execute-old-command command (last-command-triplet)))))
       (and (or command-bell meter-commands) ;Avoid call if we can.
	  (command-timing last-time-sample))
       (setq numarg nil undo nil last-time-sample nil))

;;; Handle command timing.
;;; nil=> no bell.  otherwise threshhold in seconds
;;;(register-option 'command-bell nil) ;;; moved to e_option_defaults_
;;; nil=> no bell.  fixnum=>number of bells.  otherwise function to call.
;;;(register-option 'command-bell-count nil) ;;; moved to e_option_defaults_
;;; nil=> no metering. t=> minibuffer metering. otherwise function.
;;;(register-option 'meter-commands nil) ;;; moved to e_option_defaults_

;;; Moved to e_option_defaults
;;;(defprop command-bell t value-ok-anything)
;;;(defprop command-bell-count t value-ok-anything)
;;;(defprop meter-commands t value-ok-anything)

(defun command-timing (sample)
       (or (null sample) (not (floatp sample))
	 (let ((difference (-$ (time) sample)))
	      (and command-bell (> difference (float command-bell))
		 (cond ((fixp command-bell-count)
		        (do-times command-bell-count (ring-tty-bell)))
		       (command-bell-count
		         (funcall command-bell-count difference))))
	      (cond ((eq meter-commands 't)
		   (minibuffer-print (decimal-rep difference) "s"))
		  (meter-commands
		    (funcall meter-commands difference))))))

;;; Returns command name for error messages
(defun ed-get-name (command key)
       (catenate command
	       (cond ((get command 'editor-macro) " (keyboard macro)")
		   (t ""))
	       (cond (key
		     (catenate " (" (get-key-name key) ")"))
		   (t ""))))


;;; Try to convert an argument to a fixnum and return nil if not valid
(defun ed-cv-fixnum-check (argument)
       (let ((argument-list (exploden argument)))
	  (do ((digit (car argument-list) (car argument-list))
	       (negate)
	       (value))
	      ((not digit)
	       (and negate (setq value (- value)))
	       value)
	      (cond ((and (= digit #/+) (not value)) ;+ as first char
		   (setq value 0))
		  ((and (= digit #/-) (not value))
		   (setq value 0 negate t))
		  ((and (> digit (1- #/0)) (< digit (1+ #/9)))
		   (setq value (+ (- digit #/0) (* 10. (or value 0)))))
		  (t			;not valid in a number
		    (return nil)))
	      (setq argument-list (cdr argument-list)))))

;;; 

(setq *transparent-commands*  '(escape multiplier noop re-execute-command
			 extended-command))

;;; Invoke a new-style Emacs command
;;; JSL's new version - June 1982
(defun execute-new-command (command key argument-list)
       (do ((done)
	  (flags (get command 'editor-command))
	  (function command)
	  (ignore-rejected-numarg)
	  (prologue-info)
	  (result)
	  (times))
	 (done result)
	 ;;
	 ;; Check for synonym command.
	 ;;
	 (and (symbolp flags)
	      (return (execute-command flags key argument-list)))
	 ;;
	 ;; Check for undo.
	 ;;
	 (if undo
	     (and (tlnn flags 000500)
		(setq undo nil))
	     (and (tlnn flags 000400)
		(return (execute-command (get command 'ed-undo-function)
				     key argument-list)))
	     (and (tlne flags 000700)
		(display-error (ed-get-name command key)
			     " does not accept the undo prefix.")))
	 ;;
	 ;; Here to process numeric arguments.
	 ;;
	 (if numarg
	     ;;
	     ;; Check for &numeric-function.
	     ;;
	     (if (tlnn flags 001000)
	         (setq function (get function 'ed-numeric-function))
	         (ensure-autoload function)
	         (or (and function (getl function '(subr lsubr fsubr
					    expr lexpr fexpr)))
		   (display-error (ed-get-name command key)
			        " does not accept a numeric argument."))
	         (setq flags (or (get function 'editor-command) 0)
		     ignore-rejected-numarg t))
	     ;;
	     ;; Check for &negative function.
	     ;;
	     (if (and (< numarg 0) (tlnn flags 200000))
	         (setq function (get function 'ed-negative-function))
	         (ensure-autoload function)
	         (or (and function (getl function '(subr lsubr fsubr
					    expr lexpr fexpr)))
		   (display-error (ed-get-name command key) " does not "
			        "accept a negative numeric argument."))
	         (setq flags (or (get function 'editor-command) 0)
		     numarg (- numarg)
		     ignore-rejected-numarg t))
	     ;;
	     ;; Now process &repeat, &reject, &ignore and check bounds.
	     ;;
	     (let ((numarg-type (logand flags (lsh 070000 18.)))
		 (numarg-range (and (tlnn flags 100000)
				(get function 'ed-numeric-range))))
		(setq times (ed-interpret-numarg command key numarg-type
					   numarg-range
					   ignore-rejected-numarg))))
	 ;;
	 ;; Simple case.
	 ;;
	 (if (and (null argument-list)
		(tlne flags 406040))	; Has no special handling needed.
	     ;;
	     ;; Deal with numeric argument, if any.
	     ;;
	     (cond (times (setq numarg nil))
		 (t (setq times 1)))
	     ;;
	     ;; Call the function, and return its result.
	     ;;
	     (return
	       (cond ((eq (cadr function) 'subr)
		    (do ((i 1 (1+ i))
		         (f (caddr function))
		         (inv (or (memq command *transparent-commands*)
			        (memq command nobreak-functions))))
		        ((> i times) result)
		        (setq result (subrcall t f))
		        (or inv
			  (setq previous-command command
			        previous-argument-list nil))))
		   (t (do ((i 1 (1+ i))
			 (inv (or (memq command *transparent-commands*)
				(memq command nobreak-functions))))
			((> i times) result)
			(setq result (funcall function))
			(or inv
			    (setq previous-command command
				previous-argument-list nil)))))))
	 ;;
	 ;; Prepare for cleanup handler, in case specified.
	 ;;
	 (unwind-protect
	   (progn
	     ;;
	     ;; Do prologue if specified.
	     ;;
	     (and (tlnn flags 004000)	;has prologue code.
		(setq prologue-info
		      (funcall (get function 'ed-prologue-function))))
	     ;;
	     ;; Process arguments.
	     ;;
	     (and (or (tlnn flags 400000)	;wants arguments
		    (not (null argument-list)))
		(setq argument-list
		      (ed-interpret-arguments command key function flags
					argument-list)))
	     ;;
	     ;; Clear numarg for &repeat case.
	     ;;
	     (cond (times (setq numarg nil))
		 (t (setq times 1)))
	     ;;
	     ;; Do the command as many times as necessary, calling the
	     ;; prologue after each invocation, if there is one.
	     ;;
	     (do ((epilogue (and (tlnn flags 002000)
			     (get function 'ed-epilogue-function)))
		(i 1 (1+ i))
		(inv (or (memq command *transparent-commands*)
		         (memq command nobreak-functions))))
	         ((> i times))
	         (setq result (apply function argument-list))
	         (and epilogue
		    (setq result (funcall epilogue prologue-info
				      result (= i times))))
	         (or inv
		   (setq previous-command command
		         previous-argument-list argument-list)))
	     ;;
	     ;; We won't need cleanup handler anymore.
	     ;;
	     (setq done (> times 0)))
	   ;;
	   ;; Here we check for cleanup handler.
	   ;;
	   (and (not done) (setq done t)
	        (tlnn flags 000040)
	        (setq flags (get function 'ed-cleanup-function))
	        (funcall flags prologue-info)))))


;;; Interpret the numeric argument
;;; JSL's new version - June 1982
(defun ed-interpret-numarg (command key numarg-type numarg-range
		        ignore-rejected-numarg)
       (and numarg-range		;a range is specified
	  (let ((lower (car numarg-range))
	        (upper (cdr numarg-range)))
	       (cond (lower		;lower bound specified
		     (setq lower (ed-get-encoded-value lower))
		     (and (< numarg lower)  ;lose, lose
			(display-error
			  (ed-get-name command key)
			  " does not accept a "
			  (cond ((= lower 0)	;a special case
			         "negative numeric argument.")
			        (t (catenate
				   "numeric argument < "
				   (decimal-rep lower)
				   "; you supplied "
				   (decimal-rep numarg) ".")))))))
	       (cond (upper		;upper bound specified
		     (setq upper (ed-get-encoded-value upper))
		     (and (> numarg upper)  ;lose, lose
			(display-error
			  (ed-get-name command key)
			  " does not accept a "
			  (cond ((= upper -1)	;a special case
			         "positive numeric argument.")
			        (t (catenate
				   "numeric argument > "
				   (decimal-rep upper)
				   "; you supplied "
				   (decimal-rep numarg) ".")))))))))
       (cond ((zerop numarg-type)		; Pass numeric argument.
	    nil)
	   ((= numarg-type (lsh 010000 18.))	; Repeat numeric argument.
	    numarg)
	   ((= numarg-type (lsh 020000 18.))	; Ignore numeric argument.
	    (setq numarg nil))
	   ;;
	   ;; If we get here, numarg-type = (lsh 030000 18.) Reject.
	   ;;
	   (ignore-rejected-numarg
	     (setq numarg nil))
	   (t (display-error (ed-get-name command key)
			 " does not accept a numeric argument."))))


;;; Interpret and complete the command's argument list
;;; Slightly modified by JSL summer '82
(defun ed-interpret-arguments (command key function flags argument-list)
       (let ((nargs-given (length argument-list))
	   (nargs-wanted (logand flags 777777))
	   (args-template (get function 'ed-argument-list)))
	  (and (= nargs-wanted 0)		;no arguments allowed
	       (> nargs-given 0)		;but some were supplied
	       (display-error (ed-get-name command key)
			  " does not accept arguments."))
	  (do ((i 1 (1+ i))			;go through the arguments
	       (args-wanted args-template (cdr args-wanted))
	       (args-given argument-list (cdr args-given))
	       (new-arguments))
	      ((> i nargs-wanted)		;until all args processed
	       (nreverse new-arguments))	;'twas built in reverse
	      (setq new-arguments (cons
			        (ed-interpret-single-arg
				command key nargs-wanted nargs-given i
				(car args-wanted)
				(car args-given)
				(= i nargs-wanted) (cdr args-given))
			        new-arguments)))))


;;; Interpretation of a single argument
(defun ed-interpret-single-arg (command key nargs-wanted nargs-given
			  arg-no arg-template arg-supplied
			  last-argp rest-of-args-supplied)
       (let ((data-type		;data type of argument
	     (logand (car arg-template) (lsh 700000 18.)))
	   (have-prompt		;non-zero => prompt if missing
	     (tlnn (car arg-template) 040000))
	   (have-default		;non-zero => default value exists
	     (tlnn (car arg-template) 020000))
	   (have-restrictions	;non-zero => value is restricted
	     (tlnn (car arg-template) 010000))
	   (prompt-info (cadr arg-template))
	   (default-info (caddr arg-template))
	   (restriction-info (cadddr arg-template))
	   (show-error (cond ((tlnn (car arg-template) 040000)
			  ;;can prompt for new value
			  'display-error-noabort)
			 (t 'display-error)))
	   (completion-list (eval (car (cddddr arg-template)))))
	  (do ((the-argument arg-supplied)	;start with what's given
	       (have-argument))
	      (have-argument the-argument)	;return constructed arg
	      (cond
	        ((or (= data-type (lsh 300000 18.))	;&rest-as-string
		   (= data-type (lsh 400000 18.)))	;&rest-as-list
	         (or last-argp
		   (display-error "Argument #" (decimal-rep arg-no)
			        " of " (ed-get-name command key)
			        " is a rest-of-arguments type, but "
			        "is not the last argument."))
	         (setq have-argument t	;this will succeed
		     the-argument (cond
				((= data-type (lsh 300000 18.))
				 ;;wants a string
				 (catenate
				   (or arg-supplied "")
				   (do ((args
					rest-of-args-supplied
					(cdr args))
				        (x "" (catenate
					      x " " (car args))))
				       ((null args) x))))
				(t	;wants a list
				  (append (and arg-supplied
					     (list arg-supplied))
					rest-of-args-supplied)))))
	        ((and last-argp rest-of-args-supplied)
	         (display-error (ed-get-name command key) " expects "
			    (decimal-rep nargs-wanted) " arguments;"
			    " you supplied " (decimal-rep nargs-given)
			    "."))
	        (the-argument
		;;something here, check it for legality
		(cond ((zerop data-type)	;string argument, no checking
		       (setq have-argument t))
		      ((= data-type (lsh 100000 18.))    ;wants a symbol
		       (let ((x (ed-interpret-symbol-arg
			        command key arg-no the-argument
			        show-error have-restrictions
			        restriction-info)))
			  (setq the-argument (car x)
			        have-argument (cdr x))))
		      ((= data-type (lsh 200000 18.))
		       ;;wants an integer
		       (let ((x (ed-interpret-integer-arg
			        command key arg-no the-argument
			        show-error have-restrictions
			        restriction-info)))
			  (setq the-argument (car x)
			        have-argument (cdr x))))
		      (t		;unknown data type
		        (display-error "Argument #" (decimal-rep arg-no)
				   " of " (ed-get-name command key)
				   " has an unknown data type."))))
	        (t			;prompt or use default
		(cond (have-prompt	;prompt for it
		        (setq the-argument (minibuf-response
				         (ed-get-encoded-value
					 (car prompt-info))
				         (cdr prompt-info)))
		        (and have-default ;if there's a default
			   (nullstringp the-argument)  ;no value given
			   (setq the-argument (ed-get-encoded-value
					    default-info))))
		      (have-default	;have default value
		        (setq the-argument (ed-get-encoded-value
				         default-info)))
		      (t		;no prompt, no default
		        (display-error "Argument #" (decimal-rep arg-no)
				   " of " (ed-get-name command key)
				   " has no prompt or default value.")
		        )))))))


;;;
;;; Interpretation of an argument which should be a symbol
;;;

(defun ed-interpret-symbol-arg (command key arg-no the-argument show-error
				have-restrictions restriction-info)
       (let ((argument (intern (make_atom (e_lap_$trim the-argument))))
	   (have-argument nil))		;not found yet
	  (cond (have-restrictions		;but it's value is limited
		(let ((possible-values (ed-get-encoded-value
				     restriction-info)))
		     (cond ((memq the-argument possible-values)
			  (setq have-argument t))
			 (t		;not good
			   (funcall show-error
				  "Argument # " (decimal-rep arg-no)
				  " of " (ed-get-name command key)
				  " must be one of:"
				  (do ((values possible-values
					     (cdr possible-values))
				       (x "" (catenate x " "
						   (car values))))
				      ((null values) x)))
			   (setq argument nil)))))	;force prompt
	        (t			;value not restricted, got it
		(setq have-argument t)))
	  (cons argument have-argument)))

;;;
;;; Interpretation of an argument which should be an integer
;;;

(defun ed-interpret-integer-arg (command key arg-no the-argument show-error
				 have-restrictions restriction-info)
       (let ((value (cond ((fixp the-argument) the-argument)
		      (t (ed-cv-fixnum-check the-argument))))
	   (have-argument))			;none yet
	  (cond (value			;got something
		(cond (have-restrictions	;but restricted
		        (let ((lower (car restriction-info))
			    (upper (cdr restriction-info)))
			   (cond (lower	;has lower bound
				 (setq lower (ed-get-encoded-value
					     lower))
				 (cond ((< value lower)
				        (cond ((= lower 0)
					     (funcall
					       show-error
					       "Argument #"
					       (decimal-rep arg-no)
					       " of "
					       (ed-get-name
					         command key)
					       " must not be "
					       "negative."))
					    (t
					      (funcall
					        show-error
					        "Argument #"
					        (decimal-rep arg-no)
					        " of "
					        (ed-get-name
						command key)
					        " must be >= "
					        (decimal-rep lower)
					        "; you supplied "
					        (decimal-rep value)
					        ".")))
				        (setq value nil)))))
					;;force prompt
			   (cond (upper	;has upper bound
				 (setq upper (ed-get-encoded-value
					     upper))
				 (cond ((> value upper)
				        (cond ((= upper -1)
					     (funcall
					       show-error
					       "Argument #"
					       (decimal-rep arg-no)
					       " of "
					       (ed-get-name
					         command key)
					       " must not be "
					       "positive."))
					    (t (funcall
					         show-error
					         "Argument #"
					         (decimal-rep arg-no)
					         " of "
					         (ed-get-name
						 command key)
					         " must be <= "
					         (decimal-rep upper)
					         "; you supplied "
					         (decimal-rep value)
					         ".")))
				        (setq value nil))))))
					;force prompt
		        (and value		;passed the tests
			   (setq have-argument t)))
		      (t			;unrestricted, got it
		        (setq have-argument t))))
	        (t			;not a number
		(funcall show-error
		         "Argument #" (decimal-rep arg-no) " of "
		         (ed-get-name command key)
		         " must be an integer, not " the-argument ".")
		(setq have-argument nil)))	;force prompt
	  (cons value have-argument)))

;;;
;;; Evaluate an encoded value.
;;;

(defun ed-get-encoded-value (encoded-value)
        (let ((type (car encoded-value))
	    (value (cadr encoded-value)))
	   (cond ((eq type 'quote) value)	;actual value is here
	         ((eq type 'eval) (funcall value))   ;value from function
	         (t			;unknown
		 (display-error "Unknown value encoding: " type)))))

;;; 

;;; Execute an old style Emacs command
;;; Slightly modified by JSL (mostly format) - June 1982
(defun execute-old-command (command key)
       (let ((function command)
	   (numarg-repeat))
	  (setq numarg-repeat (get command 'argwants))
	  (and (< (or numarg 1) 0)
	       numarg-repeat
	       (setq numarg (- numarg)
		   function (or (get command 'negative-arg-function)
			      'bad-negative-argument)))
	  (or (eq (cadr function) 'subr)
	      (get function 'subr)
	      (get function 'expr)
	      (get function 'autoload)
	      (display-error "Undefined function " function " for "
			 command " (" (get-key-name key) ")"))
	  (setq numarg-repeat (cond (numarg-repeat (or numarg 1))
			        (t 1)))
	  (cond ((eq (cadr function) 'subr)
	         (do ((i 1 (1+ i))
		    (f (caddr function)))
		   ((> i numarg-repeat))
		   (subrcall t f)
		   (setq previous-command command
		         previous-argument-list nil)))
	        (t (do ((i 1 (1+ i)))
		     ((> i numarg-repeat))
		     (funcall function)
		     (setq previous-command command
			 previous-argument-list nil))))))


;;; Execute an actual command the specified number of times
;;; with the given arguments.
(defun execute-command-function (command function ntimes argument-list)
       (cond ((and (eq (cadr function) 'subr) (< (length argument-list) 5))
	    (do ((i 1 (1+ i))
	         (f (caddr function))
	         (nargs (length argument-list)))
	        ((> i ntimes))
	        (cond ((= nargs 0)
		     (subrcall t f))
		    ((= nargs 1)
		     (subrcall t f (car argument-list)))
		    ((= nargs 2)
		     (subrcall t f (car argument-list)
			     (cadr argument-list)))
		    ((= nargs 3)
		     (subrcall t f (car argument-list)
			     (cadr argument-list) (caddr argument-list)))
		    ((= nargs 4)
		     (subrcall t f (car argument-list)
			     (cadr argument-list) (caddr argument-list)
			     (car (cdddr argument-list)))))
	        (or (memq command '(escape multiplier noop
			        re-execute-command extended-command))
		  (setq previous-command command
		        previous-argument-list argument-list))))
	   (t (do i 1 (1+ i) (> i ntimes) (apply function argument-list)
		(or (memq command '(escape multiplier noop
				re-execute-command extended-command))
		    (setq previous-command command
			previous-argument-list argument-list))))))


;;; Emacs command to re-execute the last command
(defcom re-execute-command
        &undo &pass
        &numeric-argument (&pass)
        (or previous-command
	  (display-error "No saved previous command"))
        (execute-command previous-command  nil previous-argument-list))


;;; Emacs command invoked for an unbound key
(defcom undefined-command
        &numeric-argument (&ignore)
        &undo &ignore
        (display-error "Unknown command: " (get-key-name (last-command-triplet))))


;;; Emacs command invoked for a key whose command doesn't accept negative arguments
(defcom bad-negative-argument
        &undo &ignore
        &numeric-argument (&ignore)
        (display-error "Command rejected negative argument: " (get-key-name (last-command-triplet))))


;;; Function to return the last key typed by the user
(defun last-command-triplet ()
       (cond ((eq last-command-triplet-mpfxk 'meta)
	    (list 1 last-command-triplet-1 nil))
	   (t (list 0 last-command-triplet-1 last-command-triplet-mpfxk))))

;;; 

;;;
;;;	ESC-X Command
;;;	 New version: 27 August 1979 by GMP
;;;

;;; Invoke an Emacs command with arguments as read from mini-buffer
(defcom extended-command
        &arguments ((command-line &prompt "Command: "
			    &completions Fundamental/.ext-commands))
        &numeric-argument (&pass)
        &undo &pass
        (let ((command-list (parse-command-line command-line)))  ;split into pieces
	   (if (not (null command-list))
	       (let ((command-name (car command-list))
		   (arguments (cdr command-list)))
		  (or (nullstringp command-name)   ;if nothing there
		      (let ((command (intern (make_atom command-name))))
			 (ensure-autoload command)
			 (cond ((getl command '(editor-command editor-macro))
			        (execute-command command nil arguments))
			       (t (execute-old-extended-command command arguments)))))))))


;;; Parse a line into tokens, obeying the Multics quoting convention
(defun parse-command-line (line)
       (do ((input (exploden line))
	  (answer nil))
	 (nil)
	 (setq input
	       (do ((input1 input (cdr input1)))
		 ((or (null input1)
		      (not (member (car input1) '(#^I #^J #/ ))))
		  input1)))
	 (cond ((null input)
	        (return (nreverse answer)))
	       (t
	         (setq answer
		     (cons
		       (do ((result ""))
			 ((or (null input)
			      (member (car input) '(#^I #^J #/ )))
			  result)
			 (setq result
			       (catenate result
				       (cond
				         ((= (car input) #/")
					(do ((input1 (cdr input) (cdr input1))
					     (quoted t)
					     (piece ""))
					    ((not quoted)
					     (setq input input1)
					     piece)
					    (cond
					      ((null input1)
					       (display-error "Unbalanced quotes."))
					      ((and (= (car input1) #/")
						  (equal (cadr input1) #/"))
					       (setq input1 (cdr input1)
						   piece (catenate piece """")))
					      ((= (car input1) #/")
					       (setq quoted nil))
					      (t
					        (setq piece (catenate piece
								(ItoC (car input1))))))))
				         (t
					 (do ((input1 (cdr input) (cdr input1))
					      (piece (ItoC (car input))
						   (catenate piece (ItoC (car input1)))))
					     ((or (null input1)
						(member (car input1) '(#^I #^J #/  #/")))
					      (setq input input1)
					      piece)))))))
		       answer))))))


;;; Invoke an old-style extended command (no prompting, etc.)
(defun execute-old-extended-command (command arguments)
       (or (getl command '(expr subr lsubr autoload))
	 (display-error "Unknown command: " command))
       (ensure-autoload command)
       (let ((argsprop (args command))
	   (nargs (length arguments)))
	  (cond ((null argsprop) nil)		;unknown number wanted
	        ((and (not (< nargs (or (car argsprop)
				  (cdr argsprop))))
		    (not (> nargs (cdr argsprop))))
	         nil)			;correct number supplied
	        (t
		(display-error "Wrong number of arguments to extended command " command "."))))
       (apply command			;execute command
	    (do ((args arguments (cdr args))	;intern/convert all arguments
	         (new-arg-list nil
			   (cons (let ((argument (car args))
				     (value))
				    (setq value (ed-cv-fixnum-check argument))
				    (cond (value value)
					(t (intern (make_atom argument)))))
			         new-arg-list)))
	        ((null args) (nreverse new-arg-list)))))

;;; 

;;;
;;;	O boy hairy "macro" feature.
;;;	Appreciations to Dan Weinreb's "EINE" E.L.E.,
;;;	The state of the art advances now with my cursor.

;;;	Redone pretty much wholesale 2/11/79 to allow "input chars".
;;;	Have a good time in California, DLW, thanks for everything, -bsg.

;;; When a macro is being executed, this is called to supply input from the
;;; executing macro.
(defun kmacro-get-one-cmd (expected-type)
       (let ((this (car macro-execution-in-progress))
	   (rest (cdr macro-execution-in-progress)))
	  (cond ((and (numberp this)(eq expected-type 'input-char))
	         (setq macro-execution-in-progress rest)
	         this)
	        ((eq expected-type 'toplevel-char)
	         (cond ((eq this 'macend)
		      (execute-single-editor-enmacroed-command 'macend)
		      (cond (macro-execution-in-progress
			    (kmacro-get-one-cmd expected-type))
			  (t nil)))
		     ((atom this)
		      (display-error "Keyboard macro lost synchrony."))
		     ((eq (car this) 'toplevel-char)
		      (setq macro-execution-in-progress rest)
		      (cdr this))
		     (t nil)))
	        ((eq expected-type 'input-char)
	         ;;^U ^F, the ^F is like this in "articifially generated"
	         ;;macros.  char will get this, i.e., nothing at all,
	         ;;and go to the tty for input
	         (setq macro-execution-in-progress rest) ;1/29/80 fix bsg
	         (cdr this)))))
		
       

;;; When a macro is being recorded, this is called to record a single input
;;; character. Toplevelness is stored for ease in displaying definition.
;;;  (An idea by R. M. Stallman)

(defun kmacro-record-input (ch type)
       (setq macro-collection-in-progress
	   (cons (cond ((eq type 'toplevel-char)
		      (cons 'toplevel-char ch))
		     (t ch))
	         macro-collection-in-progress)))

;;; The commands to start and stop collecting macroes (macros?, macreaux?)

(defcom begin-macro-collection
        &numeric-argument (&reject)
        (cond (macro-collection-in-progress
	      (display-error "Macro already in collection."))
	    (minibufferp			;aaah, mustatio patris
	      (command-quit))
	    (t (assert-minor-mode 'Macro/ Learn)
	       (setq macro-collection-in-progress (list nil)))))


(defcom end-macro-collection
        &numeric-argument (&pass)
        (wrap-up-macro-definition)
        (and numarg (execute-last-editor-macro)))


(defun editor-macro-arg-interp (arg)
       (cond ((not arg) 1)			;once
	   ((= arg 0) 'query)
	   ((< arg 0) 'forever)
	   ((> arg 9999.) 'forever)
	   (t arg)))


(defun push-editor-macro-level (mac ntimes)
       (and (> (length macrostack) 20.)
	  (display-error "Too much macro recursion."))
       (and macrostack (rplaca (cdr (car macrostack)) macro-execution-in-progress))
       (setq macrostack (cons (list mac mac ntimes) macrostack))
       (setq macro-execution-in-progress (cadr (car macrostack))))


(defun wrap-up-macro-definition ()
       (or macro-collection-in-progress (display-error "No macro in progress."))
       (negate-minor-mode 'Macro/ Learn)
       (setq last-macro-definition
	   (cdr (nreverse (cons 'macend
			    (do ((l macro-collection-in-progress (cdr l)))
			        ((null l)(display-error "Void macro."))
			        (and (not (atom (car l)))
				   (eq (caar l) 'toplevel-char)
				   (return (cdr l))))))))
       (setq macro-collection-in-progress nil))


(defcom execute-last-editor-macro
        &numeric-argument (&pass)
        (or last-macro-definition (display-error "No macro to run."))
        (push-editor-macro-level last-macro-definition (editor-macro-arg-interp numarg)))


(defun execute-single-editor-enmacroed-command (x)
       (cond ((eq x nil))			;empty in list
	   ((eq x 'halt)
	    (setq macrostack (cdr macrostack))
	    (setq macro-execution-in-progress (cadar macrostack)))
	   ((eq x 'repeat)
	    (setq macro-execution-in-progress (caar macrostack))
	    (rplaca (cdar macrostack) macro-execution-in-progress))
	   ((eq x 'macend)
	    (let ((count (caddar macrostack)))
	         (cond ((eq count 'query)
		      (cond ((macro-query-get-answer)
			   (execute-single-editor-enmacroed-command 'repeat))
			  (t (execute-single-editor-enmacroed-command 'halt))))
		     ((eq count 'forever)
		      (execute-single-editor-enmacroed-command 'repeat))
		     ((< count 2)
		      (execute-single-editor-enmacroed-command 'halt))
		     (t (rplaca (cddar macrostack) (1- count))
		        (setq macro-execution-in-progress (caar macrostack))
		        (rplaca (cdar macrostack) macro-execution-in-progress)))))
	   (t (display-error "Internal macro format error: " x)))))))

;;;
;;;
;;;	Macro utilities
;;;

;;; Save a macro definition
(defcom save-macro
        &prologue &eval (or last-macro-definition
		        (display-error "No macro defintion to store."))
        &arguments ((macro-name &symbol
			  &default &eval
				 (let ((name (intern-minibuf-response "Macro name? " NL)))
				      (cond ((getl name '(editor-command expr subr autoload))
					   (display-error name " is not an acceptable name."))
					  (t name))))
		(macro-key &symbol
			 &default &eval
				(get-key-name (key-prompt "On what key? "))))
        &numeric-argument (&reject)
        (putprop macro-name last-macro-definition 'editor-macro)
        (or (memq macro-key '(CR ^J))		;don't want it anywhere
	  (set-key macro-key macro-name)))


(defcom show-last-or-current-macro
        &numeric-argument (&pass)
        (cond (macro-collection-in-progress (wrap-up-macro-definition)))
        (show-editor-macro last-macro-definition))


(defcom show-macro
        &arguments ((macro-name &symbol &prompt "Macro name: "))
        &numeric-argument (&pass)
        (cond ((get macro-name 'editor-macro)
	     (show-editor-macro (get macro-name 'editor-macro)))
	    (t (display-error macro-name " is not a defined macro."))))


(defun kmacro-display-interpret (x)
       (prog (the-interpretation the-input fun prefix metap numbering stringing l2list whoops)
	   (setq the-input (nreverse (cdr (reverse x))))
tlc 	   (cond ((null the-input)
		(cond (stringing
		        (setq the-interpretation
			    (kmacro-stringing-util stringing the-interpretation))))
		(return (nreverse the-interpretation))))
	   (setq x (car the-input) the-input (cdr the-input))
	   (cond ((not (atom x))(setq x (cdr x))))   ;ignore tlc, ok here.
	   (setq prefix nil)
	   (cond ((> x char-input-mask) (setq x (bit-clear 200 x) metap 1))
	         (t (setq metap 0)))
	   (setq fun (get-key-binding (list metap x nil)) whoops x)
	   (cond (numbering
		 (cond ((kmacro-numberp x)
		        (setq numbering (cons x numbering))
		        (go tlc))
		       (t (setq the-interpretation
			      (cons (cons (implode (nreverse numbering))
				        'Numeric/ argument) the-interpretation)
			      numbering nil)))))
	   (cond ((and (null fun)(not (symbolp 3)))   ;ARRAYP
		(setq prefix x))
	         ((or (eq fun 'escape)
		    (eq fun 'escape-dont-exit-minibuf))
		(and stringing (setq the-interpretation
				 (kmacro-stringing-util stringing the-interpretation)
				 stringing nil))
		(cond ((and (eq fun 'escape)
			  the-input (not (atom (car the-input)))))
		      ;;probbly was ESC ending minibuffer, next was tlc.
 		      ((and the-input (kmacro-number-or-plusminusp (car the-input)))
		       (setq numbering (list (kmacro-number-or-plusminusp (car the-input)))
			   the-input (cdr the-input))
		       (setq the-interpretation
			   (cons (cons (key-total-printed-symbol metap x prefix) fun)
			         the-interpretation))
		       (go tlc))
		      (t (setq metap 1)
		         (cond ((null the-input)
			      (setq x whoops prefix nil metap 0))
			     (t (setq x (cond ((numberp (car the-input))
					   (car the-input))
					  (t (cdar the-input)))
				    the-input (cdr the-input))
			        (and (> x (1- (CtoI "a")))
				   (< x (1+ (CtoI "z")))
				   (setq x (- x 40))))))))
	         ((eq fun 'multiplier)
		(and stringing (setq the-interpretation
				 (kmacro-stringing-util stringing the-interpretation)
				 stringing nil))
		(setq the-interpretation
		      (cons (cons (key-total-printed-symbol metap x prefix)
			        fun)
			  the-interpretation))
		(cond ((and the-input (kmacro-number-or-plusminusp (car the-input)))
		       (setq numbering (list (kmacro-number-or-plusminusp (car the-input)))
			   the-input (cdr the-input))))
		(go tlc)))
	   (cond ((not (null prefix))
		(cond ((null the-input)(setq x whoops prefix nil metap 0))
		      (t (setq x (cond ((numberp (car the-input))
				    (car the-input))
				   (t (cdar the-input)))
			     the-input (cdr the-input))
		         (and (> x (1- (CtoI "a")))(< x (1+ (CtoI "z")))
			    (setq x (- x 40)))))))
	   (setq fun (get-cmd-symbol-3args metap x prefix))
	   (cond ((memq fun '(self-insert overwrite-mode-self-insert))
		(setq stringing (cons (ascii x) stringing)))
	         (t (cond (stringing
			(setq the-interpretation
			      (kmacro-stringing-util
			        stringing the-interpretation)
			      stringing nil)))
		  (setq the-interpretation
		        (cons (cons (key-total-printed-symbol metap x prefix)
				(get-cmd-symbol-3args metap x prefix))
			    the-interpretation))))
	   (setq l2list nil)
cl2c	   (cond ((or (null the-input)
		    (and (not (atom (car the-input)))	;collect lev 2 ch
		         (eq (caar the-input) 'toplevel-char)))
		(cond (l2list
		        (setq the-interpretation
			    (cons (cons (apply 'catenate
					   (nreverse l2list))
				      'Input/ Characters)
				the-interpretation))))
		(go tlc))
	         (t (setq l2list (cons (ascii (car the-input)) l2list)
		        the-input (cdr the-input))
		  (go cl2c)))))

(defun kmacro-stringing-util (s int)
       (map '(lambda (x)(cond ((eq (car x) '/")(rplaca x """""")))) s)
       (cons (cons (catenate """" (apply 'catenate (nreverse s)) """")
	         'String)
	   int))

(defun kmacro-numberp (x)
       (cond ((numberp x))
	   ((not (atom x))(setq x (cdr x))))
       (and (> x (1- (CtoI "0"))) (< x (1+ (CtoI "9"))) x))

(defun kmacro-number-or-plusminusp (x)
       (cond ((numberp x))
	   ((not (atom x)) (setq x (cdr x))))
       (cond ((and (> x (1- (CtoI "0"))) (< x (1+ (CtoI "9")))) x)
	   ((= x (CtoI "+")) '+)
	   ((= x (CtoI "-")) '-)))


(defun show-editor-macro (x)
       (setq x (kmacro-display-interpret x))	;Figger out what it means.
       (init-local-displays)
       (cond (numarg (mapc 'show-editor-macro-2 x))    ;hairy kind
	   (t (local-display-generator-nnl
	        (do ((mac x (cdr mac))
		   (stuff nil (cons (caar mac) stuff)))
		  ((null mac)(apply 'catenate	;WARNING 511 limit
				(mapcar '(lambda (y)(catenate " " y)) (nreverse stuff))))))))
       (end-local-displays))

(defun show-editor-macro-2 (x)
       (local-display-generator-nnl
         (catenate (car x) TAB
	         (cond ((getl (setq x (cdr x))
			  '(expr subr autoload)) x)
		     ((memq x '(String Input/ Characters Numeric/ argument)) x)
		     ((get x 'editor-macro)
		      (catenate x " (keyboard macro)"))
		     (t "--????--")))))


(defcom macro-query
        &numeric-argument (&reject)
        (cond (macro-collection-in-progress
	      (display-error-noabort "Inserting query at this point."))
	    ((not macro-execution-in-progress)
	     (display-error "macro query: no macro running."))
	    (t (cond ((not (macro-query-get-answer))
		    (setq macro-execution-in-progress (caar macrostack)))))))


(defun macro-query-get-answer ()
       (let ((macro-execution-in-progress nil)
	   (macro-collection-in-progress nil))
	  (echo-buffer-print "ok? :")
	  (redisplay)
	  (do ((ans (get-char)(get-char)))
	      (nil)
	      (cond ((= ans 7)(command-quit))
		  ((= ans 161)(command-quit))
		  ((= ans 12))
		  ((= ans 40)(return t))
		  ((= ans 15)(return nil))
		  ((= ans 131)(return t))	;y
		  ((= ans 156)(return nil))	;n
		  (t (return nil))))))

;;;	

;;;
;;;	Quit handling and no-op department - done right BSG 3/28/79
;;;	Improvements for process preservation - BSG 3 December '79


(defun emacs-quit-handler (arg)
       (setq arg arg)
       (signalquit))


(defcom signalquit
        &undo &ignore
        &numeric-argument (&ignore)
        (cond ((eq e-quit-transparency 'transparent)
	     (ioc z))			;This is to check flag safely even if NIL gets clobbered!
					;If this thing blows, you simply can't hit quit on Emacs.
	    (t (let ((oqt e-quit-transparency)	;So that we can quit cleanly
		   (e-quit-transparency 'transparent))
		  (randomize-redisplay)	;in case quit was caused by
		  (or oqt
		      (progn
		        (e_pl1_$set_emacs_tty_modes)	;tty reconnect
		        (clear-the-screen)
		        (and split-mode-p (rdis-restore-screen-to-one-split))))
		  (and DCTL-epilogue-availablep (DCTL-epilogue))
		  (e_pl1_$dump_output_buffer)
		  (e_pl1_$set_multics_tty_modes)
		  (terpri)
		  (cond ((and (eq emacs-name 'emacs_) quit-on-break)
		         (emacs$set_emacs_return_code
			 (error_table_ 'action_not_performed))
		         (or tasking-emacs (lisp-quit-function))))
		  (signalquit-hardcore-vt132-writearound)
		  (ioc z)
		  (e_pl1_$set_emacs_tty_modes)
		  (and DCTL-prologue-availablep (DCTL-prologue))
		  (and split-mode-p (rdis-recreate-splits-on-screen))
		  (or oqt (progn		;Redisplay suppressed
			  (full-redisplay)
			  (display-error-noabort
			    "Restarting from QUIT... ")
			  (redisplay)))))))

;;; Writearound for the hardcore/vt132 bug that causes screen to not
;;; be cleared on ^Z^Z or BREAK.  The problem looks like this:
;;;
;;; (1) Emacs sends characters to fix up screen.
;;; (2) Emacs does (ioc z), causing signal_ quit.
;;; (3) default_error_handler_ does a resetwrite.
;;; (4) Hardcore has not yet sent the clearing characters; they get eaten.
;;; (5) Screen stays screwed, though no longer in Emacs.
;;; (6) User gets confused.
;;;
;;; The only solutions are: (1) Do write_status's until all output is out,
;;; or (2) Just do a (sleep) of some interesting length.  I chose the sleep
;;; option.  If hardcore ever gets fixed, it would be nice to do a
;;; force_out operation to make sure the characters get out.
;;; Richard Mark Soley 14 November 1981
(defun signalquit-hardcore-vt132-writearound ()
       (and (eq tty-type 'vt132) (sleep 2)))

(defcom noop
        &numeric-argument (&ignore)
        &undo &ignore
        )

;;; This hack hides the lisp "quit" function, rebinding "quit"
;;; to "quit-the-editor", a much nicer function from Emacs' point of view.

(putprop 'lisp-quit-function (get 'quit 'subr) 'subr)
(remprop 'quit 'subr)
(defcom-synonym quit quit-the-editor)

;;; Exit from EMACS
(defcom quit-force
        &numeric-argument (&reject)
        (clear-reset)
        (set-lisp-rdis-meters)
        (alarmclock 'time nil) (alarmclock 'runtime nil)
        (cond ((zerop (e_tasking_$quit)) (tasking-restart))
	    (t (lisp-quit-function))))

(defun clear-reset ()
       (clear-the-screen)
       (and split-mode-p (rdis-restore-screen-to-one-split))
       (and DCTL-epilogue-availablep (DCTL-epilogue))
       (e_pl1_$dump_output_buffer)
       (e_pl1_$set_multics_tty_modes))

;;; Restart a tasking Emacs.
(defun tasking-restart () (tasking-restart-internal) (pi-handler))

(defun tasking-restart-internal ()
       (e_pl1_$init)
       (e_pl1_$set_emacs_tty_modes)
       (randomize-redisplay)
       (and DCTL-prologue-availablep (DCTL-prologue))
       (let ((su-args (e_argument_parse_$get_startup_info)))
	  (setq args:apply-arg (caddr (cddddr su-args))
	        args:paths (caddr su-args))
	  (setq emacs-start-ups-need-running 'default-emacs-start-up)
	  (init-echnego-bittab))
       (clear-the-screen)
       (setq tasking-restarted t))

;;; Decide if it's okay to quit now.
(defun okay-to-quit? ()
       (do ((buffers known-buflist (cdr buffers))
	  (found nil))
	 ((null buffers)
	  (cond ((not found) t)
	        (t (init-local-displays)
		 (local-display-generator-nnl "Modified Buffers:")
		 (local-display-generator-nnl "")
		 (mapc 'local-display-buffer-info found)
		 (local-display-generator-nnl "-------------------------")
		 (yesp "Modified buffers exist.  Quit?"))))
	 (and (not (get (car buffers) 'dont-notice-modified-buffer))
	      (not (empty-buffer-p (car buffers)))
	      (get-buffer-state (car buffers) 'buffer-modified-flag)
	      (setq found (cons (car buffers) found)))))

(defun local-display-buffer-info (buffer)
       (let ((path (get-buffer-state buffer 'fpathname)))
	  (local-display-generator-nnl
	    (catenate
	      (cond ((eq current-buffer buffer) ">")
		  ((eq previous-buffer buffer) "<")
		  (t " "))
	      (cond ((get-buffer-state buffer 'buffer-modified-flag) "*")
		  (t " "))
	      (cond (path
		    (catenate buffer
			    (substr "                         "
				  1 (max (- 25.
					  (stringlength buffer))
				         1))
			    path))
		  (t buffer))))))

;;; Mark this Emacs as dead if tasking, then quit.
(defcom destroy-task
        (and minibufferp
	   (display-error "No quitting while in the minibuffer."))
        (cond ((not tasking-emacs)
	     (display-error "This is not a tasking Emacs."))
	    ((not (okay-to-quit?)) (command-quit))
	    (t (e_tasking_$destroy_me)
	       (run-emacs-epilogue-actions)
	       (quit-force))))

;;; Exit from EMACS if no buffers are modified or user says OK
(defcom quit-the-editor
        &numeric-argument (&reject)
        (and minibufferp
	   (display-error "No quitting while in the minibuffer."))
        (cond (tasking-emacs (clear-reset) (e_tasking_$quit) (tasking-restart))
	    ((okay-to-quit?)
	     (run-emacs-epilogue-actions)
	     (quit-force))
	    (t (command-quit))))	      

(defun run-emacs-epilogue-actions ()		;5/6/80
       (do nil ((null emacs-epilogue-action-list))
	 (errset (apply (caar emacs-epilogue-action-list)
		      (cdar emacs-epilogue-action-list)))
	 (setq emacs-epilogue-action-list (cdr emacs-epilogue-action-list))))


(defun set-emacs-epilogue-handler (fnandargs dupflg)
       (or (and dupflg (assq (car fnandargs) emacs-epilogue-action-list))
	 (setq emacs-epilogue-action-list (cons fnandargs emacs-epilogue-action-list))))

;;;

(defun telnet-loser (c)
       (cond ((or (= c 363)(= c 364))		;BREAK, IP
	    (signalquit))
	   ((= c 253.)			;IAC DO
	    (setq c (e_pl1_$get_char))
	    (cond ((not (= c 1))		;DO ECHO
		 (display-error-noabort "Ignoring TELNET IAC DO " (implode (explodec c))))))
	   ((= c 254.)			;IAC DONT
	    (setq c (e_pl1_$get_char))
	    (cond ((not (= c 1))		;DONT ECHO
		 (display-error-noabort "Ignoring TELNET IAC DONT " (implode (explodec c))))))
	   (t (display-error-noabort "Ignoring TELNET IAC " (implode (explodec c)) "(octal). Good luck."))))


(defun define-autoload-lib fexpr (x)
       (mapc '(lambda (y)(set-autoload-lib y (car x)))(cdr x)))

;;;
;;;
;;;	HELP! What did I type?!?!? 2/11/79
;;;

(defcom help
        &undo &ignore
        &numeric-argument (&ignore)
        (init-local-displays)
        (local-display-generator-nnl
	(catenate "Help segments on Emacs are found in " documentation-dir "."))
        (mapc 'local-display-generator-nnl
	    '("See emacs.gi.info there for full information on everything."
	       "Type the escape key, the question mark key, and some key that"
	       "you want to know about to find out about it.  Type a control underscore"
	       "at any time to get more help.  Type control underscore"
	       "and a question mark for all help commands."
	       "Type two linefeeds to remove this display,"
	       "or any other display that ends with -- * * * * * * * --,"
	       "from your screen."))
        (end-local-displays))

(defcom-synonym ? help)


(defcom help-on-tap
        &numeric-argument (&ignore)
        &undo &ignore
        (minibuffer-print "HELP: (? for more info): ")
        (do x (get-char)(get-char) nil
	  (and (> x (1- #/a))
	       (< x (1+ #/z))
	       (setq x (- x 40)))
	  (cond ((= x 12))
	        ((= x #/H)(help))
	        ((= x #/C)(execute-command 'describe-key nil nil))
	        ((= x #/D)(execute-command 'describe nil nil))
	        ((= x #/A)(execute-command 'apropos nil nil))
	        ((= x #/L)(help-list-typin))
	        ((= x #/?)(help-whats-on-tap))
	        ((= x 7)(command-quit))	;^G
	        (t (help-whats-on-tap)))
	  (or (= x 12)(return nil)))
        (minibuffer-print ""))


(defun help-whats-on-tap ()
       (init-local-displays)
       (mapc 'local-display-generator-nnl
	   '("^_ H gives general help info."
	     "^_ ? gives this list of what ^_ can do."
	     "^_ A followed by a word and a CR looks for appropriate"
	     "     matching commands. Type ^_ D apropos CR for more on this."
	     "^_ C prompts for a character (or key sequence) and tells what it does."
	     "^_ D followed by an extended command name and a CR tells"
	     "     about the extended command."
	     "^_ L Lists the last 50 characters or commands typed."))
       (local-display-generator-nnl
         "Type two linefeeds to remove this display from your screen.")
       (end-local-displays))


(defun help-list-typin ()
       (do ((stop (cond ((= history-next-index 0) 50.)
		    (t history-next-index)))
	  (cur history-next-index (1+ cur))
	  (first t nil)
	  (nl)
	  (l))
	 ((and (not first)(= cur stop))
	  (do c 0 (1+ c)(= c 50.)
	      (or l (return nil))
	      (setq nl (cons (car l) nl) l (cdr l)))
	  (init-local-displays)
	  (do ((line (catenate (printable (car nl)) " ")
		   (catenate line (cond (nl (printable (car nl)))
				    (t ""))
			   " ")))
	      ((null nl)
	       (or (nullstringp line)(samepnamep line " ")
		 (local-display-generator-nnl line)))
	      
	      (cond ((> (stringlength line)(- screenlinelen 6))
		   (local-display-generator-nnl line)
		   (setq line "")))
	      (setq nl (cdr nl))))
	 (and (= cur 50.)(setq cur 0))
	 (cond ((numberp (saved-command-history cur))
	        (setq l (cons (saved-command-history cur) l)))
	       ((null (saved-command-history cur)))
	       ;; Next case is combined chars from get-top-level-char-innards
	       (t (setq l (append (nreverse (explodec (saved-command-history cur)))
			      l)))))
       (local-display-generator-nnl "Type two linefeeds to remove this display from the screen.")
       (end-local-displays))
