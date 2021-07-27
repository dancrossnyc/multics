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
;;;	Various Hairy Search Commands
;;;	 GMP
;;;	CR/NL handling 5/23/80 by BSG
;;;	Gratuitous marks 11/06/81 by Barmar
;;;	JSL's regular expressions and other stuff, 30 August 1982 Barmar
;;;	Added ^_ (self-documentation) response to searches,
;;;	and moved query-replace out to e_macops_. 31 August 1982 Barmar
;;;

(%include backquote)

;;; read macro 12/3/78 by BSG
(eval-when (compile eval)
(setsyntax '/# 'macro
	 '(lambda ()
		(cond ((= (tyipeek) 57)
		       (tyi)
		       (tyi))
		      ((= (tyipeek) 136)
		       (tyi)
		       (- (boole 1 137 (tyi)) 100)))))
);;;end of eval-when

(defun chars-left-in-line macro (x)
       `(- curlinel curpointpos 1))

(defun save-excursion-on-search-failure macro (x)
       (let ((dummy (gensym))
	   (forms (cdr x))
	   (mark (gensym)))
	  `(let ((,dummy nil)
	         (,mark nil))
	        (unwind-protect
		(progn
		  (setq ,mark (set-mark))
		  (setq ,dummy (progn . ,forms)))
		(if ,mark
		    (if (null ,dummy) (go-to-mark ,mark))
		    (release-mark ,mark))))))

(%include e-macros)

(declare (special search-forward last-search-string search-string search-ring
	        search-from-end tty-no-upmotionp must-announce-search
	        last-char-was-^S isearch-stack macro-execution-in-progress
	        ITS-string-search-set-mark home-mark isearch-exit-char
	        MCS-editing-characters rubout-character)
         
         (*expr minibuffer-rubout search:maybe-push-default
	      set-permanent-key mark-at-current-point-p
	      exch-point-mark forward-search-bounded go-to-line-point
	      reverse-search-bounded search:announce-partial-failure 
	      search:last-string search:numeric-prompt search:prompt
	      search:rotate-ring)

         (*lexpr gratuitous-mark-setter))


;;; Command intended for use in start_up.emacs.  It sets permanent definitions
;;; of ^S and ^R to specified type of search.  Note that when an unrecognized
;;; type is supplied it merely prints an error without using command-quit.

(defcom set-search-mode
        &args ((search-type &symbol &prompt "Search mode:  "
		        &completions '(string character ITS-string
				   its-string incremental regexp
				   regular-expression default)))
        (cond ((memq search-type '(default string))
	     (set-permanent-key '^S 'string-search)
	     (set-permanent-key '^R 'reverse-string-search))
	    ((memq search-type '(character))
	     (set-permanent-key '^S 'character-search)
	     (set-permanent-key '^R 'reverse-character-search))
	    ((memq search-type '(ITS-string its-string))
	     (set-permanent-key '^S 'ITS-string-search)
	     (set-permanent-key '^R 'reverse-ITS-string-search))
	    ((memq search-type '(regular-expression regexp))
	     (set-permanent-key '^S 'regexp-search)
	     (set-permanent-key '^R 'reverse-regexp-search))
	    ((eq search-type 'incremental)
	     (set-permanent-key '^S 'incremental-search)
	     (set-permanent-key '^R 'reverse-incremental-search))
	    (t (display-error-noabort "Unknown search mode: " search-type)
	       (ring-tty-bell))))
;;; 

;;;
;;;	Character search commands (from ITS)
;;;	 GMP, 08/31/78
;;;


;;; Character search command
(defcom character-search
        (let ((search-forward t))
	   (character-search-)))


;;; Reverse character search command
(defcom reverse-character-search
        (let ((search-forward nil))
	   (character-search-)))


;;; Subr that actually does character search
(defun character-search- ()
       (with-mark home-mark
         (let ((quoted nil))
	  (do-forever
	    (let ((ch (get-char)))
	         (cond ((and (= ch #^A) (not quoted))	; string search
		      (ITS-string-search-) (stop-doing))
		     ((and (= ch #^G) (not quoted))	; punt
		      (command-quit))
		     ((or (= ch #^J)	; find line break
			(and (= ch #^M) (not quoted)))     ; ^M (unquoted), same as ^J
		      (search:maybe-push-default NL 'string)
		      (if search-forward
			(if (lastlinep) (display-error "Search fails."))
			(next-line)
			else (if (firstlinep) (display-error "Search fails."))
			(prev-line) (go-to-end-of-line))
		      (gratuitous-mark-setter home-mark)
		      (stop-doing))
		     ((and (= ch #^Q) (not quoted))	; quote char
		      (setq quoted t))
                         ((and (= ch #^R) (not quoted))     ; reverse direction
                          (if search-forward
                              (setq search-forward (not search-forward))
                              else (and (search-for-default-string)
				(gratuitous-mark-setter home-mark))
			(stop-doing)))
                         ((and (= ch #^S) (not quoted))     ; look for default
                          (and (search-for-default-string)
			 (gratuitous-mark-setter home-mark))
		      (stop-doing))
		     ((and (= ch #^_) (not quoted))
		      (character-search-documentation))
		     (t			; look for this
		       (let ((result nil))
			  (if search-forward (setq result (forward-search (ascii ch)))
			      else (setq result (reverse-search (ascii ch))))
			  (search:maybe-push-default (ascii ch) 'string)
			  (if result
			      (gratuitous-mark-setter home-mark)
			      (stop-doing)
			      else (display-error "Search fails."))))))))))


;;; Search for current default string
(defun search-for-default-string ()
       (if (nullstringp last-search-string)
	 (display-error "No default search string.")
	 else (let ((result nil))
		 (if (> (stringlength last-search-string) 1)
		     (minibuffer-clear)
		     (minibuffer-print (cond (search-forward
					 "")
				         (t
					 "Reverse "))
				   "Search: " last-search-string))
		 (if search-forward (setq result (forward-search last-search-string))
		     else (setq result (reverse-search last-search-string)))
		 (or result (display-error "Search fails.")))))

(defun character-search-documentation ()
       (init-local-displays)
       (mapc 'local-display-generator-nnl
	   '("Character search options:" ""
	     "^S        Search for default search string"
	     "^R        If searching forward, reverse direction, otherwise"
	     "          Search back for default string"
	     "^A        ITS string search"
	     "CR, LF    Search for next newline"
	     "^G        Abort search"
	     "^Q	      Reads a character and searches for it"
	     "^_        Print this description"
	     "anything else"
	     "          searches for the character"
	     "" "Type any character to remove this display."))
       (end-local-displays)
       (redisplay)
       (get-char))
;;; 

;;;
;;;	ITS String search commands
;;;	 GMP, 08/31/78
;;;	Cleaned up and bugs fixed 1 July 1981 Barry Margolin
;;;	Merged and installed 1 July 1981 RMSoley
;;;

;;; ITS string search command
(defcom ITS-string-search
        (let ((search-forward t))
	   (ITS-string-search-)))


;;; Reverse ITS string search command
(defcom reverse-ITS-string-search
        (let ((search-forward nil))
	   (ITS-string-search-)))


;;; Subr to perform ITS string search
(defun ITS-string-search- ()
       (with-mark home-mark
	        (setq last-char-was-^S nil
		    search-string ""
		    search-from-end nil)
	        (ITS-string-search-announce)
	        (let ((ITS-string-search-set-mark nil)
		    (rubout-character (cadr MCS-editing-characters)))
		   (do-forever
		     (if (eq (ITS-string-search-process-char (get-char))
			   'done)
		         (stop-doing))))
	        (if (not macro-execution-in-progress)
		  (minibuffer-print-noclear "   Done."))))


;;; Announce direction, type, and search string
(defun ITS-string-search-announce ()
       (if (not macro-execution-in-progress)
	 (minibuffer-clear)
	 (if search-forward
	     (if search-from-end
	         (minibuffer-print "BJ ITS String Search: ")
	         else (minibuffer-print "ITS String Search: "))
	     else
	     (if search-from-end
	         (minibuffer-print "ZJ Reverse ITS String Search: ")
	         else (minibuffer-print "Reverse ITS String Search: ")))
	 (minibuffer-print-noclear search-string))
       (setq must-announce-search nil))


;;; Handle single character of ITS string search
(defun ITS-string-search-process-char (ch)
       (prog1
         (cond
	 ((or (= ch 177) (= ch rubout-character))
	  (if (nullstringp search-string)
	      (ITS-string-search-quit)
	      else
	      (isearch-chop-string-and-minibuffer)   ; need better for printing
	      'continue))
	 ((= ch #^J) 'continue)		;LF
	 ((= ch #^G)
	  (ITS-string-search-quit))		; punt
	 ((= ch #^B)			; complement search from beginning
	  (if search-forward
	      (setq search-from-end (not search-from-end))
	      (ITS-string-search-announce)
	      else
	      (ITS-string-search-error
	        "Can not search from beginning in reverse search."
	        nil))
	  'continue)
	 ((= ch #^E)			; complement search from end
	  (if search-forward
	      (ITS-string-search-error
	        "Can not search from end in forward search."
	        nil)
	      else
	      (setq search-from-end (not search-from-end))
	      (ITS-string-search-announce))
	  'continue)
	 ((= ch #^L)
	  (if (not macro-execution-in-progress)
	      (minibuffer-clear)
	      (redisplay))		; redisplay
	  (ITS-string-search-announce)
	  'continue)
	 ((= ch #^Y)			; append default string
	  (if (nullstringp last-search-string)
	      (ITS-string-search-error "No default search string." nil)
	      else
	      (setq search-string
		  (catenate search-string last-search-string))
	      (ITS-string-search-out last-search-string))
	  'continue)
	 ((= ch #^D)			; yank default and rotate
	  (if (nullstringp last-search-string)
	      (ITS-string-search-error "No default search string." nil)
	      else
	      (setq search-string (search:rotate-ring))
	      (setq last-search-string (search:last-string)) ; copy of top
	      (ITS-string-search-announce))
	  'continue)
	 ((= ch #^Q)			; quote next chararacter
	  (let ((ch1 (ascii (get-char))))
	       (setq search-string (catenate search-string ch1))
	       (ITS-string-search-out ch1))
	  'continue)
	 ((= ch #^R)			; reverse direction of search
	  (setq search-forward (not search-forward))
	  (ITS-string-search-announce)
	  'continue)
	 ((or (= ch #^S) (= ch #^[))		; ^S or ESC, search and maybe quit
	  (if (and (= ch #^[) last-char-was-^S)	; ESC after ^S, just exit
	      'done
	      else
	      (if (nullstringp search-string)
		(setq search-string last-search-string)
		(ITS-string-search-out search-string))
	      (if (nullstringp search-string)
		(ITS-string-search-error "No search string." (= ch #^[))
		else
		(with-mark
		  start-pos
		  (let ((result nil))
		       (if search-from-end
			 (if search-forward
			     (go-to-beginning-of-buffer)
			     else (go-to-end-of-buffer)))
		       (if search-forward
			 (setq result (forward-search search-string))
			 else
			 (setq result (reverse-search search-string)))
		       (if result
			 (if (not ITS-string-search-set-mark)
			     (setq ITS-string-search-set-mark t)     ;remember that we did it.
			     (gratuitous-mark-setter home-mark))
			 (or macro-execution-in-progress
			     (redisplay))
			 (if tty-no-upmotionp
			     (setq must-announce-search t))
			 else
			 (ITS-string-search-error "Search fails."
					      (= ch #^[))
			 (go-to-mark start-pos)))))
	      (search:maybe-push-default search-string 'string)
	      (if (= ch #^S) 'continue	; keep looking
		else 'done)))		; ESC, search terminates
	 ((= ch #^_)
	  (ITS-string-search-documentation)
	  'continue)
	 ((and (or (< ch 40) (> ch 177))	; unknown control
	       (not (or (= ch #^M) (= ch #^I))))
	  (ring-tty-bell)
	  'continue)
	 (t				; normal character
	   (if (= ch #^M) (setq ch #^J))	;cr => nl 5/23/80
	   (setq search-string 
	         (catenate search-string (ascii ch)))
	   (ITS-string-search-out (ascii ch))))
         (setq last-char-was-^S (= ch #^S))))

;;; Add string to minibuffer unless must redisplay minibuffer
(defun ITS-string-search-out (string)
       (if must-announce-search (ITS-string-search-announce)
	 else (or macro-execution-in-progress
		(minibuffer-print-noclear string))))


;;; Print error for ITS string search
(defun ITS-string-search-error (message use-minibuffer)
       (if (or tty-no-upmotionp use-minibuffer)
	 (minibuffer-print message)		; not display-error since not fatal
	 (setq must-announce-search t)
	 else				; for display, print it
	 (init-local-displays)
	 (local-display-generator-nnl message)
	 (minibuffer-print-noclear ""))	; reposition cursor
       (if macro-execution-in-progress (command-quit)
	 else (ring-tty-bell)))


;;; Exit ITS string search
(defun ITS-string-search-quit ()
       (if (not macro-execution-in-progress)
	 (minibuffer-print-noclear "   Done.")) ; If displaying, output message.
       (command-quit))

(defun ITS-string-search-documentation ()
       (init-local-displays)
       (mapc 'local-display-generator-nnl
	   `("ITS string search options:" ""
	     ,(catenate "DEL, "
		      (ItoC rubout-character)
		      "    Remove last character from search string")
	     "ESC       Exit search, possibly searching first if previous"
	     "          character was not ^S"
	     "^S        Search for next occurrence of search string or default"
	     "^R        Reverse search direction"
	     "^B        Toggle ""search from beginning of buffer"""
	     "^E        Toggle ""search from end of buffer"""
	     "^Y        Add default search string to search string"
	     "^D        Rotate default search string ring, and makes it the"
	     "          search string"
	     "CR        Add newline to search string"
	     "^G        Abort search and return to starting point"
	     "^Q	      Reads a character and adds it to search string"
	     "LF        Nothing"
	     "^L        Redisplay"
	     "^_        Print this description"
	     "printing characters, TAB, ^I"
	     "          Adds to the search string, and searches"
	     "" "Type any character to remove this display."))
       (end-local-displays)
       (redisplay)
       (get-char))
;;; 

;;;
;;;	Incremental Search
;;;


;;; Incremental search command
(defcom incremental-search
        (let ((search-forward t))
	   (incremental-search-)))

;;; Reverse Incremental search command
(defcom reverse-incremental-search
        (let ((search-forward nil))
	   (incremental-search-)))

;;;Subr to do all the work
(defun incremental-search- ()
       (setq isearch-stack (list (cons nil (set-mark))))
       (setq search-string "")
       (incremental-search-announce)
       (let ((isearch-exit-char nil)
	   (rubout-character (cadr MCS-editing-characters)))
	  (with-mark home-mark
		   (do-forever
		     (or macro-execution-in-progress (redisplay))
		     (if (eq (isearch-process-char (get-char)) 'done)
		         (stop-doing)))
		   (if (not (nullstringp search-string))     ;if didn't abort search
		       (gratuitous-mark-setter home-mark))
		   (search:maybe-push-default search-string 'string))
	  (mapc '(lambda (x)
		       (release-mark (cdr x)))
	        isearch-stack)
	  (if (not macro-execution-in-progress)
	      (minibuffer-print-noclear "   Done.")
	      (redisplay))
	  (and isearch-exit-char
	       (process-char isearch-exit-char))))

;;; Process a single character
(defun isearch-process-char (ch)
       (cond ((or (= ch 177)
	        (= ch rubout-character)) ;rubout last char
	    (isearch-rubout))
	   ((= ch #^G)			; abort search
	    (ring-tty-bell)
	    (setq search-string "")
	    (go-to-mark (cdar (last isearch-stack)))
	    'done)
	   ((= ch #^L)			; redisplay
	    (or macro-execution-in-progress (redisplay))
	    (incremental-search-announce)
	    'continue)
	   ((= ch #^Q)			; quote next char
	    (isearch-search-single (ascii (get-char))))
	   ((or (= ch #^S)(= ch #^R))		; search again or use default
	    (let ((new-dir (= ch #^S)))
	         (if (not (eq new-dir search-forward))
		   (setq search-forward new-dir)
		   (or macro-execution-in-progress
		       (minibuffer-clear))
		   (incremental-search-announce)))
	    (if (not (nullstringp search-string))
	        (search:maybe-push-default search-string 'string)
	        (setq search-string "")
	        else
	        (or macro-execution-in-progress
		  (minibuffer-print-noclear last-search-string)))
	    (setq isearch-stack (cons (cons nil (set-mark))    ;non-inserting
				isearch-stack))
	    (let ((nss (catenate search-string last-search-string)))
	         (if search-forward		;Movin' right...
		   (if (looking-at last-search-string) ;already in front of it, OK
		       (forward-search last-search-string)
		       (setq search-string nss)
		       'continue
		       else
		       (if (forward-search nss)
			 (setq search-string nss)
			 'continue
			 else		;not found again
			 (or macro-execution-in-progress
			     (minibuffer-clear))
			 (incremental-search-failure)
			 (incremental-search-announce)))
		   else			;Movin' left...
		   (if (reverse-search nss)
		       (setq search-string nss)
		       'continue
		       else
		       (or macro-execution-in-progress
			 (minibuffer-clear))
		       (incremental-search-failure)
		       (incremental-search-announce)))))
	   ((= ch #^[)			; all done
	    'done)
	   ((= ch #^J) 'continue)
	   ((= ch #^M) (isearch-search-single NL))
	   ((= ch #^_) (incremental-search-documentation))
	   ((and (or (< ch 40) (> ch 177))	;random control char, exits 
	         (not (= ch #^I)))		;search, then gets executed
	    (setq isearch-exit-char ch)
	    'done)
	   (t				;normal char, search for it
	     (isearch-search-single (ascii ch)))))


;;; Delete a character from search string
(defun isearch-rubout ()
       (cond ((null (cdr isearch-stack))	;nothing to rubout, abort
	    (ring-tty-bell)
	    'done)
	   (t
	     (go-to-mark (cdar isearch-stack))
	     (release-mark (cdar isearch-stack))
	     (cond ((caar isearch-stack)	;rubbing out self-insert
		  (isearch-chop-string-and-minibuffer)))
	     (setq isearch-stack (cdr isearch-stack))
	     'continue)))


;;; Delete a character from search string
(defun isearch-rubout ()
       (cond ((null (cdr isearch-stack))	;nothing to rubout, abort
	    (ring-tty-bell)
	    'done)
	   (t
	     (go-to-mark (cdar isearch-stack))
	     (release-mark (cdar isearch-stack))
	     (cond ((caar isearch-stack)	;rubbing out self-insert
		  (isearch-chop-string-and-minibuffer)))
	     (setq isearch-stack (cdr isearch-stack))
	     'continue)))

(declare (special display-ctlchar-with-^))

(defun isearch-chop-string-and-minibuffer ()
       (let ((sl (stringlength search-string)))
	  (let ((lastch (CtoI (substr search-string sl 1))))
	       (setq search-string (substr search-string 1 (1- sl)))
	       (if (and (not tty-no-upmotionp)
		      (not macro-execution-in-progress))
		 (minibuffer-rubout
		   (cond ((and (> lastch 37) (< lastch 177)) ;printing char
			1)
		         (display-ctlchar-with-^ 2)
		         (t 4)))))))	;pretty kludgey, eh?

;;; Search for a single character incrementally
(defun isearch-search-single (ch)
       (if (and (not tty-no-upmotionp)		;put in buffer if needed
	      (not macro-execution-in-progress))
	 (minibuffer-print-noclear ch))
       (setq search-string (catenate search-string ch))
       (setq isearch-stack (cons (cons 'insert (set-mark))
			   isearch-stack))
       (if search-forward
	 (if-at ch			;char is here, continue along
	        (forward-char)
	        'continue
	        else			;not here, search again
	        (if (forward-search search-string)
		  'continue		;found it
		  else			;not found, flush char typed
		  (incremental-search-failure)
		  (isearch-rubout)))
	 else				;Reverse Isearch
	 (if (looking-at search-string)
	     'continue
	     else
	     (do-times (1- (stringlength search-string))
		     (forward-char))
	     (if (reverse-search search-string)
	         'continue
	         else
	         (do-times (1- (stringlength search-string))
		         (backward-char))
	         (incremental-search-failure)
	         (isearch-rubout)))))

(defun incremental-search-announce ()
       (if (not macro-execution-in-progress)
	 (if search-forward (minibuffer-print "Incremental Search: ")
	     else (minibuffer-print "Reverse Incremental Search: "))
	 (minibuffer-print-noclear search-string))
       'continue)

(defun incremental-search-failure ()
       (if macro-execution-in-progress
	 (go-to-mark (cdar (last isearch-stack)))
	 (mapc '(lambda (x)
		      (release-mark (cdr x)))
	       isearch-stack)
	 (setq search-string "")
	 (search-failure-annunciator)
	 else
	 (ring-tty-bell)))

(defun incremental-search-documentation ()
       (init-local-displays)
       (mapc 'local-display-generator-nnl
	   `("Incremental search options:" ""
	     ,(catenate "DEL, "
		      (ItoC rubout-character)
		      "    Undo last character")
	     "ESC       Exit search"
	     "^S        Search for next occurrence of search string or default"
	     "^R        Reverse search for next occurrence"
	     "CR        Add newline to search string"
	     "^G        Abort search and return to starting point"
	     "^Q	      Reads a character, adds it to search string, and searches"
	     "LF        Nothing"
	     "^L        Redisplay"
	     "^_        Print this description"
	     "printing characters, TAB, ^I"
	     "          Adds to the search string, and searches"
	     "other control characters"
	     "          Ends search, executes as an Emacs command"
	     "" "Type any character to remove this display."))
       (end-local-displays)
       (redisplay)
       (get-char))


;;;
;;; Global Regular Expression Print
;;;

(defcom global-regexp-print
        &arguments ((string &string &default
		        &eval (regexp:prompt "Global regexp print")))
        (setq string (regexp:compile-and-save string))
        (let ((foundflag)
	    (tempmark))
	   (save-excursion
	     (go-to-beginning-of-buffer)
	     (do-forever
	       (setq tempmark (regexp:search string))
	       (if (not tempmark) (stop-doing))
	       (if (not foundflag)
		 (setq foundflag t)
		 (init-local-displays))
	       (if (not (mark-on-current-line-p tempmark))
		 (exch-point-mark tempmark)
		 (do-forever
		   (local-display-current-line)
		   (next-line)
		   (if (mark-on-current-line-p tempmark) (stop-doing))))
	       (release-mark tempmark)
	       (local-display-current-line)
	       (if (lastlinep) (stop-doing))
	       (next-line)))
	   (if foundflag (end-local-displays)
	       else (search-failure-annunciator))))

;;; These commands autoload from emacs-extended-searches

;;;
;;; Regular Expression searches in Lisp.
;;;  J. Spencer Love, 7 May 1982
;;;

(defcom-synonym regexp-search-command regexp-search)

(defcom regexp-search
        &cleanup regexp:command-cleanup
        &prologue regexp:command-prologue
        &epilogue regexp:command-epilogue
        &inverse reverse-regexp-search
        &negative-function reverse-regexp-search
        &numeric-argument &repeat
        &args ((regexp &default &eval (regexp:prompt "Regexp search")))
        (setq regexp (regexp:compile-and-save regexp))
        (save-excursion-on-search-failure
	(regexp:search regexp)))


(defun regexp-search-in-line (regexp)
       (setq regexp (regexp:compile-and-save regexp))
       (save-excursion-on-search-failure
         (regexp:match regexp (chars-left-in-line) nil)))


(defcom reverse-regexp-search
        &cleanup regexp:command-cleanup
        &prologue regexp:command-prologue
        &epilogue regexp:command-epilogue
        &inverse regexp-search
        &negative-function regexp-search
        &numeric-argument &repeat
        &args ((regexp &default &eval (regexp:prompt "Reverse regexp search")))
        (setq regexp (regexp:reverse (regexp:compile-and-save regexp)))
        (save-excursion-on-search-failure
	(regexp:reverse-search regexp)))


(defun reverse-regexp-search-in-line (regexp)
       (setq regexp (regexp:reverse (regexp:compile-and-save regexp)))
       (save-excursion-on-search-failure
         (regexp:reverse-match regexp curpointpos nil)))

(defun regexp:command-prologue ()
       (list (or numarg 1) 0 (set-mark)))


(defun regexp:command-cleanup (prologue-info)
       (if prologue-info
	 (if (cdddr prologue-info)
	     (release-mark (cdddr prologue-info)))
	 (if (caddr prologue-info)
	     (go-to-mark (caddr prologue-info))
	     (release-mark (caddr prologue-info)))))


(defun regexp:command-epilogue (prologue-info result last-time)
       (cond (result
	     (rplaca (cdr prologue-info) (1+ (cadr prologue-info)))
	     (and (cdddr prologue-info)
		(release-mark (cdddr prologue-info)))
	     (or last-time
	         (rplacd (cddr prologue-info) result)))
	   ((null (cdddr prologue-info))
	    (search-failure-annunciator))
	   (t (setq result (cdddr prologue-info)
		  last-time t)
	      (save-excursion
	        (go-to-mark (caddr prologue-info))
	        (set-the-mark))))
       (if last-time
	 (exch-point-mark result)
	 (set-the-mark)
	 (exch-point-mark result)
	 (release-mark result)
	 (release-mark (caddr prologue-info))
	 (rplaca (cddr prologue-info) nil)	; For cleanup
	 (if (< (cadr prologue-info) (car prologue-info))
	     (search:announce-partial-failure (cadr prologue-info)))))


(defun regexp:prompt (prompt)
       (setq prompt (search:prompt (search:numeric-prompt prompt)))
       (regexp:compile-and-save prompt)
       (search:maybe-push-default prompt 'regexp))

;;;
;;; Translating regular expressions to list form.
;;;
;;; The format of a compiled regular expression is:
;;;
;;;   ((original-string . reversed-token-list) . token-list)
;;;
;;; The original-string is the argument given to compile-regexp.
;;; The reversed-token-list is initially nil, and is filled in
;;; by reverse-regexp, which returns the car of the compiled regexp.
;;;
;;; Each token in the token list is of the form:
;;;
;;;   (tag . value)
;;;
;;; CONSTRUCT	TAG		VALUE
;;;   ^		begins-string	nil
;;;   $		ends-string	nil
;;;   string	constant		string from (maknam)
;;;   .*		star		nil
;;;   *		star		preceding char from (ascii)
;;;   .		dots		count of contiguous dots
;;;

(defvar regexp:saved-compiled-string nil)

(defun regexp:compile-and-save (regexp)
       (cond ((nullstringp regexp)
	    (if regexp:saved-compiled-string regexp:saved-compiled-string
	        else (display-error "No saved regular expression.")))
	   ((samepnamep regexp (caar regexp:saved-compiled-string))
	    regexp:saved-compiled-string)
	   (t (setq regexp:saved-compiled-string (regexp:compile regexp)))))


(defun regexp:reverse (regexp)
       (cond ((cdar regexp) (car regexp))
	   ((null (cdr regexp)) (car regexp))
	   ((< (length (cdr regexp)) 2)
	    (rplacd (car regexp) (cdr regexp)))
	   (t (rplacd (car regexp) (reverse (cdr regexp))))))

;;; Here follow macros for lexically inserting code into compile-regexp,
;;; which follows them.  In some cases the macros are used in multiple
;;; places, but others are split out to make the code clearer and keep
;;; the indentation reasonable for 80 column screens.

(defun regexp-emit macro (x)			; A conventional PUSH macro
       (let ((tag (cadr x))
	   (value (caddr x)))
	  `(rplacd compiled-regexp (cons (cons ,tag ,value)
				   (cdr compiled-regexp)))))


(defun regexp-emit-constant macro (x)		; Construct a constant string
       (let ((delimiter (cadr x)))		; to be PUSHed, if present.
	  `(cond ((null constant-begins))
	         ((eq constant-begins ,delimiter)
		(setq constant-begins nil))
	         (t (do ((cursor constant-begins (cdr cursor)))
		      ((eq (cdr cursor) ,delimiter)
		       (rplacd cursor nil)))
		  (regexp-emit 'constant (maknam constant-begins))
		  (setq constant-begins nil)))))


(defun regexp-emit-dots macro (x)		; Count the contiguous dots
       (let ((delimiter (cadr x)))		; and PUSH a token for them.
	  `(cond ((null dots-begin))
	         ((eq dots-begin ,delimiter)
		(setq dots-begin nil))
	         (t (do ((count 1 (1+ count))
		       (cursor dots-begin (cdr cursor)))
		      ((eq (cdr cursor) ,delimiter)
		       (regexp-emit 'dots count)))
		  (setq dots-begin nil)))))


(defun regexp-mark-constant macro (x)		; Note the beginning of a
       `(progn				; constant string.
	(regexp-emit-dots this-one)
	(if (null constant-begins) (setq constant-begins this-one))))

(defun regexp:compile (regexp-string)
       (let ((regexp-list (exploden regexp-string))
	   (compiled-regexp (list (list regexp-string))))
	  (if (= (car regexp-list) #/^)
	      (regexp-emit 'begins-line nil)
	      (setq regexp-list (cdr regexp-list)))
	  (do ((backslash-at 'backslash-at)
	       (ch (car regexp-list) (cadr this-one))
	       (constant-begins)
	       (dots-begin)
	       (escape)
	       (escape-patch)
	       (last-one nil this-one)
	       (star-at)
	       (this-one regexp-list (cdr this-one)))
	      ((null this-one)
	       (if escape
		 (display-error
		   "Invalid use of ""\c"" at end of regular expression."))
	       (regexp-emit-constant nil)
	       (regexp-emit-dots nil)
	       (rplacd compiled-regexp (nreverse (cdr compiled-regexp))))
	      (cond (escape
		    (setq escape nil)
		    (regexp-mark-constant))
		  ((= ch #/\)
		   (setq backslash-at this-one
		         escape-patch last-one)
		   (regexp-mark-constant))
		  ((and (= ch #/c) (eq backslash-at last-one))
		   (setq escape t)
		   (if (eq constant-begins backslash-at)
		       (setq constant-begins nil)
		       else (rplacd escape-patch (cdr this-one))))
		  ((= ch #/.)
		   (regexp-emit-constant this-one)
		   (if (null dots-begin) (setq dots-begin this-one))
		   (rplaca this-one nil))
		  ((= ch #/*)
		   (if (eq last-one star-at)
		       (display-error
		         "Invalid use of ""*"" in regular expression."))
		   (regexp-emit-constant last-one)
		   (regexp-emit-dots last-one)
		   (regexp-emit 'star (and (car last-one)
				       (ascii (car last-one))))
		   (setq star-at this-one))
		  ((and (= ch #/$) (null (cdr this-one)))
		   (regexp-emit-constant this-one)
		   (regexp-emit-dots this-one)
		   (regexp-emit 'ends-line nil))
		  (t (regexp-mark-constant))))))

;;;
;;; Regular Expression match routines.
;;;
;;; Here follow a number of pairs of action routines.  These routines are
;;; in the form of macros for lexical insertion of code into the routines
;;; regexp: search and match, forward and reverse, which are the recursive
;;; search routines which actually perform regular expression
;;; matching.  The macro pairs are for forward and reverse matching
;;; respectively, and are grouped together for ease of maintenance.
;;;

(declare (special curline curstuff))

(defun regexp-constant-floating macro (x)
       `(do ((backup (1- (stringlength (cdar regexp))))
	   (mark)
	   (string (cdar regexp)))
	  ((not (forward-search string)) nil)
	  (setq mark (regexp:match regexp 0 nil))
	  (if mark
	      (exch-point-mark mark)
	      (do-times (1+ backup) (backward-char))
	      (exch-point-mark mark)
	      (return mark))
	  (do-times backup (backward-char))))


(defun reverse-regexp-constant-floating macro (x)
       `(do ((backup (1- (stringlength (cdar reverse-regexp))))
	   (mark)
	   (string (cdar reverse-regexp)))
	  ((not (reverse-search string)) nil)
	  (setq mark (regexp:reverse-match reverse-regexp 0 nil))
	  (if mark
	      (exch-point-mark mark)
	      (do-times (1+ backup) (forward-char))
	      (exch-point-mark mark)
	      (return mark))
	  (do-times backup (forward-char))))

(defun regexp-constant-within-balance macro (x)
       `(do ((backup (1- (stringlength (cdar regexp))))
	   (cl curline)
	   (count)
	   (cpp curpointpos)
	   (mark)
	   (string (cdar regexp)))
	  ((not (setq count (forward-search-bounded string balance)))
	   (go-to-line-point cl cpp)
	   nil)
	  (setq mark (regexp:match regexp 0 nil))
	  (if mark
	      (exch-point-mark mark)
	      (do-times (1+ backup) (backward-char))
	      (exch-point-mark mark)
	      (return mark))
	  (setq balance (- balance count 1))
	  (do-times backup (backward-char))))


(defun reverse-regexp-constant-within-balance macro (x)
       `(do ((backup (1- (stringlength (cdar reverse-regexp))))
	   (cl curline)
	   (count)
	   (cpp curpointpos)
	   (mark)
	   (string (cdar reverse-regexp)))
	  ((not (setq count (reverse-search-bounded string balance)))
	   (go-to-line-point cl cpp)
	   nil)
	  (setq mark (regexp:reverse-match reverse-regexp 0 nil))
	  (if mark
	      (exch-point-mark mark)
	      (do-times (1+ backup) (forward-char))
	      (exch-point-mark mark)
	      (return mark))
	  (setq balance (- balance count 1))
	  (do-times backup (forward-char))))

(defun regexp-dots-floating macro (x)
       `(do ((count (cdar regexp))
	   (result))
	  ((or (if (not (> count (chars-left-in-line)))
		 (setq curpointpos (+ curpointpos count))
		 (setq result
		       (regexp:match regexp (chars-left-in-line) nil)))
	       (lastlinep))
	   result)
	  (next-line)))


(defun reverse-regexp-dots-floating macro (x)
       `(do ((count (cdar reverse-regexp))
	   (result))
	  ((or (if (not (> count curpointpos))
		 (setq curpointpos (- curpointpos count))
		 (setq result (regexp:reverse-match reverse-regexp
					      curpointpos nil)))
	       (firstlinep))
	   result)
	  (prev-line)))

(defun regexp-dots-anchored macro (x)
       `(let ((count (cdar regexp)))
	   (if (not (> count (chars-left-in-line)))
	       (let ((cl curline)
		   (cpp curpointpos)
		   (result))
		  (if (> count balance) (setq balance 0 star-mark nil)
		      else (setq balance (- balance count)))
		  (setq curpointpos (+ curpointpos count))
		  (setq result (regexp:match regexp balance star-mark))
		  (cond (result (exch-point-mark result)
			      (setq curpointpos cpp)
			      (exch-point-mark result))
		        (t (go-to-line-point cl cpp)))
		  result))))


(defun reverse-regexp-dots-anchored macro (x)
       `(let ((count (cdar reverse-regexp)))
	   (if (not (> count curpointpos))
	       (let ((cl curline)
		   (cpp curpointpos)
		   (result))
		  (if (> count balance) (setq balance 0 star-mark nil)
		      else (setq balance (- balance count)))
		  (setq curpointpos (- curpointpos count))
		  (setq result (regexp:reverse-match
			       reverse-regexp balance star-mark))
		  (cond (result (exch-point-mark result)
			      (setq curpointpos cpp)
			      (exch-point-mark result))
		        (t (go-to-line-point cl cpp)))
		  result))))

(defun regexp-star-floating macro (x)
       `(let ((char (cadr regexp))
	    (cl curline)
	    (cpp curpointpos)
	    (result (regexp:search regexp)))
	   (if result
	       (exch-point-mark result)
	       (cond (char
		     (do ()
		         ((and (eq cl curline) (= cpp curpointpos)))
		         (or (eq char (curchar))
			   (return nil))
		         (forward-char)))
		   ((eq cl curline)
		    (go-to-line-point cl cpp))
		   (t (go-to-beginning-of-line)))
	       (exch-point-mark result)
	       result))))


(defun reverse-regexp-star-floating macro (x)
       `(let ((char (cadr reverse-regexp))
	    (cl curline)
	    (cpp curpointpos)
	    (result (regexp:reverse-search reverse-regexp)))
	   (if result
	       (exch-point-mark result)
	       (cond (char
		     (do ()
		         ((and (eq cl curline) (= cpp curpointpos)))
		         (or (eq char (lefthand-char))
			   (return nil))
		         (backward-char)))
		   ((eq cl curline)
		    (go-to-line-point cl cpp))
		   (t (go-to-end-of-line)))
	       (exch-point-mark result)
	       result))))

(defun regexp-star-anchored macro (x)
       `(let ((char (cdar regexp))
	    (cl curline)
	    (cpp curpointpos)
	    (my-mark)
	    (result))
	   (cond (star-mark (setq my-mark star-mark)
			(exch-point-mark my-mark))
	         (t (setq my-mark (set-mark))))
	   (cond (char
		 (do ()
		     ((not (eq char (curchar))))
		     (forward-char)
		     (setq balance (1+ balance))))
	         (t (setq balance (+ balance (chars-left-in-line)))
		  (go-to-end-of-line)))
	   (exch-point-mark my-mark)
	   (setq result (regexp:match regexp balance my-mark))
	   (or star-mark (release-mark my-mark))
	   (cond (result (exch-point-mark result)
		       (go-to-line-point cl cpp)
		       (exch-point-mark result)))
	   result))


(defun reverse-regexp-star-anchored macro (x)
       `(let ((char (cdar reverse-regexp))
	    (cl curline)
	    (cpp curpointpos)
	    (my-mark)
	    (result))
	   (cond (star-mark (setq my-mark star-mark)
			(exch-point-mark my-mark))
	         (t (setq my-mark (set-mark))))
	   (cond (char
		 (do ()
		     ((not (eq char (lefthand-char))))
		     (backward-char)
		     (setq balance (1+ balance))))
	         (t (setq balance (+ balance curpointpos))
		  (go-to-beginning-of-line)))
	   (exch-point-mark my-mark)
	   (setq result (regexp:reverse-match reverse-regexp
				        balance my-mark))
	   (or star-mark (release-mark my-mark))
	   (cond (result (exch-point-mark result)
		       (go-to-line-point cl cpp)
		       (exch-point-mark result)))
	   result))

;;;
;;; The actual top-level recursive forward search routines.
;;;

(defun regexp:search (regexp)
       (setq regexp (cdr regexp))
       (cond ((null regexp) (set-mark))
	   ((eq (caar regexp) 'constant)
	    (regexp-constant-floating))
	   ((eq (caar regexp) 'dots)
	    (regexp-dots-floating))
	   ((eq (caar regexp) 'star)
	    (regexp-star-floating))
	   ((eq (caar regexp) 'ends-line)
	    (go-to-end-of-line)
	    (set-mark))
	   ;;
	   ;; if we get this far, (caar regexp) = 'begins-line.
	   ;;
	   ((and (lastlinep) (not (bolp))) nil)
	   (t (if (not (bolp)) (next-line))
	      (do ((result))
		((or (setq result (regexp:match regexp 0 nil))
		     (lastlinep))
		 result)
		(next-line)))))


(defun regexp:match (regexp balance star-mark)
       (setq regexp (cdr regexp))
       (cond ((null regexp)
	    (prog1 (set-mark)
		 (if star-mark (go-to-mark star-mark))))
	   ((eq (caar regexp) 'constant)
	    (regexp-constant-within-balance))
	   ((eq (caar regexp) 'dots)
	    (regexp-dots-anchored))
	   ((eq (caar regexp) 'star)
	    (regexp-star-anchored))
	   ((eq (caar regexp) 'ends-line)
	    (cond ((< balance (chars-left-in-line)) nil)
		(t (prog1 (set-mark)
			(go-to-end-of-line)))))
	   ((bolp)			; (caar regexp) = begins-line
	    (regexp:match regexp 0 nil))))

;;;
;;; The actual top-level recursive reverse search routines.
;;; Note that they closely parallel the forward regexp search, but the
;;; roles of begins-line (^) and ends-line ($) have been interchanged.
;;;

(defun regexp:reverse-search (reverse-regexp)
       (setq reverse-regexp (cdr reverse-regexp))
       (cond ((null reverse-regexp) (set-mark))
	   ((eq (caar reverse-regexp) 'constant)
	    (reverse-regexp-constant-floating))
	   ((eq (caar reverse-regexp) 'dots)
	    (reverse-regexp-dots-floating))
	   ((eq (caar reverse-regexp) 'star)
	    (reverse-regexp-star-floating))
	   ((eq (caar reverse-regexp) 'begins-line)
	    (go-to-beginning-of-line)
	    (set-mark))
	   ;;
	   ;; if we get this far, (caar reverse-regexp) = 'ends-line.
	   ;;
	   ((and (firstlinep) (not (eolp))) nil)
	   (t (if (not (eolp)) (prev-line) (go-to-end-of-line))
	      (do ((result))
		((or (setq result
			 (regexp:reverse-match reverse-regexp 0 nil))
		     (firstlinep))
		 result)
		(prev-line) (go-to-end-of-line)))))


(defun regexp:reverse-match (reverse-regexp balance star-mark)
       (setq reverse-regexp (cdr reverse-regexp))
       (cond ((null reverse-regexp)
	    (prog1 (set-mark)
		 (if star-mark (go-to-mark star-mark))))
	   ((eq (caar reverse-regexp) 'constant)
	    (reverse-regexp-constant-within-balance))
	   ((eq (caar reverse-regexp) 'dots)
	    (reverse-regexp-dots-anchored))
	   ((eq (caar reverse-regexp) 'star)
	    (reverse-regexp-star-anchored))
	   ((eq (caar reverse-regexp) 'begins-line)
	    (cond ((> balance curpointpos) nil)
		(t (prog1 (set-mark)
			(go-to-beginning-of-line)))))
	   ((eolp)		; (caar reverse-regexp = ends-line
	    (regexp:reverse-match reverse-regexp 0 nil))))
