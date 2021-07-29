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

;;; Multics EMACS
;;;     A product of Greenberg, 3/78
;;;     Started by BSG & BEE 3/3/78
;;;
;;;               EMACS's most basic functions
;;;               The sine qua non of Emacs' functionality, split away
;;;               June 1981 RMSoley.  This is the stuff that used to be
;;;               in emacs_ and some of what was in e_macops_.
     
;;;                BSG, WMY, GMP, RSL, RMSoley, and many others.


;;; HISTORY COMMENTS:
;;;  1) change(86-02-24,Margolin), approve(), audit(), install():
;;;     Pre-hcom journalization:
;;;     Modified: June 1982 - Barmar - to add JSL's new searching primitives.
;;;                   Also added more &undo's, and gave self-insert and &undo.
;;;     Modified: 2 November 1983 - Barmar - to add (backward forward)-n-chars
;;;                   and make (backward forward)-char use them.
;;;     Modified: 25 November 1983 - Barmar - to add JSL's .unh hack for paragraphs.
;;;     Modified: 29 November 1983 - Barmar - rename (forward backward)-char to
;;;                   ===-command and put back the old ===.  This is because the new
;;;                   commands use look at the value of numarg, which primitives should
;;;                   not do, and (forward backward)-char is a primitive.
;;;     Modified: 3 December 1983 - Barmar - to change add-new-line and
;;;                   delete-line to not change number-of-lines-in-buffer when
;;;                   in the minibuffer.
;;;     Modified: 19 January 1984 - Barmar - to comment out the register-option
;;;                   forms, as they were moved into e_option_defaults_.
;;;  2) change(86-02-24,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     More pre-hcom journalization:
;;;     Modified: 25 December 1984 - Barmar - to use defmacro, delete some macros
;;;                   that are also in e-macros.incl.lisp.
;;;     Modified: 26 December 1984 - Barmar - to not (zerop old-numarg) in
;;;                   autofill-self-insert, since old-numarg can be nil, to initialize
;;;                   current-command with defvar, move %include's to before most
;;;                   declares, fix save-buffer-state to not reference unbound local
;;;                   vars.
;;;     Modified: 27 December 1984 - Barmar - fix register-local-var to initialize
;;;                   local variables that were unbound, as documented, and remove the
;;;                   change to save-buffer-state.
;;;     Modified: 30 December 1984 - Barmar - change retrieve-buffer-state to
;;;                   maintain the Macro Learn minor mode, minibuffer-response to
;;;                   not change key bindings in recursive minibuffers, changes to
;;;                   fill-mode to fill on CR and TAB.
;;;     Modified: 6 January 1985 - Barmar - changed to use defstruct for eline
;;;                   and mark, move gratuitous-mark-setter into prologue for
;;;                   go-to-(beginning end)-of-buffer, changed del-mark-from-buffer
;;;                   to bind curline (release-mark needs this).
;;;     Modified: 27 January 1985 - Barmar - added some special declarations.
;;;     Modified: 3 February 1985 Barmar: took CR out of fill-mode-delimiters,
;;;                   and special-cased it in fill-mode/fill-mode-off; changed
;;;                   fill-mode-off to interact with speedtype better, like fill-mode
;;;                   does.
;;;  3) change(86-02-24,Margolin), approve(86-02-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added insert-new-line and insert-new-empty-line.  Added
;;;     buffer-file-dtcm to the buffer data structure in
;;;     buffer-in-nihilem-factus-est.  Fixed destroy-buffer-contents and
;;;     reinitialize-current-buffer to release buffer tempsegs.
;;;  4) change(88-01-11,Schroth), approve(88-02-29,MCR7852),
;;;     audit(88-06-06,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Update minibuffer-response for split screen displays.
;;;                                                      END HISTORY COMMENTS


;;;

(declare (genprefix /!e_basic_))

(%include e-macros)
(%include defmacro)
(declare (macros nil))
(%include emacs-internal-macros)
(%include defstruct)
(%include other_other)

(declare (*expr convert_status_code_ cur-screen-hpos emacs_search_file_caller_
	      e$get_temporary_seg e_lap_$compare_strings e_lap_$delete-chars
	      e_lap_$forward-search-string e_lap_$ggcharn e_lap_$insert-chars
	      e_lap_$make-dat-ol-black-magic-string
	      e_lap_$reverse-search-string e_lap_$rplacstring
	      e_lap_$rplacstring-offset e_lap_$rtrim e_lap_$string_length
	      e_lap_$tabscan e_lap_$tct e_lap_$tctr filerep-to-string
	      get_pdir_ go-to-screen-hpos hcs_$make_ptr
	      instate-local-key-bindings
	      redisplay-buffer-reinit-purge redisplay-leave-buffer
	      redisplay-purge-buffer release-temp-segments
	      revert-local-key-bindings speedtype speedtypeoff))
(declare (*lexpr display-error-noabort display-error display-com-error
	       report-error report-error-noabort
	       display-error-remark minibuffer-remark
	       display-com-error-noabort minibuffer-print
	       minibuffer-print-noclear ncline comout))

(declare (special

	 process-dir		;user's process directory
	 env-dir			;dir where system libraries live
	 lisp-system-dir		;place where Lisp lives
	 multesque-readtable	;for reading without dots, etc.
	 next-multics-argno
	 firstline		;first line of buffer
	 lastline
	 curline			;current line doublecons
	 fill-prefix		;the fill prefix
	 default-fill-column	;used to reset fill-column
	 hard-enforce-fill-column	;used in echo negotiation
	 fill-column
	 default-comment-column	;used to reset comment-column
	 comment-column
	 comment-prefix
	 tab-equivalent
	 curlinel			;# chars in current line
	 minibufferp		;in Minny Buffer.
	 minibuffer-prompt-string	;for redisplay
	 minibuffer-end-string	;Usually "<>"
	 numarg			;numeric argument to current function, or nil if none
	 last-input-char		;current char, for self-inserts
	 nobreak-functions		;functions that don't break echnego
	 fpathname		;filepath
	 curpointpos		;# of chars to left of cursor, this line
	 work-seg			;seg stringorum
	 building-buf		;rplacablestring for kills, &c
	 work-string		;string segii
	 last-curline		;redisplay var
	 curstuff			;string, filecons, or work-string
	 buffer-tempsegs		;list of temp segments for this buffer
	 buffer-uid		;unique ID of segment this buffer holds
	 known-buflist		;syms with buffer-state property
	 list-of-known-options	;options used by  command
	 target-screen-hpos		;vertical motion target for next/prev l
	 current-buffer		;symbol name of current buffer
	 buffer-modified-flag	;as it says
	 buffer-minor-modes		;dorian, phrygian, aeolian, etc.
	 buffer-locvars		;local values of user op vars
	 current-buffer-mode	;current major mode
	 number-of-lines-in-buffer
	 display-linechange-interrupt-hook
	 macro-execution-in-progress	;pointer on current xec list
	 macro-collection-in-progress	;if non-null, the current macro collection
	 per-buffer-key-bindings	;as it says, assq list
	 previous-buffer		;buffer we came from
	 previous-command		;last cmd symbol
	 read-only-flag		;per buffer
	 spec-op-name-flag		;for redisplay, query replace etc.
	 tty-no-upmotionp		;Tty can't move cursor up.
	 buf-state-template		;specials to save for bufswitch
	 NLCHARSTRING		;a newline as string object
	 TAB			;ascii TAB
	 ESC			;ascii escape
	 CRET			;carriage return symbol
	 NL
	 search-ring		;ring of strings searched for recently
	 last-search-string		;same value as top of search-ring
	 last-minibuf-response
	 kill-ring		;ring of killed stuff
	 kill-ring-current-size	;8/4/80
	 kill-ring-max-size
	 dont-stash		;flag to not store killage
	 der-wahrer-mark		;mark that user knows of
	 marklist			;list of marks
	 mark-ring		;ring of user marks
	 named-mark-list		;list of named marks
	 curline-marklist		;list of marks for current line
	 damaged-flag		;redisplay, watchout
	 dont-damage-flag		;open-line, don't set above
	 touchy-damaged-flag	;el gran kludge- see redisplay
	 pdir-temp-ename
	 pdir-temp-pathname		;file in process dir
	 suppress-redisplay-flag
	 rdis-suppress-redisplay
	 e-lisp-error-mode		;see e_interact_
	 e-quit-transparency	;see signalquit in e_interact_
	 good-word-charactertbl	;Character table for words.
	 whitespace-charactertbl	;Same for whitespace.
	 two-window-mode 
	 varlist
	 OPEN-PAREN
	 CLOSE-PAREN
	 SEMI
	 SINGLEQUOTE
	 DOUBLEQUOTE
	 SLASH
	 CR
	 sexp-searcher-mark-list
	 MCS-editing-characters
	 MCS-escape-character
	 line-control:template
	 line-control:buffer
	 ))

(defvar current-command nil)			;current cmd symbol

(defmacro consp (x) `(not (atom ,x)))

(defmacro firstlinep () '(null (prevline)))

(defmacro lastlinep () '(null (nextline)))

(defmacro curline-openp () '(eq curstuff work-string))

(defmacro note-modified-buffer ()
       '(and (or read-only-flag (not buffer-modified-flag))
	   (buffer-has-been-modified--take-note)))

(defmacro defkill (command type) `(defprop ,command ,type kills))

(defmacro hook-function (hook-name)
	`(and ,hook-name (funcall ,hook-name ',hook-name)))

(defmacro defhook (hook-name)
	`(progn 'compile
	        (declare (special ,hook-name))
	        (setq ,hook-name nil)))

(setq OPEN-PAREN '/(	CLOSE-PAREN '/)	SEMI '/;
      DOUBLEQUOTE '/"	SLASH '//		SINGLEQUOTE '/'
      sexp-searcher-mark-list nil minibuffer-end-string "<>")

;;;	The "eline" (editor-line) datatype is constructed with defstruct.
;;;	It currently contains three slots:
;;;	eline-contents: either a string, a filecons (see below), or the
;;;	symbol "deleted"
;;;	eline-previous, eline-next: either another eline or nil
;;;
;;;	A filecons is a defstruct with two slots:
;;;	filecons-pointer: a fixnum-encoded pointer to the starting character
;;;	of a line in a temp-seg into which e_pl1_ copied a file at read-in
;;;	time.  
;;;	filecons-length: the length of that line, including the mandatory
;;;	newline at the end.
;;;
;;;	The variable "curstuff" is bound to either the "eline-contents"
;;;	of the current line, in which case the current line is said to
;;;	be "closed", or to the rplacable string "work-string", in which
;;;	case the contents of work-string are the valid contents of the 
;;;	line, which is then said to be "open".

(defun firstlinep () (null (prevline)))

(defun lastlinep ()  (null (nextline)))

(defun next-line ()
       (or (lastlinep)(go-to-line-point (nextline) 0)))

(defun prev-line ()
       (or (firstlinep)(go-to-line-point (prevline) 0)))

(defun open-line ()
       (or dont-damage-flag (setq damaged-flag t))     ;redisplay, forget it
       (setq touchy-damaged-flag t)		;SOMEthing happened.
       (note-modified-buffer)			;this is a macro
       (cond ((curline-openp))	;done already
	   (t (compute-marks-for-this-line)
	      (e_lap_$rplacstring work-string curstuff curlinel 0 curlinel)
	      (setq curstuff work-string))))

(defhook close-line-hook)

(defun close-line ()
       (hook-function close-line-hook)
       (cond ((curline-openp)
	    (setq curline-marklist nil)
	    (setq curstuff (substr work-string 1))
	    (setf (eline-contents curline) curstuff))
	   (t (setq curstuff (eline-contents curline)))))

(defun go-to-line-point (line point)
       (and (eq (eline-contents line) 'deleted)
	  (error "Internal error: Transfer to deleted line." nil 'fail-act))
       (setq curstuff
	   (cond ((eq curline line) curstuff)
	         (t (close-line)
		  (setq curline line)
		  (eline-contents curline)))
	   curlinel
	   (cond ((consp curstuff) (filecons-length curstuff))
	         ((stringp curstuff)(stringlength curstuff))
	         (t (error "Internal error: go-to-line-point confused."
		         nil 'fail-act))))
       (cond ((> point (1- curlinel))(setq point (1- curlinel))))
       (setq curpointpos point))

(defun add-new-empty-line ()
       (add-new-line NLCHARSTRING))

;;; Adds a new line after the current line.  Doesn't move point.
(defun add-new-line (linerepresentation)
       (prog (l)
	   (note-modified-buffer)
	   (setq damaged-flag t)
	   (setq l (make-eline contents linerepresentation
			   previous curline
			   next (nextline)))
	   (cond ((lastlinep)(setq lastline l))
	         (t (setf (eline-previous (nextline)) l)))
	   (setf (nextline) l))
       (or minibufferp
	 (setq number-of-lines-in-buffer (1+ number-of-lines-in-buffer))))

;;; Like above, but inserts BEFORE the current line
(defun insert-new-line (linerep)
       (note-modified-buffer)
       (setq damaged-flag t)
       (let ((l (make-eline contents linerep
		        next curline
		        previous (prevline))))
	  (cond ((firstlinep) (setq firstline l))
	        (t (setf (eline-next (prevline)) l)))
	  (setf (prevline) l))
       (or minibufferp (incf number-of-lines-in-buffer)))

(defun insert-new-empty-line ()
       (insert-new-line NLCHARSTRING))

(defun delete-line ()			;kill curline
       (cond ((and (lastlinep)(firstlinep)))	;act like Millard Fillmore
					;(Do nothing)
	   (t (setq damaged-flag t buffer-modified-flag t)
	      (let ((prev (prevline))
		  (next  (nextline)))
		 (and prev (setf (eline-next prev) next))
		 (and next (setf (eline-previous next) prev))
		 (close-line)
		 (setf (eline-contents curline) 'deleted)    ;mark dead for redisplay
		 (and (lastlinep)(setq lastline prev))
		 (and (firstlinep)(setq firstline next))
		 (and next (go-to-line-point next curpointpos))
		 (or minibufferp
		     (setq number-of-lines-in-buffer
			 (1- number-of-lines-in-buffer)))))))

;;; Underpinnings of single-character movement, insertion, testing, deletion.

;;; Test lefthand-character.
(defun one-back-is-a (c)
   (if (stringp c)(setq c (getchar c 1)))
   (if (at-beginning-of-buffer) nil
       else (backward-char)
	  (if (at-beginning-of-buffer)
	      (forward-char)
	      nil
	      else (prog2 0 (eq (lefthand-char) c)(forward-char)))))

;;; Return the character to the left of the cursor.
(defun lefthand-char ()
     (if (bolp) NL
         else (prog2 (backward-char)
		 (curchar)
		 (forward-char))))

;;; Insert a character into the buffer.
(defun insert-char (char)
       (let ((dont-damage-flag (cond ((eq char NL) nil)
			       ((not (eq curline last-curline)) nil)
			       ((eolp) t)
			       (t nil))))
	  (open-line))
       (or (eolp)
	 (relocate-marks curline 1 'c+));hack to optimize only
       (e_lap_$insert-chars work-string curpointpos char 1)
       (setq curlinel (1+ curlinel) curpointpos (1+ curpointpos))
       (and (eq char NL)(break-the-line)))

(defun break-the-line ()			;called with line open
       (let ((oldstring (substr work-string 1))
	   (saved-clml curline-marklist)
	   (cpp curpointpos))
	  (close-line)
	  ;; Optimize breaking for redisplay's benefit.
	  (cond ((and (not (firstlinep))(< cpp (- curlinel cpp)))
	         (prev-line)
	         (add-new-line (substr oldstring 1 cpp))	;stick before not after
	         (next-line)		;the one we just added
	         (next-line)		;the original line
	         (setq curstuff (substr oldstring (1+ cpp)))
	         (setf (eline-contents curline) curstuff) ;this is obscure
	         (setq curlinel (- (stringlength oldstring) cpp))
	         (compute-marks-for-this-line)
	         (relocate-marks (prevline) cpp 'rev-brk)
	         (relocate-marks curline (- cpp) 'c+))
	        (t (setf (eline-contents curline) (substr oldstring 1 cpp))
		 (add-new-line (substr oldstring (1+ cpp)))
		 (setq curline-marklist saved-clml)
		 (relocate-marks (nextline) curpointpos 'break)
		 (next-line)))))

;;; Underpinnings of line movement, etc.

(defun kill-to-end-of-line ()
       (and (eq current-buffer line-control:buffer)
	  (line-control:check 'end))
       (open-line)
       (or dont-stash (killsave-string (substr work-string (1+ curpointpos)(- curlinel curpointpos 1))))
       (relocate-marks curline curpointpos 'set)
       (e_lap_$rplacstring work-string NL 1 curpointpos (1+ curpointpos))
       (setq curlinel (1+ curpointpos)))


(defun merge-line ()
       (note-modified-buffer)
       (and (eq current-buffer line-control:buffer)
	  (line-control:check 0))
       (setq damaged-flag t)
       (let ((clml curline-marklist)
	   (openp (curline-openp)))
	  (close-line)
	  (cond ((lastlinep))
	        (t (let ((origcurl curline)
		       (origcpp curpointpos)
		       (thiss curstuff)
		       (thissl (1- curlinel)))
		      (cond ((or (= 0 thissl)
			       (> (let ((nextstuff
				        (eline-contents (nextline))))
				     (cond ((consp nextstuff)
					  (filecons-length nextstuff))
					 (t (e_lap_$string_length nextstuff))))
				curlinel))
			   (cond  ((> thissl 0)
				 (next-line)
				 (compute-marks-for-this-line)
				 (relocate-marks curline thissl '+)
				 (go-to-line-point origcurl origcpp)))
			   (cond (openp (setq curline-marklist clml))
			         (t (compute-marks-for-this-line)))
			   (relocate-marks (nextline) 0 '+)
			   (delete-line)
			   (cond ((> thissl 0)
				(setf (eline-contents curline)
				      (setq curstuff
					  (progn
					    (e_lap_$rplacstring work-string thiss thissl 0 thissl)
					    (e_lap_$rplacstring work-string curstuff curlinel thissl (+ thissl curlinel))
					    (substr work-string 1))))))
			   (setq origcurl curline))
			  (t (next-line)
			     (setf (eline-contents origcurl)
				 (progn
				   (e_lap_$rplacstring work-string thiss thissl 0 thissl)
				   (e_lap_$rplacstring work-string curstuff curlinel thissl (+ thissl curlinel))
				   (substr work-string 1)))
			     (compute-marks-for-this-line)
			     (relocate-marks origcurl thissl '+)
			     (delete-line)))
		      (go-to-line-point origcurl origcpp))))))

;;;
;;;	The "mark" datum.
;;;
;;;	Redone 7/3/79 by BSG for curline-marklist.
;;;	8/19/79 by BSG for named marks and mark-ring.
;;;
;;;	The format of a mark is (defstruct (mark (:conc-name))
;;;				     eline position)
;;;	"marklist" lists all marks in the current buffer.
;;;	In addition, curline-marklist lists all marks for
;;;	current line (curline) if and only if it is open.
;;;	Only marks for the current line ever have to be relocated.

(defun compute-marks-for-this-line ()
       (setq curline-marklist
	   (mapcan '(lambda (m)
			(and (eq (mark-eline m) curline)
			     (ncons m)))
		 marklist)))

(defun set-mark ()
       (let ((mark (make-mark eline curline
			position curpointpos)))
	  (push mark marklist)
	  (and (curline-openp)
	       (push mark curline-marklist))
	  mark))

(defun move-mark (to from)
       (and (eq (mark-eline to) curline)
	  (not (eq (mark-eline from)  curline))
	  (setq curline-marklist (delq to curline-marklist)))
       (alter-mark to
	         eline (mark-eline from)
	         position (mark-position from)))

(defun set-mark-here (m)
       (and (not (eq (mark-eline m) curline))
	  (curline-openp)
	  (push m curline-marklist))
       (alter-mark m
	         eline curline
	         position curpointpos))

(defun go-to-mark (m)
       (go-to-line-point (mark-eline m) (mark-position m)))

(defun release-mark (m)
       (and (eq curline (mark-eline m))
	  (setq curline-marklist (delq m curline-marklist 1)))
       (setq marklist (delq m marklist 1)))

(defun mark-reached (m)
       (cond ((eq (mark-eline m) curline)
	    (not (< curpointpos (mark-position m))))
	   ((lastlinep))			;KLUDGE
	   (t nil)))

(defun mark-same-line-p (m1 m2)
       (eq (mark-eline m1) (mark-eline m2)))

(defun mark-equal (m1 m2)
       (and (eq (mark-eline m1) (mark-eline m2))
	  (= (mark-position m1) (mark-position m2))))

(defun mark-on-current-line-p (m)
       (eq curline (mark-eline m)))

(defun mark-at-current-point-p (m)
       (and (eq (mark-eline m) curline)
	  (= (mark-position m) curpointpos)))

(defun mark-reached-backwards (m)
       (cond ((eq (mark-eline m) curline)
	    (not (> curpointpos (mark-position m))))
	   ((firstlinep))			;KLUDGE
	   (t nil)))

(defun relocate-marks (newline offset flag)	;oldline is always curline
       (mapc '(lambda (m)
		  (cond ((eq flag '+)
		         (alter-mark m
				 eline newline
				 position (+ (mark-position m) offset)))
		        ((eq flag 'set)
		         (cond ((> (mark-position m) offset)
			      (setf (mark-position m) offset))))
		        ((eq flag 'break)
		         (cond ((< curpointpos (mark-position m))
			      (alter-mark m
				        eline newline
				        position (- (mark-position m) offset)))))
		        ((eq flag 'c+)
		         (cond ((> (mark-position m) curpointpos)
			      (setf (mark-position m)
				  (max curpointpos
				       (+ offset (mark-position m)))))))
		        ((eq flag 'rev-brk)
		         (cond ((< (mark-position m) offset)
			      (setf (mark-eline m) newline))))))
	   curline-marklist)
       (or (eq curline newline)
	 (setq curline-marklist
	       (mapcan '(lambda (m)
			    (and (eq (mark-eline m) curline)
			         (ncons m)))
		     curline-marklist))))

(defun kill-forward-to-mark (m)
       (and (eq line-control:buffer current-buffer)
	  (line-control:check m))
       (open-line)
       (or dont-stash (killsave-string (point-mark-to-string m)))
       (do ((dont-stash t))((mark-reached m))
	 (cond ((not (eq (mark-eline m) curline))
	        (kill-to-end-of-line)
	        (merge-line))
	       (t (let ((howmany (- (mark-position m) curpointpos)))
		     (open-line)
		     (relocate-marks curline (- howmany) 'c+)
		     (e_lap_$delete-chars work-string curpointpos howmany)
		     (setq curlinel (- curlinel howmany)))
		(return t)))))

(defun point-mark-to-string (m)
       (cond ((point>markp m)
	    (let ((val))
	         (unwind-protect
		 (setq val (progn (exch-point-mark m)
			        (point-mark-to-string1 m)))
		 (exch-point-mark m))
	         val))
	   (t (point-mark-to-string1 m))))

(defun point-mark-to-string1 (m)
       (e_lap_$rplacstring building-buf "" 0 0 0)	;clean slate
       (with-mark savem
	        (do nil (nil)
		  (let ((limit (cond ((eq (mark-eline m) curline)
				  (mark-position m))
				 (t curlinel))))
		       (let ((curl (stringlength building-buf))
			   (addl (- limit curpointpos)))
			  (e_lap_$rplacstring-offset
			    building-buf curstuff addl curl
			    (+ curl addl) curpointpos)))
		  (cond ((eq (mark-eline m) curline)
		         (go-to-mark savem)
		         (return (substr building-buf 1))))
		  (go-to-line-point (nextline) 0) )))

(defun in-line-compare-string (str m)
       (cond ((consp m)(setq m (mark-position m))))
       (cond ((not (= (abs (- curpointpos m))(stringlength str)))
	    nil)
	   ((> curpointpos m)
	    (e_lap_$compare_strings curstuff m str 0 (- curpointpos m)))
	   (t (e_lap_$compare_strings
	        curstuff curpointpos str 0 (- m curpointpos)))))

(defun looking-at (string)
       (let ((leftl (- curlinel curpointpos))
	   (sl (stringlength string)))
	  (cond ((> sl leftl) nil)
	        ((= 0 sl) t)
	        (t (e_lap_$compare_strings
		   curstuff curpointpos string 0 sl)))))

(defun kill-backwards-to-mark (m)
       (let ((old-line curline)
	   (old-pos curpointpos))
	  (protect
	    (exch-point-mark m)
	    (kill-forward-to-mark m)
	    &failure
	    (or (and (eq old-line curline)	;only if we've made it thru
		   (= old-pos curpointpos))	;the first exchange
	        (exch-point-mark m)))))

(defcom exchange-point-and-mark
        &undo &ignore
        (cond (der-wahrer-mark (exch-point-mark der-wahrer-mark))
	    (t (report-error 'mark-not-set))))

(defun exch-point-mark (m)
       (let ((line (mark-eline m))
	   (hpos (mark-position m)))
	  (set-mark-here m)
	  (go-to-line-point line hpos)))

(defun order-mark-last (m)			;makes mark follow point
       (and (point>markp m)(exch-point-mark m)))

(defun point>markp (m)
       (cond ((eq (eline-contents (mark-eline m)) 'deleted)
	    (error "Internal error: point>markp found mark in deleted line."
		 current-buffer 'fail-act))
	   ((eq curline (mark-eline m))
	    (> curpointpos (mark-position m)))
	   (t (do ((mp (mark-eline m)
		     (eline-next mp))
		 (pp curline (eline-next pp)))
		((and (not pp)(not mp))
		 (error "Internal error: point>markp can't find mark."
		        current-buffer 'fail-act))
		(cond ((eq pp (mark-eline m))(return nil))
		      ((eq mp curline)(return t)))))))

(defun set-the-mark ()
       (cond ((and der-wahrer-mark (mark-at-current-point-p der-wahrer-mark)))
	   (t (and der-wahrer-mark
		 (not minibufferp)
		 (push-mark-ring der-wahrer-mark))
	      (release-mark der-wahrer-mark)
	      (setq der-wahrer-mark (set-mark)))))

(defun set-the-mark-here (where)
       (cond ((and der-wahrer-mark (mark-equal der-wahrer-mark where)))
	   (t (and der-wahrer-mark
		 (not minibufferp)
		 (push-mark-ring der-wahrer-mark))
	      (release-mark der-wahrer-mark)
	      (setq der-wahrer-mark where))))

(defcom set-or-pop-the-mark
        &numeric-argument (&pass)
        (cond ((null numarg)
	     (set-the-mark)
	     (or tty-no-upmotionp (minibuffer-remark "Set.")))
	    (minibufferp (ring-tty-bell))
	    (t (do x mark-ring (cadr x) nil
		 (cond ((and (car x) (mark-at-current-point-p (car x)))
		        (return nil))
		       ((eq (cadr x) mark-ring)
		        (let ((r mark-ring) (m (set-mark)))
			   (push-mark-ring m)
			   (release-mark m)
			   (setq mark-ring r))
		        (return nil))))
	       (do x mark-ring (cddr x) nil
		 (cond ((car x)(return (setq mark-ring x)))
		       ((eq (cddr x) mark-ring) (return nil))))
	       (cond ((car mark-ring)
		    (go-to-mark (car mark-ring))
		    (setq mark-ring (cddr mark-ring)))))))

(defun push-mark-ring (markval)
       (cond ((and (car mark-ring)
	         (mark-equal (car mark-ring) markval)))
	   (t (setq mark-ring (cadr mark-ring))
	      (cond ((not (car mark-ring))
		   (rplaca mark-ring (set-mark))))
	      (move-mark (car mark-ring) markval))))
	    
(defun wipe-point-mark (m)
       (let ((old-line curline)
	   (old-pos curpointpos))
	  (protect
	    (and (point>markp m)
	         (exch-point-mark m))
	    (kill-forward-to-mark m)
	    &failure
	    (or (and (eq old-line curline)	;only if we made
		   (= old-pos curpointpos))	;the first exchange
	        (exch-point-mark m)))))

;;; EIS Search and Verify: The searching and character set primitives.
;;;   Hirsute multiline search inserted 5/8/80 by BSG
;;; June 1982 - JSL - Reimplemented and/or reformatted most of this.
;;;		  Added bounded searches.

(defun forward-search-in-line (string)
       (let ((cpp)
	   (result
	     (e_lap_$forward-search-string curstuff curpointpos string)))
	  (cond ((< result 0) nil)
	        ((> curlinel
		  (setq cpp (+ result curpointpos (stringlength string))))
	         (setq curpointpos cpp)
	         result)
	        (t nil))))


(defun forward-search (string)
       (let ((cl curline)
	   (cpp curpointpos)
	   (forever 100000000.)
	   (nl1x (e_lap_$forward-search-string string 0 NL)))
	  (let ((result (cond ((< nl1x 0)
			   (search:forward-nnl string forever))
			  (t (search:forward-nl string nl1x forever)))))
	       (or result (go-to-line-point cl cpp))
	       result)))


(defun forward-search-bounded (string bound)
       (let ((cl curline)
	   (cpp curpointpos)
	   (nl1x (e_lap_$forward-search-string string 0 NL)))
	  (let ((result (cond ((< nl1x 0)
			   (search:forward-nnl string bound))
			  (t (search:forward-nl string nl1x bound)))))
	       (or result (go-to-line-point cl cpp))
	       result)))

(defun reverse-search-in-line (string)
       (let ((result
	     (e_lap_$reverse-search-string curstuff curpointpos string)))
	  (cond ((< result 0) nil)
	        (t (setq curpointpos
		       (- curpointpos result (stringlength string)))
		 result))))


(defun reverse-search (string)
       (let ((cl curline)
	   (cpp curpointpos)
	   (forever 100000000.)
	   (nl1x (e_lap_$forward-search-string string 0 NL)))
	  (let ((result (cond ((< nl1x 0)
			   (search:reverse-nnl string forever))
			  (t (search:reverse-nl string nl1x forever)))))
	       (or result (go-to-line-point cl cpp))
	       result)))


(defun reverse-search-bounded (string bound)
       (let ((cl curline)
	   (cpp curpointpos)
	   (nl1x (e_lap_$forward-search-string string 0 NL)))
	  (let ((result
		(cond ((< nl1x 0)
		       (search:reverse-nnl string bound))
		      (t (search:reverse-nl string nl1x bound)))))
	       (or result (go-to-line-point cl cpp))
	       result)))

(defun search:forward-nnl (s bound)
       (do ((count 0)
	  (result (e_lap_$forward-search-string curstuff curpointpos s)
		(e_lap_$forward-search-string curstuff curpointpos s)))
	 ((not (< result 0))
	  (cond ((not (> (setq count (+ count result)) bound))
	         (setq curpointpos (+ curpointpos result (stringlength s)))
	         count)))
	 (and (lastlinep) (return nil))
	 (setq count (+ count curlinel))
	 (and (> count bound) (return nil))
	 (next-line)))


(defun search:forward-nl (lines nlx bound)
       (do ((cl curline curline)
	  (count 0)
	  (cpp curpointpos curpointpos)
	  (lines (cdr (search:break-and-save-string lines nlx)))
	  (result))
	 ((do ((l lines (cdr l))
	       (sl)
	       (start)
	       (string))
	      ((null l)
	       (cond ((< (setq start (+ start sl)) curlinel)
		    (setq curpointpos start))
		   ((lastlinep) nil)
		   (t (next-line) t)))
	      (setq sl (e_lap_$string_length (car l))
		  string (car l))
	      (cond ((eq l lines)
		   (setq start (- curlinel sl))
		   (and (< start 0) (return nil))
		   (setq result start))
		  ((lastlinep) (return nil))
		  (t (next-line)
		     (and (> sl curlinel) (return nil))
		     (setq start 0)))
	      (or (e_lap_$compare_strings curstuff start string 0 sl)
		(return nil)))
	  (and (not (> (setq result (+ count result)) bound))
	       result))
	 (go-to-line-point cl cpp)
	 (and (lastlinep) (return nil))
	 (setq count (+ count curlinel))
	 (and (> count bound) (return nil))
	 (next-line)))

(defun search:reverse-nnl (s bound)
       (do ((count 0)
	  (result (e_lap_$reverse-search-string curstuff curpointpos s)
		(e_lap_$reverse-search-string curstuff curpointpos s)))
	 ((not (< result 0))
	  (cond ((not (> (setq count (+ count result)) bound))
	         (setq curpointpos (- curpointpos result (stringlength s)))
	         count)))
	 (and (firstlinep) (return nil))
	 (setq count (+ count curpointpos 1))
	 (and (> count bound) (return nil))
	 (prev-line)
	 (go-to-end-of-line)))


(defun search:reverse-nl (lines nlx bound)
       (do ((cl curline curline)
	  (count 0)
	  (cpp curpointpos curpointpos)
	  (initial-cl curline)
	  (result)
	  (lines (cdr (search:reverse
		      (search:break-and-save-string lines nlx)))))
	 ((do ((l lines (cdr l))
	       (sl)
	       (start 0)
	       (string))
	      ((null l)
	       (setq curpointpos start))
	      (setq sl (e_lap_$string_length (car l))
		  string (car l))
	      (cond ((eq l lines)
		   (cond ((eq initial-cl curline)
			(or (cdr l) (return nil))
			(setq result (- curpointpos sl))
			(and (< result 0) (return nil)))
		         ((null (cdr l))
			(setq start (- curlinel sl) result 0)
			(and (< start 0) (return nil)))
		         (t (setq result (- curlinel sl))
			  (and (< result 0) (return nil)))))
		  ((firstlinep) (return nil))
		  ((null (cdr l))
		   (prev-line)
		   (setq start (- curlinel sl))
		   (and (< start 0) (return nil)))
		  (t (prev-line)
		     (or (= curlinel sl) (return nil))))
	      (or (e_lap_$compare_strings curstuff start string 0 sl)
		(return nil)))
	  (and (not (> (setq result (+ result count)) bound))
	       result))
	 (go-to-line-point cl cpp)
	 (and (firstlinep) (return nil))
	 (cond ((eq cl curline)
	        (setq count (+ count curpointpos)))
	       (t (setq count (+ count curlinel))))
	 (and (> count bound) (return nil))
	 (prev-line)))

(declare (special search:saved-broken-string))

(setq search:saved-broken-string nil)

(defun search:break-and-save-string (s nlx)
       (cond ((and search:saved-broken-string
	        (samepnamep s (caar search:saved-broken-string)))
	    search:saved-broken-string)
	   (t (setq search:saved-broken-string
		  (search:break-string s nlx)))))


(defun search:reverse (broken-string)
       (cond ((cdar broken-string) (car broken-string))
	   (t (rplacd (car broken-string) (reverse (cdr broken-string))))))


(defun search:break-string (string nlx)
       (do ((line)
	  (line-length)
	  (line-list (ncons (substr string 1 (1+ nlx))))
	  (nlx (1+ nlx))
	  (sl (stringlength string)))
	 ((not (< nlx sl))
	   (cons (ncons string) (nreverse line-list)))
	 (setq line-length (1+ (e_lap_$forward-search-string string nlx NL)))
	 (and (= line-length 0) (setq line-length (- sl nlx)))
	 (setq line (substr string (1+ nlx) line-length))
	 (setq line-list (cons line line-list))
	 (setq nlx (+ nlx line-length))))

;;; Old functions for searching for strings containing newlines.
;;; Obsoleted by JSL's new stuff (above) but left in just in case
;;; someone is using them (although they are internal interfaces).

(defun forward-search-multi (s nlx)
       (let ((ol (chunkify-string s nlx)))
	  (do ()(nil)
	      (let ((ocp curpointpos)
		  (ocl curline))
		 (and (do ((l ol (cdr l)))(nil)
			(let ((cl (e_lap_$string_length (car l)))
			      (chunk (car l))
			      (remains (- curlinel curpointpos 1)))
			     (cond ((> cl remains)(return nil)))
			     (and (eq l ol) ;first chunk
				(setq curpointpos (- curlinel cl 1) remains cl))
			     (cond ((null (cdr l))	;last chunk
				  (cond ((= cl 0)	;right after NL
				         (return (bolp)))
				        ((e_lap_$compare_strings curstuff curpointpos chunk 0 cl)
				         (return (setq curpointpos (+ curpointpos cl))))
				        (t (return nil))))
				 ((not (= cl remains))(return nil))
				 ((not (e_lap_$compare_strings curstuff curpointpos chunk 0 cl))
				  (return nil)))
			     (cond ((lastlinep)(return nil)))
			     (next-line)))
		      (return t))		;inner do won.
		 (go-to-line-point ocl ocp)
		 (cond ((lastlinep)(return nil)))
		 (next-line)))))

(defun reverse-search-multi (s nlx)
       (let ((ol (reverse (chunkify-string s nlx))))   ;cant nreverse or sharing loses
	  (do ()(nil)
	      (let ((ocp curpointpos)
		  (ocl curline))
		 (and (do ((l ol (cdr l)))(nil)
			(let ((cl (e_lap_$string_length (car l)))
			      (chunk (car l)))
			     (cond ((> cl curpointpos)(return nil)))
			     (and (eq l ol)
				(setq curpointpos cl))
			     (cond ((null (cdr l))	;last chunk
				  (cond ((= cl 0)
				         (return (eolp)))
				        ((e_lap_$compare_strings curstuff (- curlinel cl 1) chunk 0 cl)
				         (return (setq curpointpos (- curlinel cl 1))))
				        (t (return nil))))
				 ((not (= cl curpointpos))(return nil))
				 ((not (e_lap_$compare_strings curstuff 0 chunk 0 cl))
				  (return nil)))
			     (cond ((firstlinep)(return nil)))
			     (prev-line)
			     (go-to-end-of-line)))
		      (return t))
		 (go-to-line-point ocl ocp)
		 (cond ((firstlinep)(return nil)))
		 (prev-line)
		 (go-to-end-of-line)))))


(declare (special ss-chunkify-last-in ss-chunkify-last-out
	        ss-chunkify-generate-meter))

(setq ss-chunkify-last-in ""
      ss-chunkify-last-out nil
      ss-chunkify-generate-meter 0)

(defun chunkify-string (s nlx)
       (cond ((eq s ss-chunkify-last-in) ss-chunkify-last-out)
	   (t (let ((l (ncons (substr s 1 nlx)))
		  (sl (stringlength s)))
		 (setq nlx (1+ nlx)
		       ss-chunkify-generate-meter
		       (1+ ss-chunkify-generate-meter))
		 (do ((chunk)(x))
		     ((null sl)
		      (setq ss-chunkify-last-in s
			  ss-chunkify-last-out (nreverse l)))
		     (setq x (e_lap_$forward-search-string s nlx NL))
		     (cond ((< x 0)
			  (setq chunk (substr s (1+ nlx)))
			  (setq ss-chunkify-generate-meter
			        (1+ ss-chunkify-generate-meter))
			  (setq sl nil))
			 (t (setq chunk (substr s (1+ nlx) x))
			    (setq nlx (+ nlx x 1))))
		     (setq l (cons chunk l)))))))

(defun search-for-first-not-charset-line (tbl)	;move point, return t/nil
       (prog (r)
	   (setq r (e_lap_$tct curpointpos (cdr tbl) curstuff))
	   (and (< (setq r (+ curpointpos r)) curlinel)
	        (return (setq curpointpos r)))))

(defun search-for-first-charset-line (tbl)	;move point, return t/nil
       (prog (r)
	   (setq r (e_lap_$tct curpointpos (car tbl) curstuff))
	   (and (< (setq r (+ curpointpos r)) curlinel)
	        (return (setq curpointpos r)))))

(defun search-back-first-not-charset-line (tbl)	;move point, return t/nil
       (prog (r)
	   (setq r (e_lap_$tctr curpointpos (cdr tbl) curstuff))
	   (cond ((> (setq r (- curpointpos r)) 0)
		(return (setq curpointpos r)))
	         ((not (zerop (boole 1
			         000000001000
			         (arraycall fixnum (cdr tbl) 2))))
		(return (setq curpointpos 0))))))

(defun search-back-first-charset-line (tbl)	;move point, return t/nil
       (prog (r)
	   (setq r (e_lap_$tctr curpointpos (car tbl) curstuff))
	   (cond ((> (setq r (- curpointpos r)) 0)
		(return (setq curpointpos r)))
	         ((not (zerop (boole 1
			         000000001000
			         (arraycall fixnum (car tbl) 2))))
		(return (setq curpointpos 0))))))

(defun charscan-table (chars)
       (let ((haves (*array (gensym) 'fixnum 128.))
	   (havenots (*array (gensym) 'fixnum 128.)))
	  (fillarray haves '(0))
	  (fillarray havenots '(0))
	  (do  ((i 1 (1+ i))
	        (chn))
	       ((> i (stringlength chars)))
	       (setq chn (getcharn chars i))
	       (store (arraycall fixnum haves (// chn 4))
		    (boole 7.	; Use this instead of "+" to avoid
				; problems when a character appears
				; in "chars" > once.
			 (arraycall fixnum haves (// chn 4))
			 (lsh 777 (* 9. (- 3 (\ chn 4)))))))
	  (do i 0 (1+ i)(= i 128.)
	      (store (arraycall fixnum havenots i)
		   (boole 6 -1 (arraycall fixnum haves i))))
	  (cons haves havenots)))

(defun charset-member (c charset)
       (or (fixp c) (setq c (getcharn c 1)))
       (not (zerop (boole 1 777 (lsh (arraycall fixnum (car charset) (// c 4))
			       (* 9. (- (\ c 4) 3)))))))

;;;	Regular Expressions `a la "QEDX"

(defun forward-regexp-search (s)
       (prog (result ix len origpos)
	   (setq origpos (set-mark))
lhoop 	   (cond ((consp curstuff)
		(setq result
		      (emacs_search_file_caller_
		        s curpointpos (car curstuff) (cdr curstuff) "")))
	         (t (setq result
		        (emacs_search_file_caller_
			s curpointpos 7777000001 0 curstuff))))
	   (setq s "")			;makes it better, I'm told.
	   (setq ix (lsh result -22) len (boole 1 result 777777))
	   (cond ((not (= ix 777777))		;found something
		(do nil ((= ix curpointpos))(forward-char))
		(release-mark origpos)
		(setq origpos (set-mark))
		(do x 1 (1+ x)(> x len) (forward-char))
		(return origpos)))
	   (cond ((lastlinep)
		(go-to-mark origpos)
		(release-mark origpos)
		(return nil)))
	   (next-line)
	   (go lhoop)))

(defun forward-regexp-search-in-line (s)
       (prog (result ix len startpos)
	   (cond ((consp curstuff)
		(setq result
		      (emacs_search_file_caller_
		        s curpointpos (car curstuff) (cdr curstuff) "")))
	         (t (setq result
		        (emacs_search_file_caller_
			s curpointpos 7777000001 0 curstuff))))
	   (setq ix (lsh result -22) len (boole 1 result 777777))
	   (cond ((not (= ix 777777))		;found something
		(do nil ((= ix curpointpos))(forward-char))
		(setq startpos (set-mark))
		(do x 1 (1+ x)(> x len) (forward-char))
		(return startpos)))
	   (return nil)))

;;; Character set searchers.
(defprop search-charset-forwards search-charset-forward expr)
(defun search-charset-forward (charset)
       (with-mark here
	        (do-forever
		(cond ((search-for-first-charset-line charset)
		       (return (curchar)))
		      ((lastlinep)
		       (go-to-mark here)
		       (return nil))
		      (t (next-line))))))

(defprop search-not-charset-forwards search-not-charset-forward expr)
(defun search-not-charset-forward (charset)
       (with-mark here
	        (do-forever
		(cond ((search-for-first-not-charset-line charset)
		       (return (curchar)))
		      ((lastlinep)
		       (go-to-mark here)
		       (return nil))
		      (t (next-line))))))

(defprop search-charset-backward search-charset-backwards expr)
(defun search-charset-backwards (charset)
       (with-mark here
	        (do-forever
		(cond ((search-back-first-charset-line charset)
		       (return (prog2 (backward-char)
				  (curchar)
				  (forward-char))))
		      ((firstlinep)
		       (go-to-mark here)
		       (return nil))
		      (t (prev-line)
		         (go-to-end-of-line))))))

(defprop search-not-charset-backward search-not-charset-backwards expr)
(defun search-not-charset-backwards (charset)
       (with-mark here
	        (do-forever
		(cond ((search-back-first-not-charset-line charset)
		       (return (prog2 (backward-char)
				  (curchar)
				  (forward-char))))
		      ((firstlinep)
		       (go-to-mark here)
		       (return nil))
		      (t (prev-line)
		         (go-to-end-of-line))))))

;;;
;;;	Whitespace hackers.
;;;	Consolidated and upgraded 9/11/78 by archy.
;;;

(mapc '(lambda (x)(putprop x t 'whiteness))(list SPACE TAB NL FF VT CRET))
(setq SPACES "                                                                                                                        ")

(setq whitespace-charactertbl
      (charscan-table (catenate SPACE TAB NL FF VT CRET)))

(defun line-is-blank ()
       (or (and (bolp)(eolp))
	 (save-excursion
	  (go-to-beginning-of-line)
	  (not (search-for-first-not-charset-line
	         whitespace-charactertbl)))))

(defun skip-over-whitespace ()
       (do-forever
        (if (search-for-first-not-charset-line whitespace-charactertbl)
	  (stop-doing))
        (if (lastlinep)
	  (go-to-end-of-line)
	  (stop-doing))
        (next-line)))

(defun skip-over-whitespace-in-line ()
       (if (search-for-first-not-charset-line whitespace-charactertbl)
	 else (go-to-end-of-line)))

(defun skip-back-whitespace-in-line ()
       (if (search-back-first-not-charset-line whitespace-charactertbl)
	 else (go-to-beginning-of-line)))

(defun skip-to-whitespace-in-line ()
   (if (search-for-first-charset-line whitespace-charactertbl)
       else (go-to-end-of-line)))

(defun skip-to-whitespace ()
       (do-forever
        (if (search-for-first-charset-line whitespace-charactertbl)
	  (stop-doing))
        (next-line)))			; No lastlinep check needed

(defun skip-back-whitespace ()
       (do-forever
        (if (search-back-first-not-charset-line whitespace-charactertbl)
	  (stop-doing))
        (if (firstlinep)
	  (go-to-beginning-of-line)
	  (stop-doing))
        (prev-line)
        (go-to-end-of-line)))

(defun skip-back-to-whitespace ()
       (do-forever
        (if (search-back-first-charset-line whitespace-charactertbl)
	  (stop-doing))
        (if (firstlinep)
	  (stop-doing))
        (prev-line)
        (go-to-end-of-line)))

(defcom delete-white-sides
        (do-forever
	(if (eolp)(stop-doing))
	(if (at-white-char)(delete-char)
	    else (stop-doing)))
        (do-forever
	(if (bolp)(stop-doing))
	(backward-char)
	(if (at-white-char)
	    (delete-char)
	    else (forward-char)
	    (stop-doing))))

;;;
;;;	Paragraph routines.
;;;	Redone totally by BSG & WMY 7/21/78
;;;

(declare (special paragraph-definition-type))
;;;(register-option 'paragraph-definition-type 1) ;;; moved to e_option_defaults_

(defun at-beginning-of-paragraph ()
       (establish-local-var 'paragraph-definition-type 1)
       (cond ((not (bolp)) nil)
	   ;; Clearly now at beginning of line,
	   ((or (eolp) (line-is-blank)) nil)
	   ;; Optimization for null line.
	   ((looking-at ".") t)
	   ;; Formatter control lines are special paragraphs,
	   ((firstlinep) t)
	   ;; First line of file is first line of paragraph.
	   ((and (= paragraph-definition-type 2)
	         (at-white-char))
	    t)
	   ;; An indented line begins a type 2 paragraph.
	   (t (save-excursion
	        (prev-line)
	        (cond ((or (looking-at ".") (line-is-blank)) t)
		    ;; Previous line blank or control like top of file.
		    ((firstlinep) nil)
		    ;; Treat hanging undent lines like control lines.
		    (t (progn (prev-line)
			    (looking-at ".unh"))))))))


(defun at-end-of-paragraph ()
       (establish-local-var 'paragraph-definition-type 1)
       (cond ((not (eolp)) nil)
	   ;; Clearly now at end of line.
	   ((or (bolp) (line-is-blank)) nil)
	   ;; Optimization for null line.
	   ((lastlinep) t)
	   ;; Last line ends paragraph.
	   ((save-excursion (go-to-beginning-of-line)
			(looking-at "."))
	    t)
	   ;; Control line is special paragraph.
	   ((and (not (firstlinep))
	         (save-excursion (prev-line)
			     (looking-at ".unh")))
	    t)
	   ;; Treat hanging undents like control lines.
	   (t (save-excursion
	        (next-line)
	        (cond ((or (looking-at ".") (line-is-blank)) t)
		    ;; Following control or blank line like end of file.
		    ((= paragraph-definition-type 2)
		     (at-white-char))
		    ;; Indentation begins new type 2 paragraph, ends this.
		    (t nil))))))

(defcom beginning-of-paragraph
        &numeric-argument (&repeat)
        &negative-function end-of-paragraph
        (if (at-beginning-of-paragraph)
	  (skip-back-whitespace)
	  (if (at-beginning-of-buffer)(command-quit)))
        (go-to-beginning-of-line)
        (do-forever
	(if (at-beginning-of-paragraph)(stop-doing))
	(if (firstlinep)(command-quit))
	(prev-line)))

(defcom end-of-paragraph
        &numeric-argument (&repeat)
        &negative-function beginning-of-paragraph
        (if (at-end-of-paragraph)
	  (skip-over-whitespace)
	  (if (at-end-of-buffer)(command-quit)))
        (do-forever
	(go-to-end-of-line)
	(if (at-end-of-paragraph)(stop-doing))
	(if (lastlinep)(command-quit))
	(next-line)))


(defcom mark-paragraph
        (if (at-beginning-of-paragraph)
	  (set-the-mark)
	  (end-of-paragraph)
	  else
	  (if (at-end-of-paragraph)
	      (set-the-mark)
	      (beginning-of-paragraph)
	      (exchange-point-and-mark)
	      else
	      (beginning-of-paragraph)
	      (set-the-mark)
	      (end-of-paragraph))))

;;;
;;;	Buffers and the multi-plexing thereof.
;;;	 (New buffer state management, GMP, 09/26/78)
;;;

;;; This template defines the saved state of a buffer.
(setq buf-state-template
      '((marklist)(curline-marklist) (buffer-modified-flag) (curstuff)
        (curline) (curlinel) (curpointpos) (fpathname) (buffer-tempsegs)
        (buffer-uid) (buffer-file-dtcm) (firstline) (lastline) (der-wahrer-mark)
        (current-buffer-mode) (per-buffer-key-bindings) (fill-prefix)
        (fill-column) (comment-column) (comment-prefix) (tab-equivalent)
        (buffer-minor-modes) (buffer-locvars) (read-only-flag)
        (number-of-lines-in-buffer) (hard-enforce-fill-column)
        (mark-ring) (named-mark-list) (display-linechange-interrupt-hook)))

;;; Buffer hooks.  RMSoley 1 July 1981
;;; Hooks for the life of your buffer.
(defhook buffer-creation-hook)
(defhook buffer-entrance-hook)
(defhook buffer-exit-hook)
(defhook buffer-destruction-hook)

(defun destroy-buffer-contents ()
       (go-to-beginning-of-buffer)
       (with-mark here
	        (go-to-end-of-buffer)
	        (without-saving
		(wipe-point-mark here)))
       (release-temp-segments buffer-tempsegs)
       (setq buffer-tempsegs nil))

;;; Create a new buffer
(defun buffer-factus-est (bufnam)
       (and minibufferp (command-quit))
       (putprop bufnam (subst nil nil buf-state-template) 'buffer-state)
       ;; give buffer initial state
       (buffer-in-nihilem-factus-est)
       (setq per-buffer-key-bindings nil
	   buffer-minor-modes nil
	   buffer-modified-flag nil
	   buffer-locvars nil
	   read-only-flag nil
	   current-buffer bufnam)
       (or (memq bufnam known-buflist)
	 (setq known-buflist (cons bufnam known-buflist)))
       (fundamental-mode)
       (hook-function buffer-creation-hook))

;;; Destroy buffer's contents
(defun buffer-in-nihilem-factus-est ()
       (setq curstuff NLCHARSTRING		;hic incipit omnia
	   curline (cons curstuff (ncons nil))
	   curlinel 1
	   curpointpos 0
	   number-of-lines-in-buffer 1
	   fpathname nil
	   buffer-tempsegs nil
	   buffer-uid 0
	   buffer-file-dtcm 0
	   der-wahrer-mark nil
	   marklist nil			;let marks not inhib gc
	   curline-marklist nil
	   mark-ring (make-kill-ring 10.)
	   named-mark-list nil
	   firstline curline
	   lastline curline))


;;; Establish bare EMACS mode
(defcom fundamental-mode
        (revert-local-key-bindings)
        (setq buffer-minor-modes nil
	    per-buffer-key-bindings nil
	    fill-prefix ""
	    fill-column default-fill-column
	    hard-enforce-fill-column nil
	    display-linechange-interrupt-hook nil
	    tab-equivalent 10.
	    comment-prefix ""
	    comment-column default-comment-column
	    current-buffer-mode 'Fundamental))

(defun exists-buffer (buffer-name)
       (memq buffer-name known-buflist))

;;; Kill the named buffer
(defun buffer-kill (bufnam)
       (cond ((get bufnam 'buffer-delenda-est))	;avoid recursion from
	   (t (putprop bufnam t 'buffer-delenda-est) ;redisplay
	      (hook-function buffer-destruction-hook)
	      (redisplay-purge-buffer bufnam)
	      (buffer-destroy-annihilate-named-marks
	        (get-buffer-state bufnam 'named-mark-list) bufnam)
	      (release-temp-segments
	        (get-buffer-state bufnam 'buffer-tempsegs))
	      (setq known-buflist (delq bufnam known-buflist))
	      (remprop bufnam 'buffer-state)
	      (remprop bufnam 'dont-notice-modified-buffer)
	      (remprop bufnam 'temporary-buffer)
	      (remprop bufnam 'buffer-delenda-est)
	      (cond ((eq bufnam current-buffer)
		   (setq current-buffer
		         (car known-buflist)))
		  (t nil)))))

(defun dont-notice-modified-buffer (buf)
       (putprop buf t 'dont-notice-modified-buffer))

;;; Reinitialize buffer
(defun reinitialize-current-buffer ()
       (redisplay-buffer-reinit-purge current-buffer)
       (buffer-destroy-annihilate-named-marks named-mark-list current-buffer)
       (release-temp-segments buffer-tempsegs)
       (buffer-in-nihilem-factus-est))

;;; Get rid of all the named marks
(defun buffer-destroy-annihilate-named-marks (loc-marklist bufnam)
       (and loc-marklist			;cheap bum to avoid 1 cons
	  (setq bufnam (mark-tag-fun bufnam)))
       (mapc '(lambda (x)(remprop x bufnam)) loc-marklist))

(defun mark-tag-fun (name)
       (make_atom (catenate name ".mark_tag")))

;;; Save state of current buffer
(defun save-buffer-state ()
       (close-line)
       (setq previous-buffer current-buffer)
       (revert-local-key-bindings)
       ;; Store old and instate new buffer variables.
       (mapc '(lambda (x)
		  (rplacd (cdr x) (symeval (car x)))
		  (set (car x) (cadr x)))	;old value
	   buffer-locvars)
       (mapc '(lambda (x)			;save state values
		  (rplacd x (symeval (car x))))
	   (get current-buffer 'buffer-state))
       (cond ((get current-buffer 'temporary-buffer)
	    (buffer-kill current-buffer)
	    (setq previous-buffer (car known-buflist)))))

;;; Set buffer to die.
(defun set-buffer-self-destruct (bufnam)
       (putprop bufnam t 'temporary-buffer))

;;; Retrieve the specified buffer's state
(defun retrieve-buffer-state (bufnam)
       (mapc '(lambda (x)			;get state values
		  (set (car x) (cdr x)))
	   (get bufnam 'buffer-state))
       (instate-local-key-bindings)
       (setq current-buffer bufnam)	;this is not the current buffer
       (mapc '(lambda (x)			;setup local variables
		  (rplaca (cdr x) (and (boundp (car x)) (symeval (car x))))
		  (set (car x) (cddr x)))
	   buffer-locvars)
       (cond (macro-collection-in-progress
	     (assert-minor-mode '|Macro Learn|))
	   (t (negate-minor-mode '|Macro Learn|))))

;;; Get the value of the specified state of the specified buffer
(defun get-buffer-state (bufnam state-name)
       (cond ((eq bufnam current-buffer)
	    (symeval state-name))
	   (t (cdr (or (assq state-name (get bufnam 'buffer-state))
		     (error "get-buffer-state: can't for"
			  (cons bufnam state-name)
			  'fail-act))))))

;;; Delete a mark from the specified buffer
;;; This is used by redisplay, q.v.
(defun del-mark-from-buffer (m bufnam)
       (cond ((eq bufnam current-buffer) (release-mark m))
	   (t (and m bufnam
		 (let ((bufstat (get bufnam 'buffer-state)))
		      (let ((gl-assq-value (assq 'marklist bufstat))
			  (curl-assq-value (assq 'curline bufstat))
			  (lc-assq-value
			    (assq 'curline-marklist bufstat)))
			 (let ((marklist (cdr gl-assq-value))
			       (curline (cdr curl-assq-value))
			       (curline-marklist (cdr lc-assq-value)))
			      (release-mark m)
			      (and gl-assq-value
				 (rplacd gl-assq-value marklist))
			      (and lc-assq-value
				 (rplacd lc-assq-value
				         curline-marklist)))))))))

;;; Instate new minor mode in current buffer
(defun assert-minor-mode (mode)
       (setq damaged-flag t)
       (or (memq mode buffer-minor-modes)
	 (setq buffer-minor-modes (cons mode buffer-minor-modes))))

;;; Remove minor mode from current buffer
(defun negate-minor-mode (mode)
       (setq damaged-flag t)			;oh barf
       ;; Copy for benefit of mode line redisplay.
       (setq buffer-minor-modes
	   (subst nil nil (delq mode buffer-minor-modes))))


(declare (*expr require-symbol))

;;; Register a local buffer variable
(defun register-local-var (v)
       (require-symbol v)
       (or (boundp v) (set v nil))
       (let ((assoc-answer (assq v buffer-locvars)))
	  (cond (assoc-answer (rplacd (cdr assoc-answer) nil))
	        (t (setq buffer-locvars
		       (cons (cons v (ncons (symeval v)))
			   buffer-locvars))))))


;;; Register a local buffer variable supplying default value
(defun establish-local-var (v default)
       (require-symbol v)
       (cond ((assq v buffer-locvars))
	   (t (or (boundp v) (set v default))
	      (register-local-var v))))


;;; Switch to existing buffer
(defun go-to-buffer (bufnam)
       (and minibufferp (command-quit))
       (let ((prop (get bufnam 'buffer-state)))
	  (cond ((eq current-buffer bufnam))
	        (prop
		(hook-function buffer-exit-hook)
		(redisplay-leave-buffer)
		(save-buffer-state)
		(retrieve-buffer-state bufnam)
		(hook-function buffer-entrance-hook))
	        (t (error "go-to-buffer: Non-existent buffer: "
		        bufnam
		        'fail-act)))))

;;; Switch to buffer creating if necessary
(defun go-to-or-create-buffer (bufnam)
       (and minibufferp (command-quit))
       (cond ((eq bufnam current-buffer))
	   ((memq bufnam known-buflist)(go-to-buffer bufnam))
	   ((= (stringlength bufnam) 0)
	    (go-to-or-create-buffer previous-buffer))
	   (t (hook-function buffer-exit-hook)
	      (save-buffer-state)
	      (setq current-buffer bufnam)
	      (buffer-factus-est bufnam)
	      (hook-function buffer-entrance-hook))))


;;; Map supplied function over all buffers
(defun map-over-emacs-buffers (function argument)
       (mapc '(lambda (x)
		  (funcall function x argument))
	   known-buflist))


;;; This predicate returns t if the specified buffer is empty
(defun empty-buffer-p (bufnam)
       (cond ((memq bufnam known-buflist)
	    (and (eq (get-buffer-state bufnam 'firstline)
		   (get-buffer-state bufnam 'lastline))
	         (= (get-buffer-state bufnam 'curlinel) 1)))
	   (t t)))		;not a buffer, claim its empty


;;; Set buffer modified flag, announcing if needed
(defun buffer-has-been-modified--take-note ()
       (cond (read-only-flag
	     (cond ((eq t read-only-flag)
		  (report-error 'read-only))
		 (t (funcall read-only-flag))))
	   (buffer-modified-flag)
	   ((get current-buffer 'temporary-buffer))  ;don't count.
	   ((and (not tty-no-upmotionp)
	         (not (empty-buffer-p current-buffer))
	         (display-error-remark "Modified.")))
	   (t nil))
       (setq buffer-modified-flag t))

;;; Yanking primitives.

(defun make-kill-ring (n)
       (do ((i 1 (1+ i))
	  (this (cons nil (cons nil nil))
	        (cons nil (cons prev nil)))
	  (prev nil this)
	  (first))
	 ((> i n)
	  (rplaca (cdr first) prev)
	  (rplacd (cdr prev) first)
	  prev)

	 (and prev (rplacd (cdr prev) this))
	 (and this (rplaca (cdr this) prev))
	 (and (= i 1)(setq first this))))


(defun killsave-reverse-list (l)(killsave-list (nreverse l)))
(defun killsave-char (c)(killsave-string (ItoC (getcharn c 1))))
(defun killsave-list (l)(killsave-string (apply-catenate l)))
(defun killsave-string (s)
       (cond ((null kill-ring)
	    (let ((c (ncons s))
		(ele (ncons nil)))
	         (rplacd ele c)
	         (rplaca ele c)
	         (rplacd c ele)
	         (setq kill-ring c kill-ring-current-size 1)))
	   ((< kill-ring-current-size kill-ring-max-size)
	    (let ((c (ncons s))
		(ele (ncons nil))
		(forward kill-ring)
		(backward (cadr kill-ring)))
	         (rplacd c ele)
	         (rplaca ele backward)
	         (rplacd ele forward)
	         (let ((eforward (cdr forward))
		     (ebackward (cdr backward)))
		    (rplaca eforward c)
		    (rplacd ebackward c))
	         (setq kill-ring c
		     kill-ring-current-size (1+ kill-ring-current-size))))
	   (t 
	     (setq kill-ring (cadr kill-ring))
	     (rplaca kill-ring s))))

(defun kill-ring-top ()
       (or kill-ring (report-error 'empty-kill-ring))
       (car kill-ring))

(defun rotate-kill-ring ()
       (setq kill-ring (cddr kill-ring)))

(defun kill-pop ()				;fixed to close ring 8/3/80
       (prog1 (kill-ring-top)
	    (rotate-kill-ring)))

(defun kill-pop-symbol ()
       (let ((popped (kill-pop)))
	  (cond ((symbolp popped)(intern popped))
	        (t (intern (make_atom popped))))))

(defun merge-kills-forward ()
       (cond ((and (get previous-command 'kills)
	         (> kill-ring-current-size 1)
	         (not dont-stash))
	    (let ((top (kill-pop))
		(bot (kill-pop)))
	         (killsave-string (catenate bot top))))))

(defun merge-kills-reverse ()
       (cond ((and (get previous-command 'kills)
	         (> kill-ring-current-size 1)
	         (not dont-stash))
	    (let ((top (kill-pop))
		(bot (kill-pop)))
	         (killsave-string (catenate top bot))))))

;;;
;;;	Startup time functions.
;;;

(defun editor-main-init ()
       (setq multesque-readtable (get (makreadtable t) 'array))
       (let ((readtable multesque-readtable))
	  (sstatus syntax 56 2)	; = .
	  (sstatus syntax 57 2)	; = /
	  (sstatus syntax 47 2)	; = '
	  (sstatus syntax 73 2))	; = ;
       (putprop 'xr-backquote-macro
	      (catenate lisp-system-dir ">lisp_backquote_")
	      'autoload)
       (setsyntax '/` 'macro 'xr-backquote-macro)
       (setq process-dir (e_lap_$trim (get_pdir_)))
       (setq work-seg (e$get_temporary_seg))
       (setq work-string (e_lap_$make-dat-ol-black-magic-string work-seg))
       (setq building-buf (e_lap_$make-dat-ol-black-magic-string
		        (e$get_temporary_seg)))
       (setq pdir-temp-ename "!!emacs!!tempbufferimage!!"
	   pdir-temp-pathname (catenate process-dir ">" pdir-temp-ename))
       (setq known-buflist nil minibufferp nil)
       (setq default-fill-column 78. default-comment-column 60.)
       (setq last-minibuf-response "")

       ;buffer for start_up to play in
       (buffer-factus-est '|<start_up_emacs_buffer>|)

       (putprop current-buffer t 'temporary-buffer)
       (setq previous-buffer current-buffer)
       (setq dont-stash nil
	   dont-damage-flag nil
	   damaged-flag t
	   target-screen-hpos 0
	   touchy-damaged-flag t)
       (setq kill-ring nil)
       (setq kill-ring-current-size 0)
       (setq search-ring (make-kill-ring 20.))
       (setq last-search-string ""))

;;;(register-option 'kill-ring-max-size 10.) ;;; moved to e_option_defaults_
;;;(register-option 'default-fill-column 78.) ;;; moved to e_option_defaults_
;;;(register-option 'default-comment-column 60.) ;;; moved to e_option_defaults_

(declare (special no-minibuffer-<>))
;;; Whether or not to quit when BREAK is hit during emacs_ invocation.
;;;(register-option 'quit-on-break t) ;;; moved to e_option_defaults_
;;;(register-option 'no-minibuffer-<> nil) ;;; moved to e_option_defaults_
;;;(register-option 'underline-whitespace nil) ;;; moved to e_option_defaults_

;;;
;;;	The minibuffer.
;;;

(defprop minibuf-response minibuffer-response expr)
(declare (special last-minibuffer-response remember-empty-response))
;;; Remember <CR> responses?
;;;(register-option 'remember-empty-response 't) ;;; moved to e_option_defaults_

;;; Updated for splits, Sept 85 EDSchroth

(declare (*expr rdis-enter-split))
(defvar (current-split
	screenlinelen
	screenheight
	main-window-size
	screen
	eline-conts
	windows
	nwindows
	minibuffer-split))

(defun minibuffer-response lexpr
       (close-line)
       (let ((curline (make-eline))
	   (line-control:buffer 0)
	   (curlinel 1)
	   (curpointpos 0)
	   (curstuff (cond ((< lexpr 3) NLCHARSTRING)
		         (t (catenate (arg 3) NLCHARSTRING))))
	   (numarg nil)
	   (firstline nil)
	   (lastline nil)
	   (recursive-minibufferp minibufferp)
	   (minibufferp (cond ((< lexpr 2) NL)
			  ((fixp (arg 2)) (ascii (arg 2)))
			  (t (arg 2))))
	   (marklist nil)
	   (curline-marklist nil)
	   (der-wahrer-mark nil)
	   (fill-prefix "")
	   (buffer-modified-flag t)
	   (read-only-flag nil)
	   (display-linechange-interrupt-hook nil)
	   (damaged-flag t)
	   (touchy-damaged-flag t)
	   (hard-enforce-fill-column nil)
	   (minibuffer-prompt-string (catenate "     " (arg 1)))
	   ;;save current split info away
	   (current-split current-split)
	   (screenlinelen screenlinelen)
	   (screenheight screenheight)
	   (main-window-size main-window-size)
	   (screen screen)
	   (eline-conts eline-conts)
	   (windows windows)
	   (nwindows nwindows))

	  (setq curlinel (stringlength curstuff))
	  (setf (eline-contents curline) curstuff)
	  (setq lastline curline firstline curline)
	  (if (not recursive-minibufferp)
	      (revert-local-key-bindings))
	  (rdis-enter-split minibuffer-split)	;activate correct split
					;this does not actually move
					;cursor, redisplay will.
	  (protect
	    (catch (charlisten) gazonga)
	    &failure
	    (minibuffer-clear)
	    &success
	    (cond ((or macro-execution-in-progress
		     suppress-redisplay-flag
		     rdis-suppress-redisplay
		     no-minibuffer-<>))
		(t (redisplay)
		   (minibuffer-print-noclear
		     minibuffer-end-string))))
	  (if (not recursive-minibufferp)
	      (instate-local-key-bindings))
	  (or macro-execution-in-progress
	      rdis-suppress-redisplay
	      (redisplay))
	  (let ((it (curbuf-as-string)))
	       (or (and (nullstringp it) (not remember-empty-response))
		 (setq last-minibuffer-response
		       (setq last-minibuf-response it)))
	       it)))

(defun jetteur-des-gazongues () (throw 'les-grandes-gazongues gazonga))

;;; Read from the minibuffer and strip whitespace.
(defprop trim-minibuf-response trim-minibuffer-response expr)
(defun trim-minibuffer-response lexpr
       (e_lap_$trim
         (minibuffer-response (arg 1)
			(cond ((> lexpr 1) (arg 2))
			      (t NL)))))

;;; Read from the minibuffer and return a symbol.
(defprop intern-minibuf-response intern-minibuffer-response expr)
(defun intern-minibuffer-response lexpr
       (intern
         (make_atom
	 (e_lap_$trim
	   (minibuffer-response (arg 1)
			    (cond ((> lexpr 1) (arg 2))
				(t NL)))))))

(args 'minibuf-response '(1 . 3))
(args 'trim-minibuf-response '(1 . 2))
(args 'intern-minibuf-response '(1 . 2))
(args 'minibuffer-response '(1 . 3))
(args 'trim-minibuffer-response '(1 . 2))
(args 'intern-minibuffer-response '(1 . 2))

;;; Ask a question requiring a yes/no response
(defun yesp (question)
       (do ((response))
	 (nil)
	 (setq response
	       (trim-minibuf-response (catenate (e_lap_$rtrim question)
					"  ")
				NL))
	 (cond ((samepnamep response "no")  (return nil))
	       ((samepnamep response "yes") (return t))
	       ((samepnamep response "n")   (return nil))
	       ((samepnamep response "y")   (return t))
	       (t (init-local-displays)
		(local-display-generator-nnl
		  "Please answer ""yes"" or ""no"".")
		(ring-tty-bell)))))

;;;
;;;	Manipulate default search string(s)
;;;	 J. Spencer Love, 14 May 1982
;;;

(defun get-search-string (prompt)
       (search:maybe-push-default (search:prompt prompt) 'string))


(declare (special search:ring))

(setq search:ring (make-kill-ring 20.))


(defun search:maybe-push-default (string type)
       (cond ((samepnamep string (search:last-string))
	    (cond ((not (eq type (search:last-type)))
		 (search:set-ring-top string type))))
	   ((< (stringlength (search:last-string)) 2)
	    (search:set-ring-top string type))
	   (t (search:push-ring string type)))
       string)


(defun search:prompt (prompt)
       (let ((completion-list)
	   (default (search:last-string)))
	  (cond ((nullstringp default)
	         (setq prompt (catenate prompt ": ")))
	        (t (setq prompt (catenate prompt " ("
				    default "): "))))
	  (setq completion-list (list default))
	  (setq prompt (minibuffer-response prompt))
	  (cond ((not (nullstringp prompt)) prompt)
	        ((not (nullstringp default)) default)
	        (t (display-error "There is no default search string.")))))


(defun search:numeric-prompt (prompt)
       (cond ((and numarg (not (= numarg 1)))
	    (catenate prompt " [" (decimal-rep numarg) " times]"))
	   (t prompt)))
	     

(defun search:last-string ()
       (cond ((car search:ring) (caar search:ring))
	   (t "")))


(defun search:last-type ()
       (cond ((car search:ring) (cdar search:ring))
	   (t nil)))


(defun search:set-ring-top (string type)
       (cond ((car search:ring)
	    (rplaca (car search:ring) string)
	    (rplacd (car search:ring) type))
	   (t (rplaca search:ring (cons string type))))
       (setq last-search-string string))


(defun search:push-ring (string type)
       (setq search:ring (cadr search:ring))
       (search:set-ring-top string type))


(defun search:pop-ring ()
       (setq last-search-string
	   (cond ((car search:ring)
		(let ((result (caar search:ring)))
		     (setq search:ring (cddr search:ring))
		     result))
	         (t ""))))


(defun search:rotate-ring ()
       (cond ((car search:ring)
	    (let ((result (caar search:ring)))
	         (do () (nil)
		   (setq search:ring (cddr search:ring))
		   (and (car search:ring) (return nil)))
	         result))
	   (t "")))

(defun nullstringp (x)
       (= (stringlength x) 0))

;;; Utility functions.

(defun curline-as-string ()
       (cond((eq curstuff work-string)(substr work-string 1))
	  ((stringp curstuff) curstuff)
	  (t (filerep-to-string curstuff))))


(defun curchar ()
       (ascii (e_lap_$ggcharn curstuff curpointpos)))


(defun insert-string (s)
       (note-modified-buffer)
       ;;open line will unconditionally cause damaged flag.
       (do ((cx 1)
	  (sl (stringlength s))
	  (nlx 0)
	  (chunk-length 0))
	 ((> cx sl))
	 (setq chunk-length (- sl cx -1))
	 (e_lap_$rplacstring-offset building-buf s chunk-length 0 chunk-length (1- cx))
	 (setq nlx (index building-buf NL))
	 (open-line)
	 (let ((insertl (cond ((= 0 nlx) chunk-length)
			  (t (1- nlx)))))
	      (relocate-marks curline insertl 'c+)
	      (e_lap_$insert-chars work-string curpointpos building-buf insertl)
	      (setq curpointpos (+ insertl curpointpos))
	      (setq curlinel (+ insertl curlinel))
	      (setq cx (+ cx insertl)))
	 (cond ((= nlx 0))
	       (t (insert-char NL)		; might like new-line- open to debate. 6/14/78
		(setq cx (1+ cx))))))

(defun apply-catenate (list)			;General utility 7/30/79 BSG
	 (e_lap_$rplacstring building-buf "" 0 0 0)
	 (do ((s list (cdr s))
	      (cl)(ll 0))
	     ((null s)(substr building-buf 1))
	     (setq cl (stringlength (car s)))
	     (e_lap_$rplacstring building-buf (car s)
			     cl ll (setq ll (+ ll cl)))))

(defun curbuf-as-string ()
       (go-to-beginning-of-buffer)
       (with-mark m
	        (go-to-end-of-buffer)
	        (point-mark-to-string m)))

;;;
;;; Error System for Multics Emacs.
;;; Richard Mark Soley, 26 June 1981
;;; To standardize and Multics-ize the error reporting of Emacs.
;;;
;;;
;;; To use the new error-reporting routine, report-error, call:
;;;
;;; (report-error error-code additional_information)
;;;
;;; Where error-code may be:
;;;   (1) An Emacs standard error code  (see list below).
;;;   (2) A Multics standard error code (from PL/1).
;;;   (3) Any error_table type error code (from PL/1, fixed bin (35.))
;;;   (4) Any error code, symbolically (i.e. 'error_table$moderr)
;;;   (5) An asterisk ('*), signifying that the bell should be rung.
;;;   (6) Any string, to be appended to "Error: ".
;;; and additional_information is more information about the error,
;;; to be printed.
;;; 
;;; The function report-error-noabort has an identical calling sequence,
;;; but does not abort the current command as report-error does.
;;;

(declare
  (special SPACE SPACES error-system:error-codes error-system:known-tables
	 null-pointer))

;;; The standard error codes.
;;; An asterisk (*) specifies that no error message should be printed.
(setq error-system:error-codes
      '(
      (bad-error-code	"Invalid error code identifier.")
      (bad-error-message	"Invalid error message: must be string or *.")
      (beginning-of-buffer	*)
      (empty-kill-ring	"Nothing in the kill ring.")
      (end-of-buffer	*)
      (need-mark-name	"You must supply a mark name.")
      (no-named-mark	"Named mark not found in buffer.")
      (mark-not-set		"The mark has not been set.")
      (object-seg		"File is an object segment.")
      (read-only		"Attempt to modify read-only buffer.")
      ))

;;; Add or change an entry in the error table.
(defun add-error-code (code message)
       (or (symbolp code)
	 (report-error 'bad-error-code SPACE code))
       (or (stringp message) (eq message '*)
	 (report-error 'bad-error-message SPACE message))
       (setq error-system:error-codes
	   (cons (list code message)
	         error-system:error-codes)))

;;; Return octal ASCII representation of a number.
(defun octal-rep (number)
       (let ((base 8.)) (maknam (exploden number))))

;;; Toplevel error reporter.
(defun report-error lexpr
       (let ((code (error-system:error-action (arg 1))))
	  (cond ((eq code '*) (command-quit))
	        ('else
		(apply 'display-error
		       (cons code
			   (error-system:canonicalize
			     (cdr (listify lexpr)))))))))

;;; Same as above, but doesn't abort.
(defun report-error-noabort lexpr
       (let ((code (error-system:error-action (arg 1))))
	  (cond ((eq code '*) (ring-tty-bell))
	        ('else
		(apply 'display-error-noabort
		       (cons code
			   (error-system:canonicalize
			     (cdr (listify lexpr)))))))))

;;; Canonicalize objects so that catenate can handle them.
(defun error-system:canonicalize (list)
       (mapcar '(lambda (x)
		    (cond ((numberp x) (decimal-rep x))
			((atom x) x)
			('else (decimal-rep x))))
	     list))

;;; Find out correct error action from error-system:table.
(defun error-system:error-action (code-name)
       (let ((found (assq code-name error-system:error-codes)))
	  (cond (found (cadr found))
	        ((numberp code-name)
	         (e_lap_$rtrim (cadr (convert_status_code_ code-name))))
	        ((eq code-name '*) '*)
	        ((not (zerop (setq found (index code-name "$"))))
	         (e_lap_$rtrim
		 (cadr
		   (convert_status_code_
		     (error-table
		       (substr code-name 1 (1- found))
		       (substr code-name (1+ found)))))))
	        ('else
		(catenate "Error: " code-name)))))

;;; Returns string representation of object, using decimal read base.
(defun decimal-rep (x)
       (let ((base 10.) (ibase 10.) (*nopoint t))
	  (maknam (exploden x))))

;;;
;;; Replacment for BSG's old error_table hack for Emacs, to access
;;; any number of error tables.
;;; Richard Mark Soley, 26 June 1981
;;;

(setq error-system:known-tables () null-pointer 007777000001)

;;; Top level function, called like (error-table 'error_table_ 'badopt)
;;; Returns number convert_status_code_ can grok.
;;; Memoizes values (to conserve hcs_$make_ptr calls) by putting value on
;;; table's name property of code-name. (Thank you, RSL - RMSoley)
(defun error-table (table-name code-name)
       (setq table-name (make_atom table-name)
	   code-name (make_atom code-name))
       (or (get code-name table-name)
	 (putprop code-name
		(arraycall fixnum
			 (error-system:get-array table-name code-name)
			 (error-system:get-code table-name code-name))
		table-name)))

;;; Get ptr to error table, store away if we haven't seen it before.
(defun error-system:get-array (table code)
       (or (cdr (assq table error-system:known-tables))
	 (let ((ptr (hcs_$make_ptr null-pointer table code)))
	      (or (zerop (cadr ptr))
		(display-com-error (cadr ptr) table))
	      (setq error-system:known-tables
		  (cons
		    (cons table
			(*array nil 'external
			        (boole 1 (car ptr) 007777000000)
			        777777))
		    error-system:known-tables))
	      (cdar error-system:known-tables))))

;;; Get ptr to entry in table.
(defun error-system:get-code (table code)
       (let ((ptr (hcs_$make_ptr null-pointer table code)))
	  (or (zerop (cadr ptr))
	      (display-com-error (cadr ptr) table "$" code))
	  (boole 1 (car ptr) 000000777777)))

;;; For compatability: implements access to standard error_table_.

(defun error_table_ (code) (error-table 'error_table_ code))

;;; This is the guts of line control.
;;; 24 June 1981 Richard Mark Soley

;;; The format of a line-control template:
;;;
;;; ( (x1 y1 flag1) (x2 y2 flag2) . . .)
;;;
;;; If the current hpos is in the range x1 <= hpos <= y1, then:
;;; If flag1 = nil, give read-only message.
;;; If flag1 = t, allow modification.
;;; Else, do (funcall flag1)
;;;

(setq line-control:buffer nil line-control:template)

;;; Turn on line control.
(defun line-control:on (template)
       (line-control:instate-template template)
       (register-local-var 'line-control:buffer)
       (setq line-control:buffer current-buffer)
       (setq read-only-flag 'line-control:handler))

;;; Turn off line control.
(defun line-control:off ()
       (setq read-only-flag nil line-control:template nil))

;;; Check validity of line control template and install if OK.
(defun line-control:instate-template (template)
       (do ((tem template (cdr tem)))
	 ((null tem)
	  (register-local-var 'line-control:template)
	  (setq line-control:template template))
	 (let ((x (caar tem))
	       (y (cadar tem))
	       (f (caddar tem)))
	      (and (or (not (symbolp f))
		     (not (numberp x))
		     (and (not (numberp y)) 
			(not (and (eq y '>) (null (cdr tem)))))
		     (and (numberp y)
			(> x y)))
		 (error "Bad line-control template supplied."
		        (car tem)
		        'fail-act)))))

;;; Handler for changes of buffer.
;;; This gets called by buffer-has-been-modified--take-note
(defun line-control:handler ()
       (line-control:check (cur-hpos)))

(defun line-control:validate (function)
       (cond ((null function)
	    (display-error "Attempt to modified protected region."))
	   ((eq function t))
	   ('else (funcall function))))

;;; The guts of line control checking.
;;; Takes three kinds of objects:
;;; 'end => check to the end of the line.
;;; a number => check for that number.
;;; a mark => check forward to the mark.
(defun line-control:check (object)
       (let ((hpos (cur-hpos)))
	  (cond ((eq object 'end)
	         (do ((tem line-control:template (cdr tem)))
		   ((null tem))
		   (let ((x (car tem)))
		        (and (not (< hpos (car x)))
			   (or (eq (cadr x) '>)
			       (not (> hpos (cadr x))))
			   (return
			     (mapc '(lambda (y)
					(line-control:validate
					  (caddr y)))
				 tem))))))
	        ((numberp object)
	         (do ((tem line-control:template (cdr tem)))
		   ((null tem))
		   (let ((x (car tem)))
		        (and (not (< object (car x)))
			   (or (eq (cadr x) '>)
			       (not (> object (cadr x))))
			   (return
			     (line-control:validate (caddr x)))))))
	        ((mark-on-current-line-p object)
	         (line-control:check-region hpos (cdr object)))
	        ('else
		(line-control:check 'end)
		(do ((line curline (eline-next line))
		     (count 0 (1+ count)))
		    ((or (> count 1)
		         (eq line (car object)))
		     (and (> count 1)
			(mapc '(lambda (y) 
				     (line-control:validate (caddr y)))
			      line-control:template))
		     (line-control:check-region 0 (cdr object))))))))
		    
(defun line-control:check-region (from to)
       (do ((tem line-control:template (cdr tem))
	  (found-from nil)
	  (last nil))
	 ((or last (null tem)))
	 (let ((x (caar tem))
	       (y (cadar tem))
	       (f (caddar tem)))
	      (cond (found-from
		    (and (not (< to x))
		         (or (eq y '>)
			   (not (> to y)))
		         (setq last t))
		    (line-control:validate f))
		  ((and (not (< from x))
		        (or (eq y '>)
			  (not (> from y))))
		   (setq found-from t)
		   (line-control:validate f))))))

;;; COMMANDS

;;;
;;;	Character movement and deletion.
;;;

(defun cur-hpos ()			;copped from rdis, 5/8/79, bsg
       (do ((origstrl (1- curlinel))
	  (strx 0)(ocol 0)(tabx)(lies 0))
	 ((not (< strx curpointpos))
	  (+ curpointpos lies))
	 (setq tabx (e_lap_$tabscan curstuff origstrl strx))
	 (setq strx (+ strx tabx) ocol (+ ocol tabx))
	 (cond ((not (< strx curpointpos))
	        (return (+ curpointpos lies))))
	 (let ((ch (e_lap_$ggcharn curstuff strx)))
	      (cond ((= ch 11)
		   (setq tabx (- tab-equivalent (\ ocol tab-equivalent))))
		  ((= ch 10)(setq tabx -1))
		  ((or (= ch 14)(= ch 15))(setq tabx (- ocol)))
		  (t (setq tabx 0))))
	 (setq lies (+ -1 lies tabx) ocol (+ ocol tabx)) ; -1 for orig tab char
	 (and (< ocol 0)(setq ocol 0))
	 (setq strx (1+ strx))))		;end of do- answer in strg

;;; Self-insert with undo function (ACW's idea)
(defcom self-insert
        &no-break
        &numeric-argument (&repeat &lower-bound 0)
        &undo de-self-insert
        (insert-char last-input-char))

(defcom de-self-insert
        (save-excursion
	(cond ((reverse-search-in-line last-input-char)
	       (redisplay)
	       (sleep .2)			;pause at character
	       (delete-char))		;then get rid of it.
	      (t (display-error last-input-char " not found")))))

;;; The \ command: simulate Multics octal escape.
(defcom escape-char
        &numeric-argument (&pass &lower-bound 0)
        (let ((first-char (get-char))
	    (the-char)		; will be result to insert
	    (char-to-execute -1))	; non digit read to be processed
	   (if (or (< first-char (CtoI "0")) (> first-char (CtoI "7")))
	       (if (not (member first-char MCS-editing-characters))
		 (insert-char MCS-escape-character))
	       (setq the-char (ascii first-char))	; insert it
	       ;; an octal digit, read escape sequence
	       else
	       (setq the-char (- first-char (CtoI "0")))	; current value
	       (do-times 2		; at most, two more chars to read
		       (let ((next-char (get-char)))
			  (if (or (< next-char (CtoI "0"))
				(> next-char (CtoI "7")))
			      (setq char-to-execute next-char)
			      (stop-doing))	; non-digit, stop now
			  (setq the-char (+ (* the-char 8.)
					(- next-char (CtoI "0")))))))
	   ;; have value to insert
	   (if (numberp the-char)
	       (setq the-char (ascii the-char)) ;cr fixed 9/12/80 bsg
	       else
	       (if (eq the-char CRET)(setq the-char NL)))
	   (do-times (or numarg 1)
		   (insert-char the-char))
	   (if (not (= char-to-execute -1))
	       (setq numarg nil)
	       (process-char char-to-execute))))

(defcom forward-char-command
        &numeric-argument (&pass)
        &negative-function backward-char-command
        &undo backward-char-command
        (cond (numarg (forward-n-chars numarg))
	    (t (forward-char))))

(defun forward-n-chars (n)
       (declare (fixnum n))
       (and (minusp n) (backward-n-chars (- n)))
       (do ((line-left (- curlinel curpointpos 1) ;curlinel includes the newline
		   (- curlinel curpointpos 1)))
	 (())				;do-forever
	 (cond ((not (> n line-left))		;same line
	        (setq curpointpos (+ curpointpos n))
	        (stop-doing))
	       ((lastlinep) (command-quit))	;can't go down
	       (t (setq n (- n line-left 1))	;go down one
		(next-line)))))

(defcom backward-char-command
        &numeric-argument (&pass)
        &negative-function forward-char-command
        &undo forward-char-command
        (cond (numarg (backward-n-chars numarg))
	    (t (backward-char))))
 
(defun backward-n-chars (n)
       (declare (fixnum n))
       (if (minusp n) (forward-n-chars (- n)))
       (do-forever
         (cond ((not (> n curpointpos))		;same line
	      (setq curpointpos (- curpointpos n))
	      (stop-doing))
	     ((firstlinep) (command-quit))	;can't go up
	     (t (setq n (- n curpointpos 1))	;go up one
	        (prev-line)
	        (go-to-end-of-line)))))

(defcom forward-char
        &numeric-argument (&repeat)
        &negative-function backward-char
        &undo backward-char
        (cond ((eolp)			;go to next line
	     (cond ((lastlinep) (command-quit))
		 (t (next-line))))		;auto to beginning
	    (t (setq curpointpos (1+ curpointpos)))))

(defcom backward-char
        &numeric-argument (&repeat)
        &negative-function forward-char
        &undo forward-char
        (cond ((bolp)
	     (cond ((firstlinep) (command-quit))
		 (t (prev-line) (go-to-end-of-line))))
	    (t (setq curpointpos (1- curpointpos)))))

(defcom delete-char
        &numeric-argument (&repeat)
        &negative-function rubout-char
        (open-line)
        (cond ((eolp)(merge-line))
	    (t (setq curlinel (1- curlinel))
	       (relocate-marks curline -1 'c+)
	       (e_lap_$delete-chars work-string curpointpos 1))))

;;;(register-option 'rubout-tabs-into-spaces nil) ;;; moved to e_option_defaults_
(declare (special rubout-tabs-into-spaces))

;;; Delete one character backwards.
(defcom rubout-char
        &numeric-argument (&repeat)
        &negative-function delete-char
        (let ((here (cur-hpos)))
	   (backward-char)
	   (and rubout-tabs-into-spaces
	        (eq current-command 'rubout-char)
	        (looking-at TAB)
	        (do-times (- here (cur-hpos) 1) (insert-string SPACE)))
	   (delete-char)))

;;; The ^Q command: quote the next input character and insert it.
(defcom quote-char
        &numeric-argument (&pass &lower-bound 0)
        (let ((the-char (ascii (get-char))))
	   (if (eq the-char CRET)(setq the-char NL))
	   (do-times (or numarg 1)
		   (insert-char the-char))))

;;;
;;; Word movement, insertion, and deletion.
;;;

;;; Character table defining what a "word" looks like.
(setq good-word-charactertbl
      (charscan-table
        "A_BCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))

(defcom forward-word
        &numeric-argument (&repeat)
        &negative-function backward-word
        (do-forever
	(if (search-for-first-charset-line good-word-charactertbl)
	    (or (search-for-first-not-charset-line good-word-charactertbl)
	        (go-to-end-of-line))
	    (stop-doing))
	(if (lastlinep)(go-to-end-of-line)(stop-doing))
	(next-line)
	(go-to-beginning-of-line)))

(defcom backward-word
        &numeric-argument (&repeat)
        &negative-function forward-word
        (do-forever
	(if (search-back-first-charset-line good-word-charactertbl)
	    (or (search-back-first-not-charset-line good-word-charactertbl)
	        (go-to-beginning-of-line))
	    (stop-doing))
	(if (firstlinep)(go-to-beginning-of-line)(stop-doing))
	(prev-line)
	(go-to-end-of-line)))

(defkill delete-word forward)
(defcom delete-word
        &undo yank
        &numeric-argument (&repeat)
        &negative-function rubout-word
        (with-mark m
	         (forward-word)
	         (kill-backwards-to-mark m))
        (merge-kills-forward))

(defkill rubout-word reverse)
(defcom rubout-word
        &undo yank
        &numeric-argument (&repeat)
        &negative-function delete-word
        (with-mark m
	         (backward-word)
	         (kill-forward-to-mark m))
        (merge-kills-reverse))

;;;
;;; Fill routines.
;;;

(defcom set-fill-column
        &arguments ((column &integer
		        &default &eval (if numarg (max numarg 1)
				       else (1+ (cur-hpos)))))
        &numeric-argument (&pass)
        (setq fill-column (1- column))
        (minibuffer-remark "Fill column = " (decimal-rep column)))

(defcom autofill-self-insert
        &undo de-self-insert
        &no-break
        &numeric-argument (&pass)

        (let ((old-numarg numarg))
	   (setq numarg nil)

	   ;; First optimize by inserting numarg-1 of the character.
	   (and old-numarg
	        (do count 2 (1+ count) (< old-numarg count)
		  (self-insert)))

	   ;; Now decide if we need to fill.
	   (unwind-protect
	     (cond ((< (cur-hpos) fill-column))
		 ((or (not (get last-input-char 'whiteness))
		      (and (not (get (lefthand-char) 'whiteness))
			 (not (= (cur-hpos) fill-column)))
		      (save-excursion
		        (skip-back-whitespace-in-line)
		        (> (cur-hpos) fill-column)))
		  (do () ((not (> (cur-hpos) fill-column)))
		      (fill-current-line))))

	     ;; Now insert the character typed.
	     (or (equal old-numarg 0)
	         (self-insert)))))

(defun un-new-line ()
       (without-saving
         (kill-to-beginning-of-line) ; Punt fill prefix.
         (rubout-char)))

(defcom fill-current-line
        (let ((stuff (with-mark
		   end
		   (do ()	;Back up to ends of words until <= fill-column
		       ((not (> (cur-hpos) fill-column)))
		       (skip-back-to-whitespace)
		       (skip-back-whitespace-in-line))
		   (prog2 0
			(point-mark-to-string end)
			(without-saving (wipe-point-mark end))))))

	   (let ((save-hpos (cur-hpos)))

	        (new-line)	   ; fill prefix inserted by this

	        (cond

		;; If fill prefix is longer than fill-column, complain.
		((> (cur-hpos) fill-column)
		 (un-new-line)
		 (insert-string stuff)
		 (new-line)
		 (display-error
		   "Fill column is shorter than fill prefix."))

		;; If had to delete all the way to begin-of-line, barf.
		((zerop save-hpos)
		 (un-new-line)
		 (insert-string stuff)
		 (new-line)
		 (display-error
		   "Word does not fit within fill column."))

		(t (with-mark here
			    (insert-string stuff)
			    (save-excursion
			      (go-to-mark here)
			      (skip-over-whitespace-in-line)
			      (without-saving
			        (wipe-point-mark here)))))))))

(defcom autofill-new-line
        &undo un-new-line
        &numeric-argument &reject
	   (unwind-protect
	     (cond ((< (cur-hpos) fill-column))
		 ((or (not (get last-input-char 'whiteness))
		      (and (not (get (lefthand-char) 'whiteness))
			 (not (= (cur-hpos) fill-column)))
		      (save-excursion
		        (skip-back-whitespace-in-line)
		        (> (cur-hpos) fill-column)))
		  (do () ((not (> (cur-hpos) fill-column)))
		      (fill-current-line))))
	     (new-line)))

(defvar fill-mode-delimiters
        (list SPACE '/: '/. '/, '/; '/? '/! '/( '/) TAB))

(defcom fill-mode
        (prog ()
	    (if (memq 'speedtype buffer-minor-modes)
	        (speedtypeoff)
	        (fill-mode)
	        (speedtype)
	        (return nil))
	    (assert-minor-mode 'fill)
	    (setq hard-enforce-fill-column t)	;see e_interact 5/1/79
	    (mapc '(lambda (x)
		         (set-key x 'autofill-self-insert))
		fill-mode-delimiters)
	    (set-key CR 'autofill-new-line)))

(defcom fill-mode-off
        (cond ((memq 'speedtype buffer-minor-modes)
	     (speedtypeoff)
	     (fill-mode-off)
	     (speedtype))
	    (t (negate-minor-mode 'fill)
	       (setq hard-enforce-fill-column nil)
	       (mapc '(lambda (x)
			  (set-key x 'self-insert))
		   fill-mode-delimiters)
	       (set-key CR 'new-line))))

(defcom-synonym fillon fill-mode)

(defcom-synonym filloff fill-mode-off)

;;; This code replaced by CRDavis' fill routines, 23 November 1981 RMSoley
;;; Reinstalled for UNB installation December 1981.

(defcom runoff-fill-region			;archy, 4/79, this version
        &undo &code
	    (copy-region)
	    (wipe-this-and-yank-previous)
	    &end-code
        &numeric-argument (&pass)
        (or dont-stash (copy-region))
        (let ((bad-lines 0))
	   (with-mark
	     end-of-buffer
	     ;; Main loop
	     (walk-through-region
	       (if (point>markp end-of-buffer)(stop-doing))
	       (with-mark
	         start			;beginning of "word"
	         (skip-over-whitespace)
	         (skip-to-whitespace)
	         (with-mark
		 place			;end of "word"
		 (if (> (cur-hpos) fill-column)
		     ;; If past fill column, move "word" to next line
		     (go-to-mark start)
		     (delete-white-sides)
		     (if (not (bolp))
		         (new-line)
		         ;; Pad completed line if required.
		         (if numarg
			   (save-excursion
			     (prev-line)
			     (go-to-end-of-line)
			     (pad-line (- fill-column (cur-hpos)))))
		         else
		         ;; only done one word, already past fill column
		         (insert-string fill-prefix)
		         (go-to-mark place)
		         (delete-white-sides)
		         (if (not (eolp))(new-line)
			   else (next-line))
		         (delete-white-sides)
		         (insert-string fill-prefix)
		         (setq bad-lines (1+ bad-lines)))
		     else
		     ;; not past fill column
		     (if (< (cur-hpos) fill-column)
		         (delete-white-sides)
		         (do-forever	;get rid of newlines
			 (if (not (eolp))(stop-doing))
			 (if (mark-reached end-of-buffer)(stop-doing))
			 (delete-char)
			 (delete-white-sides))
		         ;; put extra blank after punctuation
		         (if (memq (lefthand-char) '(/. /? /!))
			   (insert-char SPACE))
		         (insert-char SPACE)
		         else
		         ;; at fill column
		         (delete-white-sides)
		         (if (eolp)
			   (next-line)
			   (delete-white-sides)
			   (insert-string fill-prefix)
			   else
			   (if (memq (lefthand-char) '(/. /? /!))
			       (insert-char SPACE))
			   (insert-char SPACE))))))))
	   ;; done with whole region
	   (delete-white-sides)
	   (if (> bad-lines 0)
	       (display-error-remark
	         "Found "
	         (decimal-rep bad-lines)
	         " line"
	         (if (= bad-lines 1) ""
		   else "s")
	         " longer than fill column."))))

(defcom runoff-fill-paragraph
        &undo &code
	    (copy-region)
	    (wipe-this-and-yank-previous)
	    &end-code
        &numeric-argument (&pass)
        (mark-paragraph)
        (runoff-fill-region))

(defun pad-line (howmany)
       (do-forever
        (if (< howmany 1)(stop-doing))
        (go-to-beginning-of-line)
        (let ((orig-howmany howmany))
	   (do-forever
	    (if (< howmany 1)(stop-doing))
	    (skip-over-whitespace)
	    (skip-to-whitespace)
	    (if (eolp)(stop-doing))
	    (insert-char SPACE)
	    (setq howmany (1- howmany)))
	   (if (= howmany orig-howmany)
	       (stop-doing)))))

(defun backward-nonwhite-word ()
   (backward-word)
   (do-forever
	(if (bolp)(stop-doing))
	(if (at-white-char)
	    (forward-char)
	    (stop-doing))
	(backward-char)))

;;; These functions implement a paragraph formatting algorithm that chooses
;;; the "optimal" point at which to break each line.  The algorithm is
;;; adapted from the paper by James O. Achugbue in the June 1981 issue of
;;; SIGPLAN Notices.  The implementation of his algorithm and the extensions
;;; necessary to make it viable in the Emacs environment were performed by
;;; Charles R. Davis in August 1981.
;;;

;;;
;;; The size of these arrays is the number of words in the paragraph.
;;;

;(declare (array*
;	 (fixnum (word-length ?))		;Length of each word
;	 (fixnum (spaces-required ?))		;Spaces after this word
;	 (flonum (cost ?))			;Cost function -- cost of
;					;formatting from this word to
;					;the end of the paragraph.
;	 (fixnum (optimal-break ?))))		;Break index associated with
					;minimal cost.

;;;
;;; The size of these arrays is the number of lines in the paragraph.
;;;

;(declare (array*
;	 (fixnum (latest-break ?))		;Latest possible breaking pts
;	 (fixnum (earliest-break ?))		;Earliest breaking points
;	 (fixnum (line-length ?))))		;Length of the line, using
					;the latest breaking points

;;;
;;; The runoff-fill-paragraph command is used to format a single paragraph.
;;;

;(defcom runoff-fill-paragraph
;        &numeric-argument (&pass)
;        (save-excursion			;Don't disturb cursor
;	(mark-paragraph)			;Make paragraph the region
;	(runoff-fill-region)))		;Fill the region

;;;
;;; The runoff-fill-region command is used to format an arbitrary region.
;;;

;(defcom runoff-fill-region
;        &numeric-argument (&pass)
;        (or dont-stash (copy-region))		;Copy, in case of bad results
;        (with-the-mark-last end
;	(with-mark begin
;	  (format-text-in-region begin end))))

;;;
;;; format-text-in-region does all of the hard work for runoff-fill-region.
;;;

;(defun format-text-in-region (begin end)
;       (go-to-mark begin)
;
;       ;; Remove the fill prefix from the lines if it is present.
;
;       (if (> (stringlength fill-prefix) 0)
;	 (remove-fill-prefix end)
;	 (go-to-mark begin))
;
;       ;; Count the words in the paragraph.  Allocate arrays.
;
;       (let ((nwords (count-words end))
;	   (fill-column (effective-fill-column)))
;	  (*array 'word-length 'fixnum nwords)
;	  (*array 'spaces-required 'fixnum nwords)
;	  (*array 'cost 'flonum nwords)
;	  (*array 'optimal-break 'fixnum nwords)
;
;	  ;; Collect word information.
;
;	  (go-to-mark begin)
;	  (collect-words nwords)
;	  (go-to-mark begin)
;
;	  ;; Count lines (minimal number) in paragraph.  Allocate arrays.
;
;	  (let ((nlines (count-lines nwords)))
;	       (if (> nlines 1)
;		 (*array 'latest-break 'fixnum nlines)
;		 (*array 'line-length 'fixnum nlines)
;		 (*array 'earliest-break 'fixnum nlines)
;
;		 ;; Find the latest possible breaking points, and then the
;		 ;; earliest possible breaking points.  The optimal
;		 ;; breaking points lie somewhere between.
;
;		 (find-latest-breaks nwords)
;		 (find-earliest-breaks nlines)
;		 (find-optimal-breaks nlines)
;		 else
;		 (fillarray 'optimal-break '(0)))
;
;	       ;; Reformat the paragraph, using the optimal breaking points.
;
;	       (go-to-mark begin)
;	       (reformat-text nwords nlines)
;
;	       ;; If a fill prefix is defined, preface each line with it.
;
;	       (if (> (stringlength fill-prefix) 0)
;		 (go-to-mark begin)
;		 (insert-fill-prefix end)))))

;;;
;;; remove-fill-prefix removes the fill prefix from each line of the paragraph.
;;;

;(defun remove-fill-prefix (end)
;       (let ((pos (stringlength fill-prefix)))
;	  (do-forever
;	    (if (looking-at fill-prefix)
;	        (go-to-line-point curline pos)
;	        (without-saving (kill-to-beginning-of-line)))
;	    (if (mark-on-current-line-p end) (stop-doing))
;	    (if (lastlinep) (stop-doing))
;	    (next-line))))

;;;
;;; count-words simply returns the number of words in the paragraph.
;;;

;(defun count-words (end)
;       (let ((count 0))
;	  (do-forever
;	    (skip-over-whitespace)
;	    (if (mark-at-current-point-p end) (return count))
;	    (if (point>markp end) (return count))
;	    (skip-to-whitespace)
;	    (setq count (1+ count))
;	    )))

;;;
;;; effective-fill-column computes the number of columns available for the
;;; text of the paragraph, taking into consideration the fill prefix.
;;;

;(defun effective-fill-column ()
;       (if (= (stringlength fill-prefix) 0)
;	 fill-column
;	 else
;	 (go-to-beginning-of-line)
;	 (insert-string fill-prefix)
;	 (prog1 (- fill-column (cur-hpos))
;	        (kill-to-beginning-of-line))))

;;;
;;; collect-words collects the word-length and spaces-required information for
;;; every word in the paragraph.
;;;

;(defun collect-words (nwords)
;       (let ((begin-pos (cur-hpos)))
;	  (skip-over-whitespace)
;	  (skip-to-whitespace)
;	  (collect-one-word begin-pos 0)
;	  (do ((i 1 (1+ i)))
;	      ((= i nwords))
;	      (skip-over-whitespace)
;	      (setq begin-pos (cur-hpos))
;	      (skip-to-whitespace)
;	      (collect-one-word begin-pos i))))

;;;
;;; collect-one-word collects the word-length and spaces-required information
;;; for a single word.
;;;

;(defun collect-one-word (begin-pos wordno)
;       (let ((len (- (cur-hpos) begin-pos))
;	   (spaces 1))
;	  (if (> len fill-column)
;	      (display-error "The fill column is too small."))
;	  (store (word-length wordno) len)
;	  (and (memq (lefthand-char) '(/. /? /!))
;	       (setq spaces (1+ spaces)))
;	  (store (spaces-required wordno) spaces)))

;;;
;;; count-lines counts the number of lines required by the paragraph.  It
;;; simulates the use of the latest breaking indices, so the number of lines
;;; it computes is minimal.
;;;

;(defun count-lines (nwords)
;       (do ((i 1 (1+ i))
;	  (lines 1)
;	  (len (word-length 0)))
;	 ((= i nwords) lines)
;	 (setq len (+ len (spaces-required (1- i)) (word-length i)))
;	 (if (> len fill-column)
;	     (setq lines (1+ lines))
;	     (setq len (word-length i)))))

;;;
;;; find-latest-breaks determines the latest breaking points.  (latest-break n)
;;; is the number of the word with which line n begins using the latest
;;; breaking points.  The latest breaking points are determined by trying to
;;; put as many words as possible on one line before going to the next.
;;; This is the algorithm used by most simple formatters.
;;;

;(defun find-latest-breaks (nwords)
;       (store (latest-break 0) 0)
;       (do ((i 1 (1+ i))
;	  (line 0)
;	  (len (word-length 0)))
;	 ((= i nwords)
;	  (store (line-length line) len))
;	 (setq len (+ len (spaces-required (1- i)) (word-length i)))
;	 (if (> len fill-column)
;	     (store (line-length line)
;		  (- len (+ (spaces-required (1- i)) (word-length i))))
;	     (setq line (1+ line))
;	     (store (latest-break line) i)
;	     (setq len (word-length i)))))

;;;
;;; find-earliest-breaks finds the earliest breaking point for each line.
;;; This is done by essentially running the find-latest-breaks algorithm in
;;; reverse, putting as many words on a line from right to left as possible
;;; before going to the previous line.  The earliest breaking point of the
;;; last line is always set equal to the latest breaking point of that line,
;;; since putting more words on the last (partial) line would only increase
;;; the total white space in the filled portion of the paragraph.
;;;

;(defun find-earliest-breaks (nlines)
;       (store (earliest-break 0) 0)
;       (store (earliest-break (1- nlines)) (latest-break (1- nlines)))
;       (do ((i (- (latest-break (1- nlines)) 2) (1- i))
;	  (line (- nlines 2))
;	  (len (word-length (1- (latest-break (1- nlines))))))
;	 ((< i 0))
;	 (setq len (+ len (word-length i) (spaces-required i)))
;	 (if (> len fill-column)
;	     (store (earliest-break line) (1+ i))
;	     (setq len (word-length i))
;	     (setq line (1- line)))))


;;;
;;; find-optimal-breaks computes the optimal line breaking points.  It uses
;;; the earliest-break and latest-break arrays as input.  This algorithm is
;;; described (although not lucidly) in the SIGPLAN article.
;;;

;(defun find-optimal-breaks (nlines)
;
;       ;; The cost of formatting the last (partial) line is fixed.
;
;       (store (cost (latest-break (1- nlines))) 2.0)
;
;       ;; Loop backwards over the lines in the paragraph.
;
;       (do ((i (- nlines 2) (1- i))
;	  (x))
;	 ((< i 0))
;
;	 ;; x measures the length of the longest string being considered
;	 ;; for the current line.  That is, from the chosen beginning word
;	 ;; to the word before the latest-break of the next line.
;
;	 (setq x (- (line-length i)
;		  (word-length (latest-break i))
;		  (spaces-required (latest-break i))))
;
;	 ;; Loop over the slack in the current line.
;
;	 (do ((j (latest-break i) (1- j))
;	      (y))
;	     ((< j (earliest-break i)))
;	     (setq x (+ x (word-length j) (spaces-required j)))
;
;	     ;; y measures the length of the string being considered for
;	     ;; the current line.
;
;	     (setq y (+ x
;		      (word-length (latest-break (1+ i)))
;		      (spaces-required (1- (latest-break (1+ i))))))
;	     (store (cost j) 99999.0)		;Initialize cost to infinity
;
;	     ;; Loop over the slack in the next line.
;
;	     (do ((k (latest-break (1+ i)) (1- k))
;		(z))
;	         ((< k (earliest-break (1+ i))))
;	         (setq y (- y (spaces-required (1- k)) (word-length k)))
;
;	         ;; If the string under consideration is short enough to fit
;	         ;; within the fill-column, determine the cost of formatting
;	         ;; from this string to the end of the paragraph.
;
;	         (if (or (< y fill-column) (= y fill-column))
;		   (setq z (*$ (+$ 1.0 (//$ 1.0 (float y))) (cost k)))
;
;		   ;; If the cost is less than the minimum cost so far,
;		   ;; update the cost and remember the breaking point
;		   ;; associated with this cost.
;
;		   (if (< z (cost j))
;		       (store (cost j) z)
;		       (store (optimal-break j) k))))))
;
;       ;;; At this point, the optimal breaking points have been computed and
;       ;;; are linked together in the optimal-break array.  This loop brings
;       ;;; them up to the top, so that optimal-break can be indexed by line
;       ;;; number to retrive the optimal breaking points.
;
;       (let ((tem1 (optimal-break 0))
;	   (tem2 0))
;	  (store (optimal-break 0) 0)
;	  (do ((i 1 (1+ i)))
;	      ((= i (1- nlines)))
;	      (setq tem2 (optimal-break tem1))
;	      (store (optimal-break i) tem1)
;	      (setq tem1 tem2)))
;       (store (optimal-break (1- nlines)) (latest-break (1- nlines))))

;;;
;;; reformat-text uses the optimal breaking points to reformat the paragraph,
;;; whose text is still in the buffer.  If the buffer already contains line
;;; breaks at the right places, reformat-text will not disturb them so as to
;;; improve the efficiency of the redisplay.
;;;

;(defun reformat-text (nwords nlines)
;       (skip-over-whitespace)
;       (skip-to-whitespace)
;       (do ((i 1 (1+ i))
;	  (line 1))
;	 ((= i nwords))
;	 (delete-white-sides)
;	 (if (and (< line nlines)
;		(= i (optimal-break line)))
;	     (and numarg (pad-line line))
;	     (if (eolp)
;	         (forward-char)
;	         (delete-white-sides)
;	         else
;	         (insert-char NL))
;	     (setq line (1+ line))
;	     else
;	     (if (eolp)
;	         (delete-char)
;	         (delete-white-sides))
;	     (insert-string (substr SPACES 1 (spaces-required (1- i)))))
;	 (skip-to-whitespace)))

;;;
;;; pad-line inserts extra spaces in a line when adjusting, rather than simple
;;; filling, is requested.  It distributes extra spaces alternating from the
;;; left and right on subsequent lines.  This could be improved.
;;;

;(defun pad-line (line)
;       (prog (needed nbreaks uniform extra)
;	   (setq needed (- fill-column (cur-hpos)))
;	   (and (= needed 0) (return nil))
;	   (setq nbreaks
;	         (- (optimal-break line) (optimal-break (1- line)) 1))
;	   (and (= nbreaks 0) (return nil))
;	   (setq uniform (// needed nbreaks))
;	   (setq extra (\ needed nbreaks))
;	   (save-excursion
;	     (if (oddp line)
;	         (go-to-beginning-of-line)
;	         (do ((i 0 (1+ i))
;		    (n uniform uniform))
;		   ((= i nbreaks))
;		   (skip-over-whitespace)
;		   (skip-to-whitespace)
;		   (if (> extra 0)
;		       (setq n (1+ n) extra (1- extra)))
;		   (insert-string (substr SPACES 1 n)))
;	         else
;	         (do ((i 0 (1+ i))
;		    (n uniform uniform))
;		   ((= i nbreaks))
;		   (skip-back-to-whitespace)
;		   (if (> extra 0)
;		       (setq n (1+ n) extra (1- extra)))
;		   (insert-string (substr SPACES 1 n))
;		   (skip-back-whitespace))))))

;;;
;;; insert-fill-prefix inserts the fill prefix at the beginning of every line
;;; of the paragraph.
;;;

;(defun insert-fill-prefix (end)
;       (do-forever
;         (or (not (bolp)) (line-is-blank) (insert-string fill-prefix))
;         (if (mark-on-current-line-p end) (stop-doing))
;         (if (lastlinep) (stop-doing))
;         (next-line)))

;;;	Horizontal position maintenance

(defun go-to-hpos (hp)
       (prog (curpos)
	   (go-to-beginning-of-line)
	   (setq curpos 0)
	   (return (do-forever
		   (if (not (> hp curpos))(return curpos))
		   (if (eolp)(return nil))
		   (dispatch-on-current-char
		     (BACKSPACE (setq curpos (1- curpos)))
		     (TAB	 (setq curpos
			       (+ curpos
				(- tab-equivalent
				   (\ curpos tab-equivalent)))))
		     (else (setq curpos (1+ curpos))))
		   (forward-char)))))

(defun whitespace-to-hpos (h)			;give whitespace till at h.
       (prog (targ-tabstops targ-rem curhpos cur-tabstops cur-rem)
	   (setq curhpos (cur-hpos))
	   (if (= h curhpos)(return t))
	   (if (< h curhpos)(return t))
	   (if (= h (1+ curhpos))(insert-char SPACE)(return t))
	   (setq targ-tabstops (// h tab-equivalent)
	         targ-rem (\ h tab-equivalent))
	   (setq cur-tabstops (// curhpos tab-equivalent)
	         cur-rem (\ curhpos tab-equivalent))
	   (do-times (- targ-tabstops cur-tabstops)(insert-char TAB))
	   (if (not (= targ-tabstops cur-tabstops))(setq cur-rem 0))
	   (do-times (- targ-rem cur-rem)(insert-char SPACE))))

;;;
;;; Line movement, insertion, and deletion.
;;;

(defcom kill-to-beginning-of-line
        &undo yank
        (if (bolp)
	  else (with-mark m
		        (go-to-beginning-of-line)
		        (kill-forward-to-mark m))))

(defcom kill-contents-of-line
        &undo yank
        (go-to-beginning-of-line)
        (kill-to-end-of-line))

(defkill kill-lines forward)
(defcom kill-lines
        &undo yank
        &numeric-argument (&pass &lower-bound 0)
        (cond ((not numarg)
	     (cond ((eolp)
		  (or dont-stash (killsave-string NL))
		  (merge-line))
		 (t (kill-to-end-of-line))))
	    (t (do ((j numarg (1- j)))
		 ((= j 0)
		  (or dont-stash
		      (killsave-string (catenate (kill-pop) NL))))
		 (kill-to-end-of-line)
		 (or dont-stash (= j numarg)	;merge kills
		     (let ((first (kill-pop))
			 (second (kill-pop)))
			(killsave-string
			  (catenate second NL first))))
		 (merge-line))))
        (merge-kills-forward))

;;;
;;; Yank commands.
;;;

(defcom yank
        &undo wipe-region
        &numeric-argument (&pass &lower-bound 0)
        (let ((n (or numarg 1)))
	   (and (> n kill-ring-current-size)
	        (report-error 'empty-kill-ring))
	   (do ((i 1 (1+ i))
	        (ybp kill-ring (cddr ybp)))
	       ((not (< i n))
	        (set-the-mark)
	        (insert-string (car ybp))))))

(defcom wipe-this-and-yank-previous
        &numeric-argument (&reject)
        (or der-wahrer-mark (display-error "There was no previous yank."))
        (let ((what-to-wipe (point-mark-to-string der-wahrer-mark)))
	   (cond ((samepnamep (kill-ring-top) what-to-wipe)
		(without-saving (wipe-region))
		(rotate-kill-ring)
		(insert-string (kill-ring-top)))
	         (t (display-error "Yank region doesn't match kill ring.")))))

;;;
;;;
;;;	Random visible command functions
;;;

(declare (special track-eol-opt))
;;;(register-option 'track-eol-opt nil) ;;; moved to e_option_defaults_

(defcom next-line-command
        &undo prev-line-command
        &numeric-argument (&pass)
        &negative-function prev-line-command
        (or (memq previous-command '(next-line-command prev-line-command))
	  (setq target-screen-hpos (cond ((bolp) 0)  ;for 0-char case
				   ((and (eolp) track-eol-opt) 'eol)
				   (t (cur-screen-hpos)))))
        (cond (numarg (do n numarg (1- n)(= 0 n)
		      (cond ((lastlinep) (return nil)))
		      (next-line)))
	    ((lastlinep) (cond (macro-execution-in-progress (command-quit)))
		       (add-new-empty-line) (next-line))
	    (t (next-line)))
        (cond ((eq target-screen-hpos 'eol) (go-to-end-of-line))
	    (t (go-to-screen-hpos target-screen-hpos))))


(defcom prev-line-command
        &undo next-line-command
        &numeric-argument (&pass)
        &negative-function next-line-command
        (or (memq previous-command '(next-line-command prev-line-command))
	  (setq target-screen-hpos (cond ((bolp) 0)  ;for 0-char case
				   ((and (eolp) track-eol-opt) 'eol)
				   (t (cur-screen-hpos)))))
        (cond (numarg (do n numarg (1- n)(= 0 n)
		      (cond ((firstlinep) (command-quit)))
		      (prev-line)))
	    (t (prev-line)))
        (cond ((eq target-screen-hpos 'eol) (go-to-end-of-line))
	    (t (go-to-screen-hpos target-screen-hpos))))


(defcom new-line
        (cond ((eq minibufferp NL)(jetteur-des-gazongues))
	    ((not (eolp))(insert-char NL))
	    ((lastlinep)(insert-char NL))
	    ((e_lap_$compare_strings (eline-contents (nextline)) 0
			         NLCHARSTRING 0 1)
	     (next-line)
	     (cond ((lastlinep))
		 ((e_lap_$compare_strings (eline-contents (nextline)) 0
				      NLCHARSTRING 0 1))
		 (t (insert-char NL)(backward-char))))
	    (t (insert-char NL)))
        (or (= 0 (stringlength fill-prefix))(insert-string fill-prefix)))

(declare (special gratuitous-marks))
;;;(register-option 'gratuitous-marks nil) ;;; moved to e_option_defaults_

(defun gratuitous-mark-setter lexpr
       (cond ((or minibufferp (not gratuitous-marks)))
	   ((zerop lexpr) (set-or-pop-the-mark))
	   ('else (set-the-mark-here (arg 1)) (minibuffer-remark "Set."))))

(defcom go-to-beginning-of-buffer
        &undo-function go-to-end-of-buffer
        &prologue gratuitous-mark-setter
        (go-to-line-point firstline 0))

(defcom go-to-end-of-buffer
        &undo-function go-to-beginning-of-buffer
        &prologue gratuitous-mark-setter
        (go-to-line-point lastline 0)
        (go-to-end-of-line))

(defcom go-to-beginning-of-line
        &undo-function go-to-end-of-line
        (setq curpointpos 0))

(defcom go-to-end-of-line
        &undo-function go-to-beginning-of-line
        (setq curpointpos (1- curlinel)))

;;;  Debugging functions

(defprop %dccl display-cursed-current-line expr)

(defun display-cursed-current-line ()
       (let ((str (curline-as-string)))
	  (princ (substr str 1 curpointpos))
	  (princ "<")(tyo 10)(princ ">")
	  (princ (substr str (1+ curpointpos)))))

(defun %dbp () (print (list (cond ((curline-openp) 'open)(t 'closed))
		       (curline-as-string))))


