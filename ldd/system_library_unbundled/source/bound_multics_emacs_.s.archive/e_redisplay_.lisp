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
;;;	Multics EMACS Redisplay


;;; HISTORY COMMENTS:
;;;  1) change(84-01-19,Margolin), approve(), audit(), install():
;;;     pre-hcom history:
;;;               Greenberg, March 1978
;;;               3/6/78 inceptus Luna meo adjutorio.
;;;               4/19/78 duas fenestras feci.
;;;               5/30/78 ^V creavi.
;;;               6/18/78 signum linearum elongatarum, ^0^L, &c
;;;               7/5/78  Cuncta lineae comparandae sunt, quicumque sint.
;;;               7/27/78 Ostendae sunt lineae quae non in textu sunt.
;;;               8/23/78 Dua fenestrae tacebant, atque mundae factae erant.
;;;               9/6/78 Indices linearum originalum per fenestris comparo.
;;;               3/1/79 Quando laboro in medio linearum elongatarum, omnes moveatur.
;;;               4/4/79 Minibuffer in multos divisus est.
;;;               4/12/79 Mille fenestrae florent.
;;;               8/24/79 ^V et ESC-V argumentes dedi.
;;;               Septembri 1979 hoc redisplicator Paltere sustenetur.
;;;               2/12/80 tty-no-cleolp impletur,
;;;                         mode-line-hook & local-display-end-string
;;;               10/23/80 Praefix minibufferis non delendum est.
;;;       1980 Decembri e manibus meis dimissi te ut sole per mundum ambules.
     
;;;               Welcome to the rosy-fingered dawn of the New Era:
;;;               Presenting, at popular demand;
;;;               A Comment In English!
     
;;;               30 June 1981 Extending local displays, Richard Mark Soley
;;;               1 July 1981 suppress-remarks and minibuffer-clear-all, Richard Soley
;;;               5 November 1981 truncate overlength modelines, Richard Soley
;;;               19 August 1982 fixed inverse-real-world-xcoord for \c lines,
;;;                              Barry Margolin
;;;               20 August 1982 added CAH's real underlining code, Barry Margolin
;;;               12 October 1982 modified underlining to use constant 400, Barmar
;;;               3 December 1983 changed redisplay-this-line to call
;;;                               randomize-redisplay first, Barmar.
;;;               19 January 1984 commented out register-option forms, as they were
;;;  2) change(84-12-25,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Changed some "'"s to ";"s, moved first reference to
;;;     realcurdispline to after wman-init is called,
;;;     slashified #'s, changed lambda's to let's.
;;;  3) change(84-12-26,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added initializations of realcurdispline,
;;;     rdis-have-redisplayed, and last-curline, moved %include's to before
;;;     declares, replaced %include of e-define-command
;;;     with e-macros, replaced "(declare (special" with
;;;     "(defvar".
;;;  4) change(84-12-27,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Fixed redisplay to not refer out of the screen
;;;     array when a minibuffer response goes beyond the
;;;     last screen line.
;;;  5) change(84-12-30,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Moved most variable initializations from display-init
;;;     to load-time defvar.
;;;  6) change(84-12-31,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Unquoted throw/catch tag in rdis-forward-backward-screen.
;;;  7) change(85-02-03,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Moved phony-(mode path)line-edline
;;;     initializations back to display-init, because macros
;;;     are not expanded by lcp at top-level.
;;;  8) change(88-01-14,Schroth), approve(88-02-29,MCR7851),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     To implement the display of 8-bit Extended ASCII on qualified
;;;     terminals.
;;;  9) change(88-01-14,Schroth), approve(88-02-29,MCR7852),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Implemented vertically split screen windows.
;;; 10) change(89-03-10,Flegel), approve(89-04-03,MCR8088),
;;;     audit(89-04-06,Lee), install(89-05-10,MR12.3-1042):
;;;     phx20884 - removed decrementation of line_length (args:ll)
;;;                                                      END HISTORY COMMENTS


(declare (genprefix /!rdis_))

(%include emacs-rdis-dcls)
(%include emacs-internal-macros)
(%include e-macros)
(%include other_other)

(declare (*lexpr display-error display-com-error display-error-noabort
	       display-com-error-noabort minibuffer-print
	       minibuffer-print-noclear))
(declare (*expr DCTL-assert-scpos DCTL-clear-rest-of-screen DCTL-clear-screen
	      DCTL-create-split DCTL-delete-chars DCTL-delete-lines
	      DCTL-destroy-split DCTL-display-char-string
	      DCTL-init DCTL-insert-char-string DCTL-insert-lines
	      DCTL-kill-line DCTL-nextline DCTL-position-cursor
	      DCTL-ring-tty-bell DCTL-scroll-down-region
	      DCTL-scroll-up-region DCTL-select-split DCTL-write-abort
	      Rtyo charscan-table
	      command-quit curline-as-string decimal-rep e$get_temporary_seg
	      e_lap_$compare_strings e_lap_$ggcharn e_lap_$gsubstr
	      e_lap_$insert-chars e_lap_$make-dat-ol-black-magic-string
	      e_lap_$rdis-crossmatch e_lap_$rplacstring
	      e_lap_$rplacstring-offset e_lap_$tabscan_table e_lap_$tct
	      e_pl1_$dump_output_buffer e_pl1_$get_line_speed 
	      e_pl1_$resetwrite emacs$set_lisp_rdis_meters get-char
	      go-to-end-of-buffer go-to-line-point
	      rdis-reallocate-screen-evenly rdis-upd-virtual-window-point
	      rdis-update-window-struct wman-init))
(defvar  (

;;;	DCTL args

	 X Y

;;;	PL/1 pre-parsed arguments

	 args:pl				;page length
	 args:ll				;line length

;;;	TTY parameters, Defaulted by display-init,
;;;	set by specific DCTL-init
;;;
	 idel-chars-availablep	;Insert-delete chars is available.
	 idel-lines-availablep	;Insert/delete lines is available.
	 region-scroll-availablep	;VT100-like scrolling available.
	 tty-no-cleolp		;No clear to EOL, rdis must simulate.
	 tty-no-upmotionp		;Glass or printing, can't move crsr up.
	 DCTL-prologue-availablep	;Terminal must be twiddled whenever Emacs
				;tty modes are set after initialization.
	 DCTL-epilogue-availablep	;Terminal must be twiddled whenever Multics
				;tty modes are set
	 DCTL-underline-mask	;the terminal can really underline

	 (DCTL-extended-ascii nil)	;terminal can do 8bit

	 (DCTL-hardware-windows-availablep nil)
				;does terminal support windows?
	 (DCTL-max-splits 8.)	;number of windows it supports

	 screenheight		;Number of lines of screen depth.
	 screenlinelen		;Length of line - 1.
	 overstrike-availablep	;Cant rewrite one character anew
				;by writing over it.
	 (tty-type 'ASR33)		;tty ctl name, has tintinnab..* prop
				;if cant ring own bell.
	 tty-eolch-lossp		;DD4000 screw flag meaning all
				;whitespace must be printed in full.
	 abort-availablep		;Writeabort provided.  MCS doesn't work
				;right, so nobody has or can have this.

;;;
;;;	Editor (e_) state variables defining current buffer and
;;;	position therein.

	 curline			;Editorline object for current line
	 curlinel			;# of valid characters therein, incl NL
	 curstuff			;The line-data for that line, an emacs
	 			;string, see e_lap_.
	 curpointpos		;# of chars to left of "." on curline,
				;i.e., virtual buffer cursor hpos.
	 current-buffer		;Current buffer, atomic symbol.
	 firstline		;for minibuffer hack 10/23/80

	 ;; These buffer vars are used only for mode line maintenance.

	 fpathname		;Buffer file pathname
	 current-buffer-mode	;Current buffer's major mode.
	 buffer-minor-modes		;List of minor modes in this buf.
	 buffer-modified-flag	;Non-nil if buffer has been modified
	 read-only-flag		;Non-nil if buffer can't  be modified
	 user-display-variable	;Random var for R. Lamson.


;;;
;;;	The images of these buffer state variables kept from
;;;	one redisplay to the next so the Redisplay can determine
;;;	what changed.

	 (last-curline nil)		;Image of curline
	 (last-curlinel 0)		;Image of curlinel
				;e_ knows about this, to manage
				;the damaged-flag.
	 (last-buffer (gensym))	;Image of current-buffer
	 (last-fpath nil)		;Image of fpathname
	 last-bufmode		;Image of current-buffer-mode
	 (last-minor-modes nil)	;Image of buffer-minor-modes
	 (last-modified-flag nil)	;Image of buffer-modified-flag
	 (last-read-only-flag nil)	;Image of read-only-flag
	 (last-udv nil)		;Image of user-display-variable


;;;
;;;	Other editor (e_) randomness needed by Redisplay.
;;;

	 NLCHARSTRING		;An ASCII newline as a string
	 TAB			;An ASCII TAB as a string.
	 work-seg			;Multics ptr to scratch string hack seg
	 touchy-damaged-flag	;Turned off by redisplay, turned on
				;by editor if ANY modification done.
	 damaged-flag		;Turned off by redisplay, turned on by
				;editor if any mod done except adding
				;chars to the end of last-curline.
	 minibufferp		;non-nil if user typing in minibuf.
	 minibuffer-prompt-string	;like he says
	 tab-equivalent		;Number of spaces a tab is worth.
	 numarg			;numeric argument, for commands.

	 ;; Communications to Redisplay.

	 (suppress-redisplay-flag nil) ;Editor or extension wants all
				;redisplay suppressed.
	 (rdis-suppress-redisplay nil) ;Editor wants 1 redisplay suppressed.
				;Also used by local displays.
	 (rdis-upd-locecho-flg nil)	;PL/I negotiated redisplay occurred,
				;tells redisplay to do dummy redisplay
				;updating data with no output.
	 e-quit-transparency	;In the middle of QUIT/break processing
	 (rdis-inhibit-echnego nil)	;Needed by prtty hacks 11/6/79

	 ;; And communications back from.

	 two-window-mode		;In two-window mode.
	 selected-window		; uwindowx of selected window

	 pop-up-windows		;automatic window creation if on

;;;
;;;	User hooks, mainly for the benefit of Webber Emacs Applications, Inc.

	 (mode-line-hook nil)	;fcn to call for mode line
	 (local-display-end-string	;like it says
	   "-- * * * * * * * * * * * * --")
	 mode-line-herald
	 current-minor-mode-display-string ;<fill,electric> for hookfn.

;;;
;;;	Redisplay options.
;;;

	 screen-overlap		;number of lines overlap ^V, esc-V
	 (rdis-csopt nil)		;Clear to eos when rest of window n/g.
	 (rdis-wosclr-opt nil)	;Clear to eol on rest of window first
				;when rest of window n/g.
	 display-ctlchar-with-^	;Show chars as ^L instead of \014.
	 suppress-ctlchar-display	;Don't show control chars at all.
	 suppress-backspace-display	;Don't show backspaces at all.
	 suppress-rubout-display	;Don't show rubouts (\177) at all.
	 rdis-whitespace-optimize	;Attempt to print internal whitespace
				;by cursor movement.
;;;
;;;	Meters.
;;;

	 (rdis-rdis-meter 0)	;Calls to redisplay.
	 (full-rdis-meter 0)	;Times eol-optimized rdis didnt do it.
	 (rdis-detabb-meter 0)	;Number of output conversions.
	 (rdis-detab-opt-meter 0)	;Times found old output conv ok.
	 (rdis-wgen-meter 0)	;Number of windows computed.
	 (rdis-wgen-c1-meter 0)	;Of those, cursor found in old window.
	 (rdis-wgen-c2-meter 0)	;New window chosen centered on cursor.
	 (rdis-wgen-c3-meter 0)	;Window started from cursor at top.
	 (rdis-ndf-opt-meter 0)	;c1 window with no mod/detab at all.
	 (rdis-bad-echnego-meter 0)	;had to do full rdis after echnego.

;;;
;;;	Now comes all the internal hair.
;;;

	 rdis-multiwindowed-buflist	;List of bufs in many windows
	 rdis-lru-stack		;Window stack, top is mru.
	 modelwindow		;Mode line/path line window.
	 minibufwindow		;The minibuffer/echo buffer.

	 nwindows			;Number of these to look for.
	 nuwindows		;Number of user windows
;;;
;;;	Some constants and semi-constants
;;;

	  main-window-size		;# of lines in main screen region,
				;what set-screen-size sets.
	  rwork-string		;An "old black magic" (rplacable)
				;string used to hold converted image
				;of current real screen line, extended
				;by optimized redisplays, but updated
				;by all.
	  detab-buf		;blk mgc string used for output cvsn
				;and substr-ing output strings.
	  rdis-a-lotta-blanks	;a line's worth of blanks, for
				;comparing to in redisplay-line
	  ospeed			;Chars/sec from MCS, a public var.
	  (rdis-blankscan-table nil)	;e_-style EIS table for whitespace
				;optimizer's non-blank searching.
	  rdis-splln-mark		;Mark for split-line "window"



;;;
;;;	Vars holding editorlines which comprise the phony editor
;;;	buffer which is in fact the modeline and pathline.
;;;

	  phony-modeline-edline	;The mode line, as an editorline.
	  phony-pathline-edline	;The path line, as an editorline.

;;;
;;;	Items of internal redisplay state.
;;;

	  (realcurdispline '(("" nil) "" . 0)) ;The cursor-containing displayline
				;of the last redisplay.  Its
				;printablerepresentation IS rwork-strg.
	   rdis-selected-windowx	;window index of cursor window
	   rdis-selected-wlist	;The window structure for that window.
	   rdis-selected-split	;the split for that window
	   last-foundx		;Y coord of eline having cursor
	   real-foundx		;Actual screen line where cursor left

	   last-prinl		;Printing length of curdispline,
				;used to add optimally to end of it.
	   (rdis-last-tty-upprint-x -1) ;The last Y coord on simulated
				;no-upm "tty" screen where simulated
				;cursor was left.
	   rdis-last-echolinex	;last place where minibuffers printed.
	   (rdis-mbuf-transient-linex -1) ;line that wants to die.
	   some-split-damaged	;a split (possibly) other than the current
				; one needs update, means hard redisplay
;;;
;;;	Variables used by the local display hack.
;;;

	   (rdis-locdisp-linex nil)	;Next line coord where local display
				;to go. nil = regular rdis went by.
	   rdis-locdisp-window	;Fraudulent window structure for
				;local displays.

	   (rdis-have-redisplayed nil) ;Remember if a redisplay has been
				;done since last local display.
	   (local-display:force-no-more nil)	;local display teriminion control
	   (rdis-locdisp-split nil)	;split in which the local display is placed

;;;
;;; Variables used by split management
;;;

	   (nsplits 0)		;number of splits in use
	   (nusplits 0)		;# of user addressable splits

	   (maxsplits 0)		;maximum number of splits
	   (maxusplits 0)		;maximum ... of user addressable splits

	   (current-split nil)	;the split we are currently munching
				;changes when we select-window
	   (rdis-cursor-split nil)	;split display cursor is in (not necessarily eq current-split
				; as we don't want the cursor to dance)
				;changes when we actually redisplay in a split
	   (model-split nil)	;split for the mode-line/pathline window
	   (minibuffer-split nil)	;split for the minibuffer

	   (split-mode-p nil)	;on if in split mode.

	   (full-screenlinelen 0)	;so we know how big the REAL screen is
	   (full-screenheight 0)

	   (split-ids-available nil)	;list of split-id values not in use
				;in range 0..DCTL-max-splits -1

         ))

;;; tct arrays of non-printing chars. A non-zero byte ==> non-printing.
;;;(declare (array* (fixnum (rdis-tabscan-table 128.)    ;table in use. ONE OF:
;;;		        (7bit-tabscan-table 128.)    ;7bit ASCII table
;;;		        (8bit-tabscan-table 128.)))) ;8bit ASCII
(declare (special rdis-tabscan-table		;the one in use
	        7bit-tabscan-table		;default is for 7bit ASCII
	        8bit-tabscan-table		;this one   for 8bit ASCII
	        ))

;;; 
;;; (register-option 'rdis-wosclr-opt nil)		;11/23/78 sorry, Olin. -b ;moved to e_option_defaults_
;;; (register-option 'display-ctlchar-with-^ nil) ;moved to e_option_defaults_
;;; (register-option 'suppress-ctlchar-display nil) ;moved to e_option_defaults_
;;; (register-option 'suppress-backspace-display nil) ;moved to e_option_defaults_
;;; (register-option 'suppress-rubout-display nil) ;moved to e_option_defaults_
;;; (register-option 'rdis-whitespace-optimize t) ;made t 9/12/80 -- BSG ;moved to e_option_defaults_
;;; (register-option 'screen-overlap 1) ;moved to e_option_defaults_

;;;	Screen is maintained as the array "screen", containing knowledge
;;;	and images of screen. Each element of "screen" is called a "displayline",
;;;	and looks like this:

;;;	(editorline "printablerepresentationwithnonewline" . printinglength)

;;;	The array "newscreen" is used during redisplay computation only.
;;;
;;;	The array "windows" contains "windowlists" for each extant window,
;;;	indexed from 0.  The "---" lines, the modeline/pathline, and the
;;;	minibuffer all count as windows.

;;;	The representation of a window (a window-list) is as follows:

;;;	(firstline# #of-lines pointelinemark buffersymbol)

;;;	"pointelinemark" is an editor "mark" representing the last place
;;;	that "point" was in that buffer. "buffersymbol" is the buffer symbol.
;;;
;;;	The array "uwindows" (1-originned) contains indices into
;;;	windows indexed by what the user sees as "windows", i.e.,
;;;	editing, non-overhead windows.

;;;	The array "eline-conts" parallels the window array of redisplay lines
;;;	maintaining what e_ calls "line-contents" so that an "eq" check can
;;;	be made (see redisplay-window) to avoid detabbification and resultant
;;;	consing, for eq lines with eq contents cannot detabbify differently.

;;;	Setup redisplay at start of editor invocation.

(defun display-init ()

       ;; Default DCTL flags.
       (setq DCTL-prologue-availablep nil
	   DCTL-epilogue-availablep nil
	   DCTL-underline-mask nil
	   DCTL-extended-ascii nil		;no 8-bit yet
	   DCTL-hardware-windows-availablep nil
	   overstrike-availablep nil
	   tty-eolch-lossp nil
	   abort-availablep nil
	   idel-lines-availablep nil
	   idel-chars-availablep nil
	   tty-no-cleolp nil
	   region-scroll-availablep nil
	   tty-no-upmotionp nil)

       ;; Initialize redisplay work variables and options.
       (setq rwork-string (e_lap_$make-dat-ol-black-magic-string
		        (e$get_temporary_seg))
	   detab-buf (e_lap_$make-dat-ol-black-magic-string
		     (e$get_temporary_seg))
	   ospeed (e_pl1_$get_line_speed))

       ;; Tell the terminal controller to initialize itself.
       (DCTL-init)

       ;; Force values for line/page length from command line.
       (and (> args:ll 0) (setq screenlinelen args:ll)) ;;MF phx20884
       (and (> args:pl 0) (setq screenheight args:pl))

       ;; Keep true screen size safe from split munging
       (setq full-screenlinelen screenlinelen
	   full-screenheight screenheight)

       ;; Check up behind the terminal controller, setting unset options.
       (and tty-no-upmotionp (not overstrike-availablep)
	  (setq tty-no-cleolp t))
       (and tty-no-cleolp
	  (putprop 'DCTL-kill-line 'rdis-kill-eol-writearound 'expr))
       (and (getl 'DCTL-ring-tty-bell '(subr expr))
	  (putprop tty-type t 'tintinnabulum-ipsum-meum-sono))

       ;; Initialize screen arrays.
       ;; changed to array pointers from named arrays EDS Aug/85
       ;; We use both named arrays and array pointers for these arrays to allow
       ;; existing applications (some CTLs) to use old references.
       ;; The named array and array pointer are only guarenteed EQ if not
       ;; in split-screen mode (ie. (not split-mode-p))
       (setq screen (*array 'screen t screenheight))
       (setq newscreen (*array 'newscreen t screenheight))
       (setq eline-conts (*array 'eline-conts t screenheight))
       (fillarray newscreen '(nil))
       (fillarray eline-conts '(nil))

       ;; Establish proper non-printing char tabscan table
       (setq rdis-tabscan-table
	   (cond (DCTL-extended-ascii 8bit-tabscan-table) ;8bit ASCII?
	         (t 7bit-tabscan-table)))	;normal ASCII
       
       ;; Armed with tty info, finish setting redisplay variables.
       (setq rdis-a-lotta-blanks (do ((c "         " (catenate c c)))
			       ((> (stringlength c) screenlinelen) c)))
       (randomize-redisplay)

       ;; Initialize window manager, mode line, and minibuffer.

       (setq phony-modeline-edline	;The mode line, as an editorline.
	   (make-eline contents "")
	   phony-pathline-edline	;The path line, as an editorline.
	   (make-eline contents ""
		     previous phony-modeline-edline))	;prevline is mode line
       (setf (eline-next phony-modeline-edline)
	   phony-pathline-edline)		;nextline is path line
       (wman-init)
       (setq rdis-last-echolinex (startline minibufwindow))
       (and tty-no-upmotionp (setq DCTL-hardware-windows-availablep nil))
       (init-split-management)
       (update-mode-line)
       (reset-minibuffer-size))

(defun randomize-redisplay () (setq X -777 Y -777 rdis-suppress-redisplay nil))

;;;
;;;	External utilities and interfaces.
;;;

(defun redisplay-command ()
       (cond ((null numarg)
	    (full-redisplay))
	   (t (redisplay-current-window-relative numarg))))

(defun full-redisplay ()
       (randomize-redisplay)
       (setq damaged-flag t)
       (setq last-minor-modes (ncons nil) last-fpath (ncons nil))	;force prtty update.
       (and abort-availablep (rdis-write-abort))
       (clear-the-screen)
       (redisplay))

(defcom redisplay-this-line
        &numarg (&pass)
        (setq numarg (or numarg 1))
        (and (zerop numarg) (setq numarg 1))
        (let ((old-y Y)
	    (end (min screenheight
		    (cond ((minusp numarg) (1+ Y))
			(t (+ Y numarg))))))
	   (randomize-redisplay)
	   (do ((y (cond ((minusp numarg) (max 0 (+ old-y numarg)))
		       (t old-y))
		 (1+ y)))
	       ((= y end))
	       (DCTL-position-cursor 0 y)
	       (DCTL-kill-line)
	       (redisplay-line (screen y) (hokeup-line "") y))))

(defun toggle-redisplay ()
       (setq suppress-redisplay-flag (not suppress-redisplay-flag)))	;sigh

(defun rdis-write-abort ()			;This doesnt work at all
       (and (eq abort-availablep 'resetwrite)(e_pl1_$resetwrite))
       (DCTL-write-abort))			;because MCS throws away what
					;you haven't even written yet

(defun clear-the-screen ()
       (fillarray screen '(nil))		;full screen & current split
       (fillarray eline-conts '(nil))
       (randomize-redisplay)			;for general utility.
       (rdis-wipe-screen)
       (and split-mode-p			;splits other than current
	  (do ((splitix 0 (1+ splitix)) (split))
	      ((= splitix nsplits))
	      (setq split (splits splitix))
	      (setf (split-damaged split) t)	;make redisplay see it
	      (cond ((not (eq split current-split))  ;current done above
		   (fillarray (split-screen split) '(nil))
		   (fillarray (split-eline-conts split) '(nil))
		   (rdis-select-split split)
		   (rdis-wipe-screen)))))
       (e_pl1_$dump_output_buffer))
		     
(defun rdis-wipe-screen ()
       (cond ((get 'DCTL-clear-screen 'subr)
	    (DCTL-clear-screen))
	   (t (DCTL-position-cursor 0 0)
	      (DCTL-clear-rest-of-screen))))

;;; Does printing character check using the 'correct' scan table.
;;; Added Dec 84 by EDSchroth for 8bit I/O
;;; Changed Apr 85 to use rdis-tabscan-table and macro-ize.
(defmacro rdis-tabscan (strg strgl strgx)
	`(e_lap_$tabscan_table rdis-tabscan-table ,strg ,strgl ,strgx))

(defun rdis-find-non-displayable (strg strgl strgx)
       ;; 0-index of first non-displayable char in strg or strgl-strgx if none.
       ;; strgl is length of interest, strgx is offset to start at.
       (rdis-tabscan strg strgl strgx))

;;; Fake e_lap_ entry point for compatibility with existing code
(defun e_lap_$tabscan (strg strgl strgx)
       (rdis-tabscan strg strgl strgx))

;;;
;;;
;;;	Main redisplay function, invoked to update screen however
;;;	necessary, from editor.
;;;

(defun redisplay ()
       (let ((e-quit-transparency e-quit-transparency))
	  (prog (foundsw extend extendl target-hpos hisline model-needs-update
		       curwindow curwindow-split)
	        (cond ((or suppress-redisplay-flag rdis-suppress-redisplay)	;hold the old horses?
		     (setq rdis-suppress-redisplay nil)
		     (return t))
		    ((and minibufferp
			(not (eq e-quit-transparency 'redisplaying)))
		     (setq e-quit-transparency 'redisplaying)
		     (setq rdis-have-redisplayed t)
		     (return (cond ((eq curline firstline)
				(let ((curstuff (catenate minibuffer-prompt-string curstuff))
				      (curpointpos (+ curpointpos (stringlength minibuffer-prompt-string)))
				      (curlinel (+ curlinel (stringlength minibuffer-prompt-string))))
				     (redisplay)))
			         (t (let ((realness (eline-contents firstline)))	; < (king elegance)
				       (unwind-protect
				         (progn (setf (eline-contents firstline)
						  (catenate minibuffer-prompt-string realness))
					      (redisplay))
				         (setf (eline-contents firstline) realness))))))))
	        (aos rdis-rdis-meter)		;count 'em
	        (setq e-quit-transparency 'redisplaying)	;Don't let quits
					;play with the screen!
	        (setq curwindow (cond (minibufferp minibufwindow)
				(t (rdis-update-window-struct)
				   rdis-selected-wlist))
		    curwindow-split (window-split curwindow))
	        
	        (setq rdis-have-redisplayed t)

	        (or (and (eq last-fpath fpathname) (eq last-buffer current-buffer)
		       (eq last-bufmode current-buffer-mode)
		       (eq last-minor-modes buffer-minor-modes)
		       (eq last-modified-flag buffer-modified-flag)
		       (eq last-read-only-flag read-only-flag)
		       (eq last-udv user-display-variable))
		  (prog2 (setq model-needs-update t)
		         (update-mode-line)))
;;;

;;;	Determine if simple add-to-end-of-current line hack will do.
;;;	Avoid redisplaying ANY windows if so.  This hack MUST be invoked
;;;	if the PL/I negotiated echo was used, ring 0 or otherwise.
;;;

	        (cond ((and (not (or damaged-flag    ;current split does not need update
			         some-split-damaged)) ;no inactive split needs update
			(eq last-curline curline)	;sheer insertery will do
			(not model-needs-update) ;unless mode line also changed
			(not (rdis-curline-multiwindowed)))	;or curline appears more than once
		     (setq foundsw last-foundx)
		     (cond ((> (setq extendl (- (1- curlinel) last-curlinel)) 0)
			  (cond ((= extendl 1)(setq extend (ascii (e_lap_$ggcharn (wwtcomp curline) last-curlinel))))
			        (t (setq extend (e_lap_$gsubstr (wwtcomp curline) last-curlinel extendl))))

;;; Look for any characters that require special output conversion.

			  (cond ((or (not (= extendl (rdis-tabscan extend extendl 0)))
				   (> (+ last-prinl extendl) screenlinelen))  
			         (setq damaged-flag t)
			         (return (redisplay))))
			  (or rdis-upd-locecho-flg	;the 'tty' did it.
			      (progn
			        (DCTL-position-cursor last-prinl real-foundx)
			        (DCTL-display-char-string extend)))
			  (setq last-prinl (+ last-prinl extendl))
			  (rplac-lineln realcurdispline last-prinl)
			  (e_lap_$rplacstring rwork-string extend extendl (- last-prinl extendl) last-prinl)))
		     (go set-new-state)))

;;;
;;;
;;;	Redisplay all windows.
;;;
	        (and rdis-upd-locecho-flg (aos rdis-bad-echnego-meter))

	        (aos full-rdis-meter)

	        (and model-needs-update tty-no-upmotionp (not (eq fpathname last-fpath)) fpathname
		   (redisplay-line (parameterize-line phony-pathline-edline) nil
			         (1+ (car minibufwindow))))  ;noupm path update.


	        (and split-mode-p		;leave echo-buffer
		   (eq current-split minibuffer-split)	; if in minibuffer split
		   (not minibufferp)	;           but not minibuffer response
		   (rdis-open-split curwindow-split))

	        (do ((splitix 0 (1+ splitix))	;update each split
		   (s)			;split being looked at
		   (prior-current-split current-split)) ;remember current split
		  ((= splitix nsplits))
		  (setq s (splits splitix))
		  (cond ((or (split-damaged s)     ;but only if needed
			   (eq prior-current-split s))     ;know current split needs update!
		         (and split-mode-p (rdis-open-split s))
		         (do ((windowx 0 (1+ windowx))
			    (window))
			   ((= windowx nwindows))
			   (setq window (windows windowx))
			   
			   (and model-needs-update (eq window modelwindow) (setq damaged-flag t))
			   ;inhibit opt of second type, editor didn't hit damaged flag.
			   (cond ((and window
				     (or (and (bufmark window)(not minibufferp))
				         
				         ;;during minnybuffs, dont redisplay
				         ;;any window 'xcept m.b., cause finding
				         ;;starting line is hard.
				         
				         (eq window curwindow))
				     (not (and tty-no-upmotionp (eq window modelwindow)(not model-needs-update))))
				
				(setq hisline (redisplay-window
					      window
					      (cond ((eq window curwindow)
						   curline)
						  (t (car (bufmark window))))
					      1))
				(and split-mode-p (setf (split-damaged s) nil)))	;split is consistent now
			         ((and window (null (bufmark window))
				     ;;Clear out a window just born or unbuffered.
				     (not (eq window minibufwindow))
				     (eq (eline-conts (startline window)) 'hphcs))
				(redisplay-window window nil 3)
				(and split-mode-p (setf (split-damaged s) nil)))) ;split is consistent now
			   (and (eq window curwindow)(setq foundsw hisline))))))

;;;
;;;
;;;	Compute where physical cursor is to be left.
;;;	Put it there.  Compute the new redisplay state.
;;;

set-new-state
	        (or foundsw (rbarf "Redisplay can't find the cursor"))
	        (cond (split-mode-p		;reactivate correct terminal split
		      (or (eq current-split curwindow-split)
			(rdis-open-split curwindow-split))
		      (rdis-select-split curwindow-split)))
	        (setq target-hpos (real-world-cursor-xcoord
			        (wwtcomp (eline (screen foundsw)))
			        curpointpos))
	        (setq real-foundx foundsw)
	        (do nil ((< target-hpos (1+ screenlinelen)))   ;find cursor through continuation lines
		  (setq target-hpos (- target-hpos screenlinelen))
		  (cond ((or (not (< real-foundx (1- screenheight)))
			   (not (eq (eline (screen real-foundx))
				  (eline (screen (1+ real-foundx))))))
		         (return (setq target-hpos (1- screenlinelen)))))
		  (aos real-foundx))
	        (and (eq (linedata realcurdispline) rwork-string)
		   (not (eq realcurdispline (screen real-foundx)))
		   (rplac-linedata realcurdispline (substr rwork-string 1)))
	        (setq realcurdispline (screen real-foundx))
	        (setq last-prinl (lineln realcurdispline))

;;;
;;;	On no-upmotion jobbies, print something if not done by rdis-window.
;;;

	        (and tty-no-upmotionp		;is noupm
		   (not (= rdis-last-tty-upprint-x real-foundx))
		   (progn (redisplay-line realcurdispline nil real-foundx)
			(setq rdis-last-tty-upprint-x real-foundx)))

;;;   Get the last current-line image in rwork-string so that the next
;;;   optimized redisplay can hack it.

	        (or (eq (linedata realcurdispline) rwork-string)
		  (progn (e_lap_$rplacstring rwork-string
				         (linedata realcurdispline)
				         last-prinl 0 last-prinl)
		         (rplac-linedata realcurdispline rwork-string)))

	        (setq last-fpath fpathname)
	        (setq last-buffer current-buffer)
	        (setq last-bufmode current-buffer-mode)
	        (setq last-modified-flag buffer-modified-flag)
	        (setq last-read-only-flag read-only-flag)
	        (setq last-minor-modes buffer-minor-modes)
	        (setq last-udv user-display-variable)
	        (setq last-foundx foundsw)
	        
	        (setq last-curline curline
		    damaged-flag nil
		    some-split-damaged nil
		    last-curlinel (1- curlinel)
		    touchy-damaged-flag nil
		    rdis-locdisp-linex nil
		    rdis-locdisp-split nil)
	        (and minibufferp (setq rdis-last-echolinex real-foundx))
	        (DCTL-position-cursor target-hpos real-foundx)
	        (e_pl1_$dump_output_buffer)
	        (return nil))))
;;;
;;;
;;; Look for curline occuring more once on screen
;;;

(defun rdis-curline-multiwindowed ()
       ;; determines if curline appears in more than one screen location
       (and
         (memq current-buffer rdis-multiwindowed-buflist)
         (cond ((not split-mode-p)		;simple case optimization
	      (do ((i 0 (1+ i)))		;just scan screen
		((= i screenheight) nil)
		(and (not (= i last-foundx))
		     (eq (eline (screen i)) curline)
		     (return t))))
	     (t				;split-mode is hard case
	       ;; Must examine all splits to ensure damage state accurate.
	       (< 1			;count occurances
		(do ((sn 0 (1+ sn))		;loop over all splits
		     (s)			;split being checked
		     (matches 0))		;total lines eq curline
		    ((= sn nsplits) matches)
		    (setq s (splits sn))
		    (incf matches
			(do ((i 0 (1+ i))	;loop over split lines
			     (m 0)	;# curline's in this split
			     (screen (split-screen s))     ;split image
			     (nlines (split-height s)))    ;split size
			    ;;loop over all lines; may be > 1 window in split
			    ((= i nlines) m)
			    (cond ((eq curline (eline (screen i)))   ;match?
				 (and (= 0 m)
				      (setf (split-damaged s) t))  ;update state
				 (incf m)))))))))))
;;;
;;;
;;;	Moby hair - redisplay one window
;;;	This thing is now so complex that I barely understand it.
;;;

(defun redisplay-window
       (window				;the window to redisplay
         pointeline				;editor line "point" for window
         tries)				;successive algorithms to lay out
					; new screen.
					;1 = find point on old screen
					;2 = center screen about point
					;3 = point on top line

       (prog (start				;starting line # of window
	     nlines			;# of lines in window
	     oldstart			;loc of first old line on new screen
	     oldct			;index into old screen, processed
	     newct			;index into new screen, processed
	     oldx				;look ahead index, old screen
	     newx				;look ahead index, new screen
	     foundsw			;line # on which pointeline found
					;nil if not found
	     oldfat			;when doing idel-lines, matched area
	     newfat			; on screens
	     windowlim			;bottom line # of this window
	     deletedx			;Lines idelled out'ed's index
	     opt1f			;flag for detabbification optimization
	     e-quit-transparency	;SPECIAL VAR!
	     split-selected)		;are we in the window's display split?

	   (setq e-quit-transparency 'redisplaying)
	   (aos rdis-wgen-meter)		;meter it

	   (setq start (startline window)	;determine window boundaries
	         nlines (numlines window))
	   (setq oldct start newct start)	;planned no lines, considered none.
	   (setq windowlim (+ start nlines -1))	;limit of window
	   (setq deletedx (1+ windowlim))	;For idel-lines hackery.
	   (setq split-selected (not split-mode-p))

	   ;; "tries" is passed by caller, usually 1, 3 forces pointeline home.

	   ;; Try super-optimized window redisplay for cursor motion only
	   ;; Don't use to avoid modeline update.

	   (and (not damaged-flag)(not touchy-damaged-flag)(= tries 1)
	        (do x start (1+ x)(> x windowlim)    ;nil => forget it
		  (and (eq (eline (screen x)) pointeline)
		       (not (and (eq (eline (screen windowlim)) pointeline)
			       (not (< (lineln (screen windowlim)) screenlinelen))))
		       (return (setq foundsw x))))
	        (progn (aos rdis-ndf-opt-meter)
		     (return foundsw)))


	   (do ((toldct oldct (1+ toldct)))	;find first non-deleted on screen.
	       ((> toldct windowlim)		;all deleted, full redisplay
	        (setq tries 2))
	       (and (screen toldct)		;if line has stuff,
		  (not (eq (eline-contents (eline (screen toldct))) 'deleted))   ;this is a real line
		  (return (setq oldstart toldct))))	;oldstart = 1st real line thats still there.
;;;
;;;
;;;  Try the different algorithms to fill the newscreen array to represent
;;;  the new screen.
;;;
	   (and tty-no-upmotionp (setq tries 3))     ;We know exactly for prtty.
fillup
	   (setq newx start oldx oldstart)	;oldstart is place we hope
					;to find old line conts.
	   (and (> tries 1)(setq oldx (1+ windowlim)))	;Inhibit eline-conts.

	   (do ((l (cond ((= tries 1)(eline (screen oldx))) ;could merge screens
		       ((= tries 2)(find-nice-starting-line pointeline nlines))  ; disjoint screens
		       ((= tries 3) pointeline)    ;all obscure cases, big lines
		       ((> tries 3)(rbarf "Redisplay-window can't position point.")))
		 (eline-next l))
	        (screenx start (1+ screenx)))
	       ((> screenx windowlim))

	       (and (eq l pointeline)(setq foundsw screenx))   ;we found the line, this screen ok.
	       (cond ((and tty-no-upmotionp (not (eq l pointeline)))
		    (do sx1 screenx (1+ sx1)(> sx1 windowlim)
		        (store (newscreen sx1) nil)
		        (store (eline-conts sx1) nil)
		        (store (screen sx1) nil))  ;blank out rest.
		    (return nil)))
;;;
;;;
;;;	Try to use previously output-converted image of line if
;;;	car of eline is eq to whats in eline-conts.  If not, output
;;;	convert. Re-set eline-conts at refill-new-array.
;;;	opt1f will say whether this won or not.  This is solely a CPU
;;;	time and consing optimization.

;;;	Begin by trying to find current l in old screen.

	       (do ((sx1 oldx (1+ sx1)))	;find old stuff
		 ((or (> sx1 windowlim)(null (screen sx1))))
		 (cond ((eq (eline (screen sx1)) l)
		        (setq oldx sx1)	;found it- desired effect
		        (return t))))	;is what happens to  oldx


	       (cond ((and (not (> oldx windowlim))  ;try for saving work
		         (not (eq l curline))	;don't believe anything
		         (eq (eline (screen oldx)) l)	;really there
		         (eq (eline-contents l) (eline-conts oldx)))    ;makes it.

		    (aos rdis-detab-opt-meter)

		    (do ((nx screenx (1+ nx))(ox oldx (1+ ox)))
		        ((not (eq l (eline (screen ox))))
		         (setq screenx (1- nx) oldx ox)
		         (setq opt1f t))

		        (store (newscreen nx)(screen ox))
		        (cond ((= ox windowlim)
			     (setq opt1f (or (not (screen ox))(< (lineln (screen ox)) screenlinelen)))
			     (and opt1f (setq screenx nx oldx ox))
			     (return t))
			    ((= nx windowlim)    ;ox cannot be on bottom now
			     (setq opt1f (not (eq l (eline (screen (1+ ox))))))
			     (and opt1f (setq screenx nx oldx ox))
			     (return t)))))
		   (t (setq opt1f nil)))
;;;
	       (cond ((not opt1f)		;could'nt find old detabbification
					;Output-convert it NOW.

		    (store (newscreen screenx)(parameterize-line l))
;;;
;;;
;;;	Hack in the continuation lines
;;;
		    (do ((ns (newscreen screenx)(newscreen screenx)))
		        ((or (not ns)(not (> (lineln ns) screenlinelen))))

		        (store (newscreen screenx) ;old line
			     (cons l (cons (substr (linedata ns) 1 screenlinelen)
				         screenlinelen)))
		        (cond ((not (< screenx windowlim))
			     (and (< tries 3)
				(eq l pointeline)
				(setq foundsw nil)) ; cause recomputation
			     (return nil)))

		        (store (newscreen (1+ screenx))
			     (cons l (cons (substr (linedata ns)(1+ screenlinelen))
				         (- (lineln ns) screenlinelen))))
		        (aos screenx)))))


	   (let ((lastrdisline (newscreen (+ start nlines -1))))
	        (cond (lastrdisline 		;ut SCPOS: MEANINGLESS ARGS	non habet.
		      (rplac-lineln lastrdisline
				(min screenlinelen (lineln lastrdisline))))))
;;;
;;;	See if last pass got the current line, iterate if not.
;;;	Special-case prtty stuff, set up for merge scan.

	   (cond (foundsw)			;found curline, use it as is.
	         (t (setq tries (1+ tries))	;try some other technique
		  (go fillup)))


	   (cond ((= tries 1)(aos rdis-wgen-c1-meter))	;meter window type
	         ((= tries 2)(aos rdis-wgen-c2-meter))
	         ((= tries 3)(aos rdis-wgen-c3-meter)))

	   (setq oldct start)		;init for screen scan
	   (setq oldx oldstart)		;first found line

	   (cond (tty-no-upmotionp		;check for different lines
		 (or (eq (eline (screen start))(eline (newscreen start)))
		     (progn (DCTL-nextline)
			  (DCTL-assert-scpos nil 0)	;feed up.
			  (do sx2 start (1+ sx2)(> sx2 windowlim)(store (screen sx2) nil))))
		 (go real-rdis-rest-of-window))
	         ((> tries 1)(go l0)))	;couldn't find, try random match.

;;;
;;;
;;;	Found line. Gotta insert or delete.
;;;

found-matched-line


	   ;; last-line no-share hack- bsg 10/27/79
	   (cond ((and (or (= oldx windowlim)(null (screen (1+ oldx))))
		     (= newx start)
		     (not (eq (newscreen newx)(screen oldx))))  ;detabbified differently
		(go redisplay-the-rest-of-window)))

	   (setq oldfat (- oldx oldct)	;#of lines on screen before matchee
	         newfat (- newx newct))	;# of lines that want to be displayed before
					;matchee.
	   (or (and (= 0 oldfat)(= 0 newfat))	;if no idelry, match them.
	       (and (or idel-lines-availablep region-scroll-availablep)
		  (< (1+ (abs (- oldfat newfat))) nlines))   ;dont idel for small change
	       (go redisplay-the-rest-of-window))

	   (or split-selected		;if not already in correct split, go there
	       (and (setq split-selected t)
		  (rdis-select-window-split window)))

	   (do ((ct (min oldfat newfat)(1- ct)))     ;zap-redisplay common lines
	       ((= ct 0))

	       (redisplay-line (newscreen newct)(screen oldct) newct)    ;do it
	       (aos oldct)			;ground on both screens
	       (aos newct))

	   (cond ((> newfat oldfat)		;better open up
		(let ((fatdif (- newfat oldfat)))
		     (cond ((not region-scroll-availablep)
			  (DCTL-position-cursor 0 (- (1+ windowlim) fatdif))
 
			  (DCTL-delete-lines fatdif)))
		     (do xx fatdif (1- xx)(= xx 0)
		         (setq deletedx (1- deletedx))    ;Pull up line
		         (or (> deletedx windowlim)(store (screen deletedx) nil)))
					;fixes pulling up pushed off lines.
		     (DCTL-position-cursor 0 newct)
		     (cond (region-scroll-availablep
			   (DCTL-scroll-down-region fatdif windowlim))

			 (t (DCTL-insert-lines fatdif)))
		     (do i 1 (1+ i)(> i fatdif)
		         (DCTL-position-cursor 0 newct)
		         (redisplay-line (newscreen newct) nil newct)
		         (aos newct)))))

			;next line

	   (cond ((> oldfat newfat)		;extra space on screen
		(let ((fatdif (- oldfat newfat)))
		     (DCTL-position-cursor 0 newct)
		     (cond (region-scroll-availablep
			   (DCTL-scroll-up-region fatdif windowlim))
			 (t (DCTL-delete-lines fatdif)
			    (DCTL-position-cursor 0 (- (1+ windowlim) fatdif))
			    (DCTL-insert-lines fatdif)))
		     (setq deletedx (+ deletedx fatdif)) ;offset possible wipeage index
		     (setq oldct (+ oldct fatdif)))))


	   (redisplay-line (newscreen newct)(screen oldct) newct)	;whatever technology necessary

	   (aos oldct)
	   (aos newct)

	   (and (> oldct windowlim)(go redisplay-the-rest-of-window))    ;no news in old screen, useless
	   (and (> newct windowlim)(go refill-new-array)) ;screen done
;;;
;;;
;;;	Great mergo loop. Match lines in old screen and new.
;;;

l0 	   (setq newx newct)		;Scan BUFFER..
l1 	   (and (> newx windowlim)(go redisplay-the-rest-of-window))     ;will worry about fall-thru, nils.
	   (or (newscreen newx)(go redisplay-the-rest-of-window))	;nils- could never find 'em anyhow
	   (setq oldx oldct)		;now search screen for this line

l2 	   (cond ((> oldx windowlim)		;couldnt match this line
		(aos newx)		;try the next
		(go l1))
	         ((eq (eline (newscreen newx))(eline (screen oldx)))	;found it
		(go found-matched-line))
	         (t (aos oldx)		;search on..
		  (go l2)))


redisplay-the-rest-of-window

	   (or split-selected		;if not already in correct split, go there
	       (and (setq split-selected t)
		  (rdis-select-window-split window)))

	   (and (or rdis-csopt rdis-wosclr-opt)	;try screen-clear hacks
	        (not minibufferp)
	        (< newct windowlim)		;allow 1
	        (newscreen newct)		;dont do it if happy eob window
	        (< oldct windowlim)
	        (screen oldct)
	        (or (not two-window-mode)
		  (not rdis-csopt)
		  ;;(= start (startline (windows 2))))
		  (= start (startline (windows nuwindows))))
;;; Option to clear screen to end before filling rest of screen
	        (cond (rdis-csopt (DCTL-position-cursor 0 newct)
			      (DCTL-clear-rest-of-screen)
			      (setq damaged-flag t)	;if demand redisplery, mode line zonked.
			      (do i oldct (1+ i)(= i screenheight)
				(store (screen i) nil)))
;;; Option to kill lines in rest of window prior to redisplaying it.
		    (t			;wos's hack
		      (do ((newx newct (1+ newx))
			 (oldx oldct (1+ oldx)))
			((> newx windowlim))
			(cond ((and (screen oldx)(> (lineln (screen oldx)) 0))
			       (DCTL-position-cursor 0 newx)
			       (DCTL-kill-line)
			       (store (screen oldx) nil)))))))
;;;

;;;
;;;	Set up images for next time; screen with the line structure,
;;;	eline-conts with the line strings.  Compare-update whatever is
;;;	unmerged on screen.
;;;

real-rdis-rest-of-window

	   (do ((newx newct (1+ newx))
	        (old)(new)
	        (oldx oldct (1+ oldx)))
	       ((> newx windowlim))

	       (setq old (and (not (> oldx windowlim))(screen oldx))
		   new (newscreen newx))
	       (cond ((and (null new) minibufferp)   ;dont clear minibuffer tails 10/5/79
		    (setq windowlim (1- newx));cause refill to stop
		    (return nil)))
	       (cond (tty-no-upmotionp	;Worry about prtty update
		     (cond (new		;only if new stuff
			   (cond ((or (not old)  ;If old was not there, or
				    (not (= (lineln new)(lineln old))) ;if they're different,
				    (not (e_lap_$compare_strings
					 (linedata old) 0 (linedata new) 0 (lineln old))))
				(redisplay-line new (cond ((= newx rdis-last-tty-upprint-x) old)(t nil)) newx)
				(setq rdis-last-tty-upprint-x newx))))))
		   (t (redisplay-line new old newx))))	;do it


;;;
;;;	Copy current new window into "state" of window.
;;;

refill-new-array

	   (do c start (1+ c)(> c windowlim)
	       (let ((rdl (newscreen c)))
		  (store (screen c) rdl)
		  (store (eline-conts c)(cond ((eq (eline rdl) curline) 'ahem)
					(t (eline-contents
					     (eline rdl)))))))

	   (return foundsw)))


;;;

;;;
;;;	Choose the first line of the new window
;;;	Try to go a half window back from current line, measuring lines.
;;;

(defun find-nice-starting-line (centerline nlines)
       (do ((l centerline (eline-previous l))
	  (lrl centerline)			;last known good
	  (i (// nlines 2)))
	 ((not (> i 0))(or l lrl))
	 (cond (l (setq lrl l)		;remember last real line
		(setq i (- i (rdis-/#-of-lines-in- l))))
	       (t (setq i (1- i))))))


;;;
;;;	Generate a displayline given an editorline.
;;;

(defun parameterize-line (editorline)
       (and editorline
	  (cons editorline			;car is editor's thought
	        (let ((detabif
		      (detabbify (cond ((eq editorline curline) curstuff)
				   (t (car editorline))))))
		   (cons detabif (stringlength detabif))))))
       
;;; 
;;;
;;;	El Peludo -- The hairy one
;;;

(defun redisplay-line (newl oldl sx)		;try to replace this by that.
       (prog (oldlrep newlrep oldll newll leftcommon rightcommon oldarea newarea have-slain ischct)
	   (setq oldlrep (cond (oldl (setq oldll (lineln oldl))(linedata oldl))
			   (t (setq oldll 0) "")))
	   (setq newlrep (cond (newl (setq newll (lineln newl))(linedata newl))
			   (t (setq newll 0) rdis-a-lotta-blanks)))
	   (and (= oldll 0)
	        (not tty-eolch-lossp)
	        (setq oldll newll oldlrep rdis-a-lotta-blanks))	;heh, heh.

	   (and (or (eq oldlrep newlrep)
		  (and (= newll oldll)
		       (e_lap_$compare_strings oldlrep 0 newlrep 0 oldll)))
	        (return nil))

	   (setq leftcommon 0 rightcommon 0)
	   (do i 1 (1+ i)(> i (min newll oldll))
	       (cond ((= (getcharn oldlrep i)(getcharn newlrep i))
		    (setq leftcommon (1+ leftcommon)))
		   (t (return nil))))
	   (do ((j oldll (1- j))
	        (i newll (1- i)))
	       ((or (= leftcommon i)(= leftcommon j)))

	       (cond ((= (getcharn oldlrep j)(getcharn newlrep i))
		    (setq rightcommon (1+ rightcommon)))
		   (t (return nil))))


	   (setq newarea (- newll rightcommon leftcommon)
	         oldarea (- oldll rightcommon leftcommon))

	   (and (< rightcommon 3)(go kill-eol))

;;;
;;;	Assess whether or not to try idel-chars.  Overwriting an equal-length
;;;	area is considered a special case.
;;;

	   (and (> rightcommon 0)(< rightcommon 4)(= newarea oldarea)
	        (> newarea 15.)(go kill-eol))	;3/1/79 avoid long repaint
					;cause haarenk can do better

	   (cond ((and (= oldarea newarea)	;Must be equal length.
		     (or (not overstrike-availablep)	;tty can overpaint
		         (e_lap_$compare_strings oldlrep leftcommon rdis-a-lotta-blanks 0 oldarea))) ;clean stuff
		(DCTL-position-cursor leftcommon sx)	;do it.
		(rdis-substr-display newlrep leftcommon newarea)
		(return nil)))		;All done.

	   (or idel-chars-availablep (go kill-eol))  ;Can't do it.
	   (and (> (+ (+ 4 newarea)(* 2 oldarea)) (- newll leftcommon))
	        (go kill-eol))		;not worth it.

	   (DCTL-position-cursor leftcommon sx)

	   (cond (overstrike-availablep	;All cases, can't overpaint.
		 (or (= 0 oldarea)(DCTL-delete-chars oldarea))
		 (or (= 0 newarea)(DCTL-insert-char-string (substr newlrep (1+ leftcommon) newarea))))
	         ((> oldarea newarea)
		(and (> newarea 0)(rdis-substr-display newlrep leftcommon newarea))
		(DCTL-delete-chars (- oldarea newarea)))
	         (t (or (= oldarea 0)(rdis-substr-display newlrep leftcommon oldarea))
		  (DCTL-insert-char-string (substr newlrep (+ 1 leftcommon oldarea)(- newarea oldarea)))))
	   (return nil)

kill-eol 	   (and (> newll leftcommon)		;if lotta blanks, not there.
	        (not tty-eolch-lossp)
	        (e_lap_$compare_strings newlrep leftcommon rdis-a-lotta-blanks 0 (- newll leftcommon))
	        (setq newll leftcommon))

	   (and idel-chars-availablep		;3/1/79 try idel for \c push
	        (> newll leftcommon)		;gotta be real stuff
	        (= newll oldll)		;of equal length both ends
	        (setq ischct (e_lap_$rdis-crossmatch oldlrep newlrep leftcommon oldll))
	        (> (- newll (abs ischct)) 10.)
	        (cond ((< ischct 0)
		     ;;Guy deleted characters, theres a new one at the end
		     (DCTL-position-cursor leftcommon sx)
		     (DCTL-delete-chars (setq ischct (- ischct)))
		     (DCTL-position-cursor (- newll ischct) sx)
		     (rdis-substr-display newlrep (- newll ischct) ischct)
		     (return nil))
		    ((> ischct 0) ;dont know what 0 means cant happen
		     ;;Guy inserted, the end is chopped.
		     (DCTL-position-cursor (- newll ischct) sx)
		     (DCTL-kill-line)
		     (DCTL-position-cursor leftcommon sx)
		     (DCTL-insert-char-string (substr newlrep (1+ leftcommon) ischct))
		     (return nil))))

	   (cond ((not overstrike-availablep))
	         ((= leftcommon oldll))
	         ((and (> newll leftcommon)
		     (or (not tty-no-upmotionp)    ;fixes end;end; bug.
		         (not (> oldll newll))
		         (e_lap_$compare_strings oldlrep newll rdis-a-lotta-blanks 0 (- oldll newll)))
		     (e_lap_$compare_strings oldlrep leftcommon rdis-a-lotta-blanks 0 (- newll leftcommon))))
	         ((and (= newll leftcommon)	;don't kill blanks
		     (> oldll newll)
		     (e_lap_$compare_strings oldlrep newll rdis-a-lotta-blanks 0 (- oldll newll))))
	         (t (setq have-slain t)	;flag killing
		  (DCTL-position-cursor leftcommon sx)
		  (DCTL-kill-line)))

	   (and (eq oldlrep rdis-a-lotta-blanks)(setq have-slain t))

	   (cond ((> newll leftcommon)	;Add new extension
		(or (> oldll leftcommon)(setq have-slain t)) ;nothing out there
		(cond (rdis-whitespace-optimize    ;New hair here 2/13/80
		        (cond ((and tty-no-cleolp (> oldll leftcommon)(> newll oldll))
			     (DCTL-position-cursor leftcommon sx)
			     (rdis-substr-display newlrep leftcommon (- oldll leftcommon))
			     (setq leftcommon oldll have-slain t)))
		        (cond ((rdis-whitespace-optimizer newlrep leftcommon newll have-slain sx)
			     (return nil)))))
		;;Print out the extension standardly by default.
		(DCTL-position-cursor leftcommon sx)
		(rdis-substr-display newlrep leftcommon (- newll leftcommon))))
	   ;; Now kill whats left off end.

	   (and (or have-slain (not (> oldll newll)))(return nil))	;no need

	   (cond ((not (e_lap_$compare_strings oldlrep newll rdis-a-lotta-blanks 0 (- oldll newll)))
		(DCTL-position-cursor newll sx)
		(DCTL-kill-line)))

	   (return nil)
	   ))

(defun rdis-substr-display (stuff start howmany)
       (e_lap_$rplacstring-offset detab-buf stuff howmany 0 howmany start)
       (DCTL-display-char-string detab-buf))


;;; 
;;;
;;;	Haarenkoenig: Do CRTSTY-like whitespace condensation at low speeds.
;;;

(defun rdis-whitespace-optimizer (lrep start end+ have-slain y)
       (prog (next-to-go for-blanks for-non-blanks optinc)
	   (and tty-no-cleolp (not have-slain)(return nil))
	   (and tty-eolch-lossp (return nil))
	   (setq optinc (// (- end+ start) 6))
	   (or (> optinc 1)(return nil))
	   ;;(do x start (+ x optinc)(> (+ x optinc) end+)
	   ;;(and (e_lap_$compare_strings lrep x rdis-a-lotta-blanks 0 optinc)
	   ;;(return (setq worthit t))))
	   ;;(or worthit (return nil))
	   
	   ;;
	   ;; We're gonna do it. Kill to eol first.
	   ;;
	   (or have-slain (progn (DCTL-position-cursor start y)
			     (DCTL-kill-line)))
	   (or rdis-blankscan-table (setq rdis-blankscan-table (charscan-table " ")))
	   (setq next-to-go start for-non-blanks 0)
	   (do ()((not (< next-to-go end+)))
	       (setq for-blanks (e_lap_$tct next-to-go (car rdis-blankscan-table) lrep))
	       (and (> (+ for-blanks next-to-go) end+)(setq for-blanks (- end+ next-to-go)))
	       (cond ((> for-blanks 0)	;real stuff to print
		    (cond ((< for-non-blanks 4)(DCTL-position-cursor (- next-to-go for-non-blanks) y)
					 (rdis-substr-display rdis-a-lotta-blanks 0 for-non-blanks))
			(t (DCTL-position-cursor next-to-go y)))
		    (rdis-substr-display lrep next-to-go for-blanks)
		    (setq next-to-go (+ for-blanks next-to-go))))
	       (or (< next-to-go end+)(return t))    ;ended nonblank
	       (setq for-non-blanks (e_lap_$tct next-to-go (cdr rdis-blankscan-table) lrep))
	       (setq next-to-go (+ next-to-go for-non-blanks)))
	   (return t)))

;;;
;;;	Eol-kill writearound for gran dumb tty's, BSG 2/12/80
;;;	Switched into DCTL-kill-line by display-init
;;;

(defun rdis-kill-eol-writearound ()
       (let ((l (or (lineln (screen Y)) 0)))
	  (and (> l X)(rdis-substr-display rdis-a-lotta-blanks X (- l X)))))

;;;
;;;	Minibuffer printing functions
;;;

(defun rdis-choose-echo-linex ()
       (rdis-enter-split minibuffer-split)
       (setq rdis-last-echolinex
	   (let ((mbottom (+ (numlines minibufwindow)
			 (startline minibufwindow) -1))
	         (mtop (startline minibufwindow))
	         (favor 99.)
	         (thisfavor 0)(favorite 0))
	        (do lx mtop (1+ lx)(> lx mbottom)
		  (setq thisfavor
		        (cond ((= lx rdis-mbuf-transient-linex) 10.)
			    ((or (null (screen lx))
			         (e_lap_$compare_strings
				 (linedata (screen lx)) 0
				 rdis-a-lotta-blanks 0
				 (lineln (screen lx))))
			     (cond ((= lx mtop) 30.)(t 20.)))
			    ((> lx rdis-last-echolinex) 40.)
			    ((and (< lx rdis-last-echolinex)
				(not (= lx mtop)))
			     50.)
			    ((= lx mtop) 60.)
			    ((= lx rdis-last-echolinex) 70.)
			    (t 80.)))
		  (cond ((< thisfavor favor)
		         (setq favor thisfavor favorite lx))))
	        (setq rdis-mbuf-transient-linex -1)
	        favorite)))

(defun echo-buffer-print (strg)
       (rdis-choose-echo-linex)
       (echo-buffer-overwrite strg rdis-last-echolinex))

(defun echo-buffer-utter (strg)
       (echo-buffer-print strg)
       (setq rdis-mbuf-transient-linex rdis-last-echolinex))

(defun echo-buffer-rewrite (strg)
       (echo-buffer-overwrite strg rdis-last-echolinex))

(defun echo-buffer-overwrite (strg linex)
       (rdis-enter-split minibuffer-split)
       (cond (tty-no-upmotionp (DCTL-nextline)
			 (DCTL-assert-scpos nil linex)
			 (DCTL-position-cursor 5 linex))
	   (t (redisplay-line nil (screen linex) linex)))
       (store (screen linex) nil)
       (store (newscreen linex) nil)
       (echo-buffer-print- (catenate "     " strg) linex))

(defun echo-buffer-print- (strg linex)
       (rdis-enter-split minibuffer-split)
       (let ((rdl (hokeup-line strg)))
	  (redisplay-line rdl (screen linex) linex)
	  (store (screen linex) rdl)
	  (store (newscreen linex) rdl)
	  (setq rdis-last-tty-upprint-x linex)
	  (DCTL-position-cursor (lineln rdl) linex)
	  (e_pl1_$dump_output_buffer)))

(defun hokeup-line (strg)
       (setq strg (parameterize-line
		(cons (catenate strg NLCHARSTRING) (ncons nil))))
       (rplac-lineln strg (min screenlinelen (lineln strg)))
       strg)

(defun echo-buffer-outprint (strg)
       (rdis-enter-split minibuffer-split)
       (let ((curline-there (screen rdis-last-echolinex)))
	  (cond ((null curline-there)(echo-buffer-print strg))
	        (t (echo-buffer-print-
		   (catenate (linedata curline-there) strg)
		   rdis-last-echolinex)))))

(defun echo-buffer-clear ()
       (echo-buffer-overwrite "" rdis-last-echolinex))

(defun echo-buffer-clear-all ()
       (rdis-enter-split minibuffer-split)
       (setq rdis-mbuf-transient-linex (startline minibufwindow))
       (do ((i rdis-mbuf-transient-linex (1+ i))
	  (end (+ (numlines minibufwindow) rdis-mbuf-transient-linex)))
	 ((= i end))
	 (echo-buffer-overwrite "" i)))

(defun echo-buffer-rubout (n)
       (rdis-enter-split minibuffer-split)
       (let ((rdl (screen rdis-last-echolinex)))
	  (let ((len (lineln rdl)))
	       (cond ((< len n)
		    (echo-buffer-clear))
		   (t (echo-buffer-print-
		        (substr (linedata rdl) 1 (- len n))
		        rdis-last-echolinex))))))

(defun ring-tty-bell ()
       (cond ((get tty-type 'tintinnabulum-ipsum-meum-sono) ;I ring my own bell
	    (DCTL-ring-tty-bell))
	   (t (Rtyo 007)))
       (e_pl1_$dump_output_buffer))

;;; 
;;;
;;;	Mode line maintenance
;;;

;;; This is for e_redisplay_.lisp

(defun update-mode-line ()
       (setq current-minor-mode-display-string
	   (cond ((null buffer-minor-modes) "")
	         (t (catenate
		    " <"
		    (apply 'catenate
			 (maplist
			   '(lambda (c)	;c = cons
				  (cond ((cdr c)
				         (catenate (car c) ", "))
				        (t (car c))))
			   buffer-minor-modes))
		    ">"))))
       (let ((modeline-contents "")
	   (pathline-contents ""))
	  (cond (mode-line-hook
		(let ((hook-result (funcall mode-line-hook)))
		     (setq modeline-contents (or (car hook-result) "")
			 pathline-contents (or (cadr hook-result) ""))))
	        (t (setq modeline-contents
		       (catenate
		         mode-line-herald
		         " ("
		         current-buffer-mode
		         current-minor-mode-display-string
		         ")"
		         (cond (read-only-flag " (RO)")(t ""))
		         " - "
		         current-buffer
		         (cond (user-display-variable
			       (catenate " " user-display-variable))
			     (t ""))))
		 (cond ((or buffer-modified-flag fpathname)
		        (setq pathline-contents
			    (catenate
			      (cond (buffer-modified-flag " *")
				  (t "  "))
			      (cond (fpathname (catenate " " fpathname))
				  (t ""))))))))
	  (setf (eline-contents phony-modeline-edline)
	        (catenate
		(cond ((> (stringlength modeline-contents) full-screenlinelen)
		       (substr modeline-contents 1 full-screenlinelen))
		      (t modeline-contents))
		NLCHARSTRING))
	  (setf (eline-contents phony-pathline-edline)
	        (catenate pathline-contents NLCHARSTRING)))
       (and split-mode-p
	  (setf (split-damaged model-split) t)))
;;; 
;;;
;;;	These things are called by the Fnpmeisters
;;;	    Output Conversion
;;;

(defun rdis-/#-of-\cs-in- (x)
       (let ((shrunkscreenl (- screenlinelen 2))
	   (shrunklinel (max 0 (- x 2))))
	  (1- (// (+ shrunkscreenl -1 shrunklinel) shrunkscreenl))))

(defun rdis-/#-of-\cs-to-make (x)
       (1- (// (+ screenlinelen -1 x)
	     screenlinelen)))

(defun detabbify (strg)			;MEOW!!!!!
       (aos rdis-detabb-meter)
       (e_lap_$rplacstring detab-buf "" 0 0 0)
       (do ((origstrl (1- (gstrgl strg)))
	  (strx 0)(ocol 0)(tabx))
	 ((not (< strx origstrl)) nil)

	 (setq tabx (rdis-tabscan strg origstrl strx))
	 (e_lap_$rplacstring-offset detab-buf strg tabx ocol (+ ocol tabx) strx)
	 (cond ((= (+ tabx strx) origstrl)
	        (return nil)))
	 (setq strx (+ strx tabx) ocol (+ ocol tabx))
	 (let ((ch (e_lap_$ggcharn strg strx))) ;funny char, huh!?
	      (cond ((= ch 11)		;tab
		   (let ((tl (- tab-equivalent (\ ocol tab-equivalent))))
		        (e_lap_$rplacstring detab-buf rdis-a-lotta-blanks tl ocol (+ ocol tl))
		        (setq ocol (+ tl ocol))))
		  ((and (= ch 10)		;underlining
		        (rdis-at-underline strg strx))
		   (e_lap_$rplacstring
		     detab-buf
		     (ascii (boole 7	;logior
			         400	;high-order bit
			         (rdis-underlined-char strg strx)))
		     1 (1- ocol) ocol)
		   (aos strx))
		  ((and (= ch 10)	;backspace
		        suppress-backspace-display))
		  ((and (= ch 177)	;rubout
		        suppress-rubout-display))
		  ((and (< ch 40)	;control character (including backspace)
		        suppress-ctlchar-display))
		  ((and (< ch 40) display-ctlchar-with-^)
		   (e_lap_$rplacstring detab-buf (catenate "^" (ascii (+ ch 100))) 2 ocol (+ ocol 2))
		   (setq ocol (+ 2 ocol)))
		  (t (setq ocol (+ ocol 4))
		     (e_lap_$rplacstring detab-buf (rdis-octescape ch) 4 (- ocol 4) ocol))))
	 (aos strx))			;end do- answer in detab-buf
       (cond ((not (> (stringlength detab-buf) screenlinelen))
	    (substr detab-buf 1))
	   (t (do ((n\c (rdis-/#-of-\cs-in- (stringlength detab-buf)) (1- n\c))
		 (insertx screenlinelen (+ insertx screenlinelen)))
		((= n\c 0)(substr detab-buf 1))

		(e_lap_$insert-chars detab-buf insertx "\c" 2)))))


(defun rdis-at-underline (strg strx)
       (and DCTL-underline-mask
	  (rdis-underlined-char strg strx)))


(defun rdis-underlined-char (strg strx)
       (prog (pc nc)
	   (cond
	     ((< strx 1) nil)
	     ((not (< strx (1- (gstrgl strg)))) nil)
	     (t (setq pc (e_lap_$ggcharn strg (1- strx)))
	        (setq nc (e_lap_$ggcharn strg (1+ strx)))
	        (cond ((< pc 40) nil)
		    ((< nc 40) nil)
		    ((= pc 137) (return nc))
		    ((= nc 137) (return pc))
		    (t nil))))))


(defun real-world-cursor-xcoord (strg charx)
       (prog (eolp answer)
	   (do  ((origstrl (1- (gstrgl strg)))
	         (strx 0) (ocol 0) (tabx) (lies 0))
	        ((not (< strx charx))
	         (setq eolp (= charx origstrl) answer (+ charx lies)))

	        (setq tabx (rdis-tabscan strg origstrl strx))
	        (setq strx (+ strx tabx) ocol (+ ocol tabx))
	        (cond ((not (< strx charx))
		     (return (setq eolp (= charx origstrl) answer (+ charx lies)))))
	        (let ((ch (e_lap_$ggcharn strg strx)))
		   (cond ((= ch 11)		;TAB
			(setq tabx (- tab-equivalent (\ ocol tab-equivalent))))
		         ((and (= ch 10)	;underline
			     (rdis-at-underline strg strx))
			(setq tabx -1))
		         ((and (= ch 10) suppress-backspace-display) (setq tabx 0))	;BS
		         ((and (= ch 177) suppress-rubout-display) (setq tabx 0))	;DEL to be suppressed
		         ((and (< ch 40) suppress-ctlchar-display) (setq tabx 0))	;ctl-char to be suppressed
		         ((and (< ch 40) display-ctlchar-with-^) (setq tabx 2))  ;^<char>
		         (t (setq tabx 4))))	;\nnn
	        (setq lies (+ -1 lies tabx) ocol (+ ocol tabx))	; -1 for orig tab char
	        (aos strx))			;end of do- answer in strg
	   (cond ((< answer screenlinelen))	;boundary case different here, 2l
	         ;; cant be less than 2 or 3 or so.
	         (t (setq answer (+ answer (* 2 (rdis-/#-of-\cs-in- answer))))
		  (and (not eolp)
		       (= 0 (\ answer screenlinelen))
		       (setq answer (+ 2 answer)))))
	   (return answer)))


(defun inverse-real-world-cursor-xcoord (strg schpos)
       (setq schpos (- schpos
		   (cond ((not (> schpos screenlinelen)) 0)
		         (t (* 2 (rdis-/#-of-\cs-to-make schpos))))))
       (do  ((origstrl (1- (gstrgl strg)))
	   (strx 0) (ocol 0) (tabx))
	  ((not (< ocol schpos))
	   strx)

	  (setq tabx (rdis-tabscan strg origstrl strx))
	  (cond ((not (< (+ ocol tabx) schpos))
	         (return (+ strx (- schpos ocol)))))
	  (setq strx (+ strx tabx) ocol (+ ocol tabx))
	  (let ((ch (e_lap_$ggcharn strg strx)))
	       (cond ((= ch 11)
		    (setq tabx (- tab-equivalent (\ ocol tab-equivalent))))
		   ((and (= ch 10)		;underline
		         (rdis-at-underline strg strx))
		    (setq tabx -1))
		   ((and (= ch 10) suppress-backspace-display))
		   ((and (= ch 177) suppress-rubout-display))
		   ((and (< ch 40) suppress-ctlchar-display))
		   ((and (< ch 40) display-ctlchar-with-^) (setq tabx 2))
		   (t (setq tabx 4))))
	  (setq ocol (+ ocol tabx))
	  (aos strx)))


(defun rdis-octescape (n)
       (implode (nreverse (list (prog1 (+ 60 (\ n 8.))(setq n (// n 8.)))
			  (prog1 (+ 60 (\ n 8.))(setq n (// n 8.)))
			  (+ 60 n)
			  '/\))))


(defun wwtcomp (el)				;with-what-to-compare, my dearie.
       (cond ((eq el curline) curstuff)
	   (t (car el))))


(defun gstrgl (x)				;general string length
       (cond ((stringp x)(stringlength x))
	   (t (filecons-length x))))

;;; 
;;;
;;;	Demand Redisplery.
;;;	rehacked 8/24/79 for ^v/esc-v with arguments and barfing.

(defun redisplay-window-from-eline (eline window)
       (go-to-line-point eline 0)		;convince the editor
       (rdis-upd-virtual-window-point window)
       (redisplay-window window eline 3))


(defun rdis-ensure-reasonable-window (window)
       (let ((first (startline window))
	   (stop (+ (startline window) (1- (numlines window))))
	   (ptel (car (bufmark window))))
	  (and (or (eq (eline-contents (eline (screen first))) 'deleted)
		 (do ((x first (1+ x)))
		     ((= x stop) t)
		     (and (eq (eline (screen x)) ptel) (return nil))))
	       (redisplay-window window ptel 1))))


(defun rdis-/#-of-lines-in- (eline)		;how many lines to display
       (max 1
	  (// (+  screenlinelen -1
		(real-world-cursor-xcoord (setq eline (wwtcomp eline))
				      (1- (gstrgl eline))))
	      screenlinelen)))


(defun prev-screen ()
       (rdis-forward-backward-screen (or numarg 1) 'backward))


(defun next-screen ()
       (rdis-forward-backward-screen (or numarg 1) 'forward))

(defun rdis-forward-backward-screen (howmany whichway)
       (and minibufferp (command-quit))
       (let ((window rdis-selected-wlist)
	   (step (- (numlines rdis-selected-wlist) screen-overlap)))
	  (and (< howmany 0)		;go the other way
	       (setq howmany (- howmany)
		   whichway (cond ((eq whichway 'forward) 'backward)
			        (t 'forward))))
	  (rdis-ensure-reasonable-window window)
	  (let ((catchr
		(catch
		  (progn
		    (redisplay-window-from-eline
		      (do ((el (eline (screen (startline window))))
			 (ct howmany (1- ct)))
			((= ct 0) el)
			(setq
			  el (cond
			       ((eq whichway 'forward)
			        (rdis-march-forward-screen-lines
				el step 'current))
			       (t (rdis-march-back-screen-lines
				  el step 'next))))
			(cond ((null el) (throw 'lose rdis-march))))
		      window)
		    'its-ok)
		  rdis-march)))
	       (cond ((eq catchr 'lose) (command-quit))
		   (t (rdis-find-last-foundx 
		        (startline window) (numlines window) curline))))))

;;; Place current line at given position in window (if possible)
(defun redisplay-current-window-relative (position)
       (and minibufferp (command-quit))
       (and (> position 0)(setq position (1- position)))	; 0/1 origin
       (setq rdis-have-redisplayed t)
       (cond ((< position 0)			;from bottom of window
	    (setq position (max 0 (+ (numlines rdis-selected-wlist) position))))
	   (t				;from top of window
	     (setq position (min position (1- (numlines rdis-selected-wlist))))))
       (let ((n (rdis-/#-of-lines-in- curline)))
	  (let ((actual-position		;where it should go counting long lines
		(cond ((> (+ position n) (numlines rdis-selected-wlist))
		       (cond ((> n (numlines rdis-selected-wlist)) position)
			   (t		;can push it up a little
			     (- (numlines rdis-selected-wlist) n))))
		      (t position))))	;fits nicely
	       (redisplay-window rdis-selected-wlist
			     (or 
			       (rdis-march-back-screen-lines curline actual-position 'next)
			       curline)
			     3)
	       (rdis-find-last-foundx (startline rdis-selected-wlist) (numlines rdis-selected-wlist) curline))))


;;; Command to scroll the current window up/down N lines
(defun scroll-current-window ()
       (and minibufferp (command-quit))
       (setq numarg (or numarg 1))		;default to down one line
       (and (= numarg 0) (command-quit))
       (rdis-ensure-reasonable-window rdis-selected-wlist)
       (let ((start (startline rdis-selected-wlist))
	   (numlines (numlines rdis-selected-wlist))
	   (newline curline))
	  (rdis-find-last-foundx start numlines curline)
	  (cond ((< numarg 0)		;scroll down
	         (redisplay-window rdis-selected-wlist
			       (or	;try to find new line
			         (rdis-march-back-screen-lines (eline (screen start))
						         (- numarg) 'current)
			         (command-quit)) ;couldn't
			       3)
	         (cond ((> (- last-foundx numarg)(+ start numlines -1))
		      (setq newline (eline (screen (+ start numlines -1))))
		      (let ((n (rdis-/#-of-lines-in- newline)))
			 (cond ((= n 1))	;no prob
			       ((< (prog2 (rdis-find-last-foundx start numlines newline)
				        last-foundx)
				 (+ start numlines (- n) 1)))
			       ((> n numlines)(setq newline (eline (screen start))))
			       (t (setq newline (eline-previous newline))
				(or newline (command-quit))))))))  ;??
	        (t			;scroll up
		(redisplay-window rdis-selected-wlist
			        (or	;try to find new line
				(rdis-march-forward-screen-lines (eline (screen start))
							   numarg 'next)
				(command-quit))	;couldn't
			        3)
		(cond ((< (- last-foundx numarg) start)
		       (setq newline (eline (screen start)))))))
	  (rdis-find-last-foundx start numlines newline)
	  (or (eq curline newline)	;no changes
	      (progn (go-to-line-point newline 0)
		   (rdis-upd-virtual-window-point rdis-selected-wlist))))))


(defun move-to-screen-edge ()
       (and minibufferp (command-quit))
       (rdis-ensure-reasonable-window rdis-selected-wlist)
       (and (zerop (or numarg 1)) (setq numarg 1)) ;make zero be equivalent to one
       (let ((target-line
	     (+ (startline rdis-selected-wlist) ;relative to this line on screen
	        (cond ((not numarg)		;go to middle of window
		     (quotient (numlines rdis-selected-wlist) 2))
		    ((< numarg 0)		;from bottom of window
		     (max 0 (+ numarg (numlines rdis-selected-wlist))))
		    (t			;from top of window
		      (min (1- numarg) (1- (numlines rdis-selected-wlist))))))))
	  (cond ((null (screen target-line))	;past end of buffer
	         (go-to-end-of-buffer))
	        (t			;there's a line there
		(go-to-line-point (eline (screen target-line)) 0)))))

;;; This new version by Barry Margolin.
(defun rdis-march-back-screen-lines (start count line-selector)
       (do ((n-lines 0)
	  (current (eline-previous start)	;start with previous line
		 (eline-previous current))
	  (last-current start current))
	 ((null current) last-current)	;until at start of buffer
	 (let ((n (rdis-/#-of-lines-in- current)))
	      (let ((new-n-lines (+ n-lines n)))
		 (cond ((= n-lines count)	;previous line is the one
		        (return (or (eline-next current) current)))
		       ((> new-n-lines count) ;this line does it
		        (cond ((eq line-selector 'current)
			     (return current))
			    ((eq line-selector 'next) ;wants next one
			     (return (or (eline-next current)
				       current)))
			    (t		;wants previous one
			      (return (or (eline-previous current)
				        current)))))
		       (t			;not there yet
		         (setq n-lines new-n-lines)))))))

(defun rdis-march-forward-screen-lines (start count line-selector)
       (do ((n-lines 0)
	  (current start (eline-next current)))	;from this line
	 ((null current) nil)		;ran off the edge
	 (let ((n (rdis-/#-of-lines-in- current)))
	      (let ((new-n-lines (+ n-lines n)))
		 (cond ((= n-lines count)	;this line is the one
		        (return current))
		       ((> new-n-lines count)	;this line is long and pushes us over
		        (cond ((eq line-selector 'current)
			     (return current))
			    ((eq line-selector 'next) ;wants next one
			     (return (or (eline-next current)
				       current)))
			    (t		;wants previous line
			      (return (or (eline-previous current)
				        current)))))
		       (t			;not there yet
		         (setq n-lines (+ n-lines n))))))))


(defun rdis-find-last-foundx (first num cl)
       (do ((x first (1+ x))
	  (lim (+ first num)))
	 ((not (< x lim))
	  (rbarf "rdis-find-last-foundx: can't."))
	 (cond ((eq (eline (screen x)) cl)
	        (setq last-foundx x)
	        (return nil)))))

;;;
;;;	Little interface for editor screen-hpos invocations
;;;

(defun go-to-screen-hpos (hp)
       (go-to-line-point curline (inverse-real-world-cursor-xcoord (wwtcomp curline) hp)))

(defun cur-screen-hpos ()
       (real-world-cursor-xcoord (wwtcomp curline) curpointpos))


;;; 
;;;
;;;	Reorganize screen on demand.
;;;

(defun reset-screen-size () (set-screen-size 2645.))


(defun set-screen-size (newsize)
       (rdis-assert-not-split-mode 'set-screen-size)
       (and split-mode-p
	  (display-error "You may not change screen size in split screen mode."))
       (setq newsize (min newsize (- screenheight (numlines minibufwindow)
			       (numlines modelwindow))))
       (cond ((< newsize 4)
	    (display-error "Invalid screen size: " (decimal-rep newsize)))
	   (t (cond ((= main-window-size newsize))
		  ((< main-window-size newsize)
		   (do i main-window-size (1+ i)(= i newsize)(store (screen i) nil)))
		  (t (do i newsize (1+ i)(= i main-window-size)
		         (cond ((and (screen i)(> (lineln (screen i)) 0))
			      (DCTL-position-cursor 0 i)
			      (DCTL-kill-line)))
		         (store (screen i) nil))))
	      (setq main-window-size newsize)
	      (rdis-reallocate-screen-evenly)
	      (rplac-numlines rdis-locdisp-window newsize))))


(defun rdis-cause-full-screen-recomputation ()
       (cond (split-mode-p			;only work hard if needed
	     (do ((i 0 (1+ i))
		(s))
	         ((= i nsplits))
	         (setq s (splits i))
	         (fillarray (split-eline-conts s) '(hphcs))    ;Not eq to anything
	         (setf (split-damaged s) t)))
	   (t				;non-split mode
	     (fillarray eline-conts '(hphcs)))) ;Not eq to anything
       (setq damaged-flag t
	   some-split-damaged split-mode-p))


(defun reset-minibuffer-size ()
       (rdis-assert-not-split-mode 'reset-minibuffer-size)
       (cond (tty-no-upmotionp
	    (set-minibuffer-size 1))
	   (t (set-minibuffer-size 2))))


(defun set-minibuffer-size (n)
       (rdis-assert-not-split-mode 'set-minibuffer-size)
       (prog (lucky-fellow newmlstart new-luckyfellow-size changed-top oldn)
	   (setq oldn (numlines minibufwindow))
	   (cond ((or (< n 1)(> n 6))(display-error "Invalid minibuffer size: " (decimal-rep n))))
	   (and (= n oldn)(return nil))
	   (setq lucky-fellow (windows (- nwindows 3)))
	   (setq new-luckyfellow-size
	         (- screenheight (startline lucky-fellow) n 2))
	   (cond ((< new-luckyfellow-size 3)
		(display-error "New minibuffer size would leave bottom window too small.")))
	   (setq newmlstart (- screenheight n 2))
	   (setq changed-top (cond ((< n oldn)(startline modelwindow))
			       (t newmlstart)))
	   (rplac-numlines lucky-fellow new-luckyfellow-size)
	   (rplac-startline modelwindow newmlstart)
	   (rplac-startline minibufwindow (+ 2 newmlstart))
	   (rplac-numlines minibufwindow n)
	   (setq main-window-size newmlstart)
	   (rplac-numlines rdis-locdisp-window newmlstart)
	   (setq rdis-last-echolinex (+ 2 newmlstart))
	   (do lx changed-top (1+ lx)(= lx screenheight)
	       (or (null (screen lx))(= 0 (lineln (screen lx)))
		 (progn (DCTL-position-cursor 0 lx)
		        (DCTL-kill-line)
		        (store (screen lx) nil))))
	   (rdis-cause-full-screen-recomputation)))

;;; 
;;;
;;;	Redisplay features to provide "local" displays that are
;;;	not editable and do not consume screen.
;;;
;;;	BSG 7/27/78
;;;

(defun init-local-displays ()
       (rdis-cause-full-screen-recomputation)
       (rdis-enter-local-display-split)
       (setq rdis-locdisp-linex
	   (cond (rdis-have-redisplayed (startline rdis-locdisp-window))
	         ((numberp rdis-locdisp-linex) rdis-locdisp-linex)
	         (t (startline rdis-locdisp-window))))
       (setq rdis-have-redisplayed nil)
       (cond (tty-no-upmotionp
	     (DCTL-nextline)
	     (setq rdis-last-tty-upprint-x -1)
	     (store (screen rdis-locdisp-linex) nil))))


(defun end-local-displays ()			;wait for response
       (rdis-cause-full-screen-recomputation)
       (cond (tty-no-upmotionp		;nothing to hold.
	     (e_pl1_$dump_output_buffer))
	   ((eq rdis-locdisp-linex 'abort)
	    (e_pl1_$dump_output_buffer)
	    (redisplay))
	   (t
	     (let ((local-display:force-no-more t))
		local-display:force-no-more
		(local-display-generator-nnl local-display-end-string))
	     (e_pl1_$dump_output_buffer)
	     (setq rdis-suppress-redisplay t))))


(defun local-display-generator-nnl (arg)
       (local-display-generator (catenate arg NLCHARSTRING)))


(defun local-display-current-line ()
       (local-display-generator (curline-as-string)))


(defun local-display-generator (string)
       (prog (rdis-line moregen-result nlines)
	   (or rdis-locdisp-linex (init-local-displays))
	   (and (eq rdis-locdisp-linex 'abort)(return t))
	   (setq rdis-line (parameterize-line (ncons string)))
	   (setq nlines (rdis-/#-of-lines-in- (eline rdis-line)))
	   (cond ((and tty-no-upmotionp overstrike-availablep))	;printing
	         ((= rdis-locdisp-linex (startline rdis-locdisp-window)))	;may have tried already
	         ((and (not local-display:force-no-more)
		     (> (+ nlines rdis-locdisp-linex)
		        (1- (numlines rdis-locdisp-window))))
		(setq moregen-result
		      (rdis-local-display-MORE-generator))))
	   (cond (moregen-result
		 (setq rdis-locdisp-linex 'abort)
		 (return nil)))
	   (do ((n nlines (1- n))
	        (rdl (linedata rdis-line) (substr rdl (1+ screenlinelen)))
	        (linel (lineln rdis-line)(- linel screenlinelen)))
	       ((= n 0))
	       (rdis-local-display-install-line
	         (cond ((> linel screenlinelen)(substr rdl 1 screenlinelen))
		     (t rdl))
	         (min screenlinelen linel)))
	   (setq rdis-suppress-redisplay t)
	   (return nil)))


(defun rdis-local-display-install-line (string len)
       (store (newscreen rdis-locdisp-linex)
	    (cons nil (cons string len)))
       (store (eline-conts rdis-locdisp-linex) 'random-not-found)
       (redisplay-line (newscreen rdis-locdisp-linex)
		   (screen rdis-locdisp-linex)
		   rdis-locdisp-linex)
       (cond ((screen rdis-locdisp-linex)
	    (let ((rdl (screen rdis-locdisp-linex)))
	         (rplac-linedata rdl string)
	         (rplac-lineln rdl len)))
	   (t (store (screen rdis-locdisp-linex)
		   (newscreen rdis-locdisp-linex))))
       (cond ((and tty-no-upmotionp overstrike-availablep)	;printing
	    (DCTL-nextline)
	    (store (screen 0) nil))
	   (t (aos rdis-locdisp-linex))))


(defun rdis-local-display-MORE-generator ()
       (rdis-local-display-install-line "--More?-- (space = yes, CR = no) " 33.)
       (DCTL-position-cursor 33. (1- rdis-locdisp-linex))
       (e_pl1_$dump_output_buffer)
       (prog1 (let ((char (get-char)))
	         (cond ((or (= char (CtoI "y"))(= char (CtoI " "))) nil)
		     (t t)))
	    (DCTL-position-cursor 0 (1- rdis-locdisp-linex))
	    (DCTL-kill-line)
	    (store (screen (1- rdis-locdisp-linex)) nil)
	    (setq rdis-locdisp-linex (startline rdis-locdisp-window))
	    (e_pl1_$dump_output_buffer)))

(defun set-lisp-rdis-meters ()
       (emacs$set_lisp_rdis_meters
         rdis-rdis-meter full-rdis-meter rdis-detabb-meter
         rdis-detab-opt-meter rdis-wgen-meter rdis-wgen-c1-meter
         rdis-wgen-c2-meter rdis-wgen-c3-meter rdis-ndf-opt-meter
         rdis-bad-echnego-meter))
;;;
;;;
;;; Split Management
;;;

;;;
;;;

(defun rdis-enter-split (new-split)
       ;; Opens a new split, saving the current split away. Moves cursor.
       (cond ((not (eq new-split current-split))	;only if different
	    (rdis-instate-split new-split)
	    (rdis-select-split new-split))))	;change on terminal

(defun rdis-select-window-split (window)
       ;; places the display cursor into the split that window is in
       (rdis-select-split (window-split window)))

(defun rdis-select-split (s)
       ;; activates a split on the terminal
       (cond ((not (eq rdis-cursor-split s))
	    (setq rdis-cursor-split s)
	    (setq X -777 Y -777)		;cursor location not kept for non-current splits
	    (DCTL-select-split (split-id s)))))

(defun rdis-instate-split (new-split)
       ;; saves current split data and instates new-split. Leaves cursor alone.
       (cond ((eq current-split new-split))	;don't work needlessly
	   (t (rdis-update-split-struct)	;save current state
	      (rdis-open-split new-split))))	; make it the new current split

(defun rdis-open-split (new-split)
       ;; opens a new split by unpacking it into global variables
       (setq current-split    new-split
	   screenlinelen    (split-line-length new-split)
	   screenheight	(split-height new-split)
	   main-window-size screenheight
	   damaged-flag	(split-damaged new-split)
	   screen		(split-screen new-split)
	   eline-conts	(split-eline-conts new-split)
	   windows	(split-windows new-split)
	   nwindows	(split-nwindows new-split)))

(defun rdis-update-split-struct ()
       ;; ensure split consistent after window creation/destruction
       (setf (split-damaged current-split) damaged-flag)
       (setf (split-nwindows current-split) nwindows)
       (or some-split-damaged			;keep track of damage
	 (setq some-split-damaged damaged-flag)))    ;   for redisplay

(defun rdis-enter-local-display-split ()
       (cond ((not split-mode-p))		;don't do un-needed work
	   (t (let ((new-locdisp-split (find-best-locdisp-split)))	;place it nicely
		 (cond ((not (eq new-locdisp-split rdis-locdisp-split))	;different
		        (setq rdis-locdisp-split new-locdisp-split)
		        ;;fix bogus window up
		        (setf (numlines rdis-locdisp-window) (split-height rdis-locdisp-split))
		        (setf (window-split rdis-locdisp-window) rdis-locdisp-split))))
	      (rdis-enter-split rdis-locdisp-split))))


;;; find best place to place local display
(defun find-best-locdisp-split ()
       (do ((ix 1 (1+ ix))
	  (s))
	 ((> ix nusplits) rdis-selected-split)	;use current if none found
	 (setq s (splits (usplits ix)))
	 (cond ((not (eq s rdis-selected-split))     ;use first usplit not = that of current window
	        (return s)))))

(defun rdis-create-split (sll swidth shgt shomex shomey window-to-place)
       ;; creates a split object and a terminal split
       (and (= nsplits maxsplits)
	  (display-error "Attempt to create too many splits: " (decimal-rep (1+ nsplits))))
       (let ((sid (pop split-ids-available))
	   (s))
	  (setq s (make-split id sid
			  line-length sll
			  width swidth
			  height shgt
			  home-X shomex
			  home-Y shomey
			  damaged t	;make redisplay take notice
			  screen (*array nil t shgt)
			  eline-conts (*array nil t shgt)
			  windows (*array nil t (max 1 (// (1+ shgt) 4))) ;at least 3 lines + divider in window
			  nwindows 1))
	  (setf (splits nsplits) s)
	  (setq nsplits (1+ nsplits))
	  (setf (arraycall t (split-windows s) 0) window-to-place)
	  (alter-window window-to-place
		      window-split s	;link to split
		      startline 0		;reposition at top
		      numlines shgt)	;window gets whole split
	  (DCTL-create-split sid shomex shomey swidth shgt)    ;leaves cursor there
	  s))				;return newly created split

(defun destroy-split (split-ix)
       (let ((sid (split-id (splits split-ix))))
	  (do ((i split-ix (1+ i)))		;move things down
	      ((= i (1- nsplits)))
	      (setf (splits i) (splits (1+ i))))
	  (setq nsplits (1- nsplits))
	  (setf (splits nsplits) nil)		;clear out last one
	  (DCTL-destroy-split sid)
	  (push sid split-ids-available)))	;free split id

(defun split-display-mode ()
       (prog ()
	   (if (not DCTL-hardware-windows-availablep)
	       (display-error "Split support is not available in your terminal."))
	   (if pop-up-windows		;this for JRM
	       (display-error "Turn off pop-up-windows. It is not supported with splits."))
	   (if split-mode-p
	       (display-error-noabort "Split display mode is already in effect.")
	       (return nil))
	   (setq full-screenlinelen screenlinelen    ;remember these for later reversion
	         full-screenheight screenheight)
	   (let
	     ((mbsize) (split-width) (split-ll) (split-hgt) (minisplit-width) (old-split0))
	     (setq mbsize (numlines minibufwindow)
		 split-width (// (rdis-real-ll screenlinelen) nuwindows)	;evenly apportion
					; forget about excess screen width.
					; It is almost always too much trouble (2 splits, no excess)
		 split-ll (1- split-width)	;in case terminal fumbles cursor at last char
		 split-hgt (- screenheight mbsize 2)	;screen less minibuf and mode/path
		 minisplit-width (rdis-real-ll screenlinelen))	;ensure minibuf full width

	     ;; make it look like clean slate
	     (setq old-split0 (splits 0))	;will be referencing contents but array ref is dead
	     old-split0			;reference it to stop compiler warning
	     (fillarray splits '(nil))
	     (fillarray usplits '(nil))
	     (setq nsplits 0 nusplits 0)
	     (setq split-ids-available nil)
	     (do ((i (1- DCTL-max-splits)
		   (1- i)))
	         ((< i 0))
	         (push i split-ids-available))

	     ;; create minibuffer split
	     (setq minibuffer-split		;minibuf at bottom
		 (rdis-create-split minisplit-width minisplit-width mbsize 0 (- screenheight mbsize) minibufwindow))

	     ;; create mode-line/path-line split
	     (setq model-split		;a two line split
		 (rdis-create-split minisplit-width minisplit-width 2 0 (- screenheight mbsize 2) modelwindow))

	     ;; now create user splits, one per user window
	     (do ((i 1 (1+ i))
		(s))
	         ((> i nuwindows))
	         (setq s (rdis-create-split split-ll split-width split-hgt (* (1- i) split-width) 0 (uwind i)))
	         (setf (uwindows i) (make-uwindow
				windowx 0 ;first window in split
				split s)) ;connect user window to split
	         (setf (usplits i) (1- nsplits))))   ;connect user split to split

	   (*rearray windows)		;scrap old windows stuff
	   (rdis-open-split (window-split rdis-selected-wlist))	;find current window in correct split
	   (setq rdis-selected-split current-split)
	   (rdis-select-split rdis-selected-split)

	   (setq split-mode-p t
	         nusplits nuwindows
	         suppress-redisplay-flag nil	;restart redisplay
	         rdis-suppress-redisplay nil
	         rdis-locdisp-linex nil)
	   (full-redisplay)))		;wake up redisplay

(defun rdis-real-ll (ll)
       ;; adjust a screen line-length to account for odd lengths
       ;; (which are due to terminal fumbling cursor in last column)
       (cond ((oddp ll) (1+ ll))
	   (t ll)))

(defun revert-split-mode ()
       ;; goes from split mode to standard display
       (or split-mode-p (display-error "Not in split display mode."))

       (rdis-restore-screen-to-one-split)

       ;; adjust the redisplay arrays
       (*rearray windows)
       (*rearray screen)
       (*rearray eline-conts)
       (setq windows (*array nil t 50.)
	   screen  (*array 'screen t full-screenheight)	;both named and ptr
	   eline-conts (*array 'eline-conts t full-screenheight))	;...

       ;; fix up windows
       (setf (windows 0) rdis-selected-wlist)
       (setf (windows 1) modelwindow)
       (setf (windows 2) minibufwindow)
       (setq nwindows 3
	   nuwindows 1)

       ;; fix up splits
       (fillarray usplits '(nil))
       (setf (usplits 1) 0)
       (fillarray splits '(nil))
       (setf (splits 0) (make-split id 0
			      line-length full-screenlinelen
			      width full-screenlinelen
			      height full-screenheight
			      home-X 0
			      home-Y 0
			      damaged t
			      screen screen
			      eline-conts eline-conts
			      windows windows
			      nwindows 3))
       (setq nsplits 1
	   nusplits 1
	   current-split (splits 0)
	   rdis-locdisp-split nil
	   rdis-cursor-split current-split
	   rdis-selected-split current-split
	   model-split current-split
	   minibuffer-split current-split)

       ;; make windows refer to correct split
       (setf (window-split rdis-selected-wlist) (splits 0))
       (setf (window-split modelwindow) (splits 0))
       (setf (window-split minibufwindow) (splits 0))

       ;; fix user window array
       (fillarray uwindows '(nil))
       (setf (uwindows 1) (make-uwindow windowx 0 split (splits 0)))

       ;; fix redisplay variables
       (setq rdis-lru-stack '(1)
	   rdis-multiwindowed-buflist nil
	   selected-window 1		;user window index
	   rdis-selected-windowx 0		;real window index
	   two-window-mode nil
	   screenheight full-screenheight
	   screenlinelen full-screenlinelen
	   main-window-size (- screenheight (numlines minibufwindow) (numlines modelwindow))
	   split-mode-p nil)

       ;; fix local displays
       (setf (numlines rdis-locdisp-window) main-window-size)
       (setf (window-split rdis-locdisp-window) (splits 0))
       (setq rdis-locdisp-split nil)

       ;; fix fake windows
       (setf (startline modelwindow) main-window-size)
       (setf (startline minibufwindow) (+ 2 main-window-size))

       ;; now make it take
       (full-redisplay))

(defun init-split-management ()
       ;; initializes the innards of split screen stuff
       (setq splits (*array nil t DCTL-max-splits)
	   usplits (*array nil t DCTL-max-splits))
       (fillarray splits '(nil))
       (fillarray usplits '(nil 0 nil))
       (setf (splits 0)
	   (make-split id 0
		     line-length screenlinelen
		     height screenheight
		     damaged t
		     screen screen
		     eline-conts eline-conts
		     windows windows
		     nwindows 3))		;mini, model + 1 user window
       (let ((split-0 (splits 0)))
	  (setf (window-split minibufwindow) split-0)
	  (setf (window-split modelwindow) split-0)
	  (setf (window-split (uwind 1)) split-0)    ;fix user window struct
	  (setf (uwindow-split (uwindows 1)) split-0)
	  (setq split-mode-p nil
	        nsplits 1
	        nusplits 1
	        maxsplits (cadr (arraydims splits))
	        maxusplits (1- (cadr (arraydims usplits)))     ;usplit 0 unused
	        current-split split-0
	        minibuffer-split current-split
	        model-split current-split
	        rdis-selected-split current-split
	        )))

(defun rdis-restore-screen-to-one-split ()
       (cond (split-mode-p			;protect it
	     (do ((i 1 (1+ i)))
	         ((= i nsplits))
	         (DCTL-destroy-split (split-id (splits i))))
	     (DCTL-create-split (split-id (splits 0))	;make 0 full screen
			    0 0		;home position
			    (rdis-real-ll full-screenlinelen)	;adjust for cursor wierdness
					;		  at eol fudge
			    full-screenheight)
	     (e_pl1_$dump_output_buffer)
	     (sleep 0.75))))		;MCS write-abort write-around

(defun rdis-recreate-splits-on-screen ()
       (and split-mode-p			;protect it
	  (do ((i 0 (1+ i))
	       (s))
	      ((= i nsplits) (e_pl1_$dump_output_buffer))
	      (setq s (splits i))
	      (DCTL-create-split (split-id s)
			     (split-home-X s) (split-home-Y s)
			     (split-width s) (split-height s)))))

(defun rdis-assert-not-split-mode (operation)
       (and split-mode-p			;barf if splits on
	  (display-error operation " is not supported in split display mode."))
       t)					;passed, return something nice 
