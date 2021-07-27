;;; BEGIN INCLUDE FILE emacs-rdis-dcls.incl.lisp


;;; HISTORY COMMENTS:
;;;  1) change(87-12-23,Schroth), approve(), audit(), install():
;;;     Original Written: Way back when by BSGreenberg
;;;  2) change(87-12-23,Schroth), approve(88-02-29,MCR7852),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Updated for split-screen changes: defined displayline and split
;;;     structures, changed named arrays to array pointers.  Used
;;;     defmacro and defstruct changes from Barry Margolin.
;;;                                                      END HISTORY COMMENTS



;;; Include file for Emacs redisplay modules

(%include defmacro)
(declare (macros nil))			;drop these macros at load
(%include defstruct)
(%include backquote)
(%include setf)

;;; Macro to make an array pointer look like a named array
;;; This creates a special variable and an access macro with the name given
;;; as the first argument.  The remaining variable number of arguments define
;;; the number of arguments the access macro needs and correspond to array
;;; dimensions.  Note that the array is NOT allocated by this macro.  The
;;; programmer must do so manually.
;;; Note that since the access macro generates an 'arraycall', it may be
;;; used with setf.  This is not the case with named arrays.

(defmacro defarray (array-name &rest indices)
	`(progn 'compile
	        (declare (special ,array-name))
	        (defmacro ,array-name (,@indices)
		        (list 'arraycall t ',array-name ,@indices))))

(defarray screen line-index)			;Screen rdis-lines
(defarray newscreen line-index)		;New windows being built
(defarray eline-conts line-index)		;Old linecontents

(defarray windows window-num)			;Window structures
(defarray uwindows uwindow-index)		;User-indices into windows.

;;;
;;; Screen Hardware Window Data Structures
;;;
;;; Hardware windows are called "splits" to avoid confusion with the EMACS
;;; concept of a window.
;;;

(defarray splits split-index) 		;one entry / hardware window

;;; The above array has one entry per hardware window the terminal can display.
;;; When not in split mode, only one is active and it is not really used.
;;; In split mode, this array stores the entire redisplay state for that
;;; split (hardware window) as if it were the entire screen.
;;; Moving to a new window causes a context switch using this array.
;;; See the (defstruct (split ...)) definition below.


(defarray usplits usplit-number)		;one entry per user split
					; these index into the
					; splits array

(defmacro aos (x) `(incf ,x))
(defmacro sos (x) `(decf ,x))
(defmacro rbarf (message &optional object)
	`(error ,message ,object 'fail-act))

;;;
;;;  Screen/editorline structures
;;;
(defstruct (window
	   (:eval-when (eval compile))
	   (:type list))
	 (startline 0)
	 (numlines 0)
	 (bufmark nil)
	 (bufsym nil)
	 (window-split nil))		;split the  window is in

(defmacro rplac-startline (window new-startline)
	`(setf (startline ,window) ,new-startline))
(defmacro rplac-numlines (window new-numlines)
	`(setf (numlines ,window) ,new-numlines))
(defmacro rplac-bufmark (window new-bufmark)
	`(setf (bufmark ,window) ,new-bufmark))
(defmacro rplac-bufsym (window new-bufsym)
	`(setf (bufsym ,window) ,new-bufsym))
(defmacro rplac-split (window split)
	`(setf (window-split ,window) ,split))

(defstruct (uwindow				;user window array element
	   (:eval-when (eval compile))
	   (:type list)
	   (:conc-name))
	 (windowx 0)			;index into windows array
	 (split nil))			;split owning the uwindow

(defmacro uwind (window-number)
	`(windows (uwindow-windowx (uwindows ,window-number))))
(defmacro uwind-split (window-number)		;returns split given uwindows index
	`(uwindow-split (uwindows ,window-number)))

;;;
;;; Display-line structure.
;;; (This commentary lifted from e_redisplay)
;;;
;;;	Screen is maintained as the array "screen", containing knowledge
;;;	and images of screen. Each element of "screen" is called a "displayline",
;;;	and looks like this:
;;;
;;;	(editorline "printablerepresentationwithnonewline" . printinglength)
;;;
;;;	The array "newscreen" is used during redisplay computation only.
;;;
;;;	The array "eline-conts" parallels the window array of redisplay lines
;;;	maintaining what e_ calls "line-contents" so that an "eq" check can
;;;	be made (see redisplay-window) to avoid detabbification and resultant
;;;	consing, for eq lines with eq contents cannot detabbify differently.

(defstruct (displayline
	   (:eval-when (eval compile))
	   (:type list*))			;save a cons cell
	 (eline nil)			;the editorline
	 (linedata nil)			;the directly displayable image w/o NL
	 (lineln 0))			;interesting length of image
;;;
;;; Display-line special macros. These operate on the screen array elements.
;;;
(defmacro rplac-eline (displayline new-eline)
	`(setf (eline ,displayline) ,new-eline))
(defmacro rplac-linedata (displayline new-linedata)
	`(setf (linedata ,displayline) ,new-linedata))
(defmacro rplac-lineln (displayline new-lineln)
	`(setf (lineln ,displayline) ,new-lineln))

;;;
;;; The screen split data structure
;;;
;;; This structure is used to contain the data relevant to a screen split (hardware window)
;;; which is viewed as a seperate virtual screen.  As such, all display screen
;;; data is stored in a split structure when that split is not active. When a split is
;;; active, its data is copied into the globalredisplay and window manager data
;;; (after the prior data.)
;;;

(defstruct (split
	   (:eval-when (eval compile))
	   (:type array)			;use arrays to speed access to elements
	   (:conc-name))
	 (line-length 0)			;usable split width (consider cursor wrap, scrolling etc.)
	 (width 0)			;real width on screen (used only be creation stuff)
	 (height 0)			;split depth = main-window-size
	 (id 0)				;used to talk to CTL
	 (home-X 0)			;home location on ABS screen
	 (home-Y 0)
	 (damaged t)			;needs to be redisplayed
	 (screen nil)			;array of display-lines
	 (eline-conts nil)			;array of corr. elines
	 (windows nil)			;the windows in this split
	 (nwindows 0))


;;; END   INCLUDE FILE emacs-rdis-dcls.incl.lisp
