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
;;;	Multics Emacs Window Manager


;;; HISTORY COMMENTS:
;;;  1) change(84-01-19,Margolin), approve(), audit(), install():
;;;     pre-hcom history:
;;;               27 April 1979 by BSG
;;;               To DLW, MARG, RMS, and all the others who
;;;                think/thought about this mishegoss all day long.
;;;     Modified: 19 January 1984 - Barmar - commented out register-option form,
;;;                   as it was moved to e_option_defaults_.
;;;  2) change(84-12-25,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Slashified #'s, changed lambda's to
;;;     let's, use defmacro, use the uwind macro in places where it
;;;     it is spelled out.
;;;  3) change(84-12-26,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Fixed bug in rdis-update-window-struct
;;;     that I put in last night when rewriting lambda's.
;;;  4) change(84-12-27,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Fix the rewritten lambda in rdis-window-totenpurge.
;;;  5) change(84-12-28,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     remove buggy optimization from rdis-update-window-struct.
;;;  6) change(85-01-06,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     changed to use set-mark-here instead
;;;     of rplac'ing marks manually.  This may also fix some bugs, since
;;;     it now updates curline-marklist.  Changed to use make-mark
;;;     and make-eline in wman-init, rather than cons.
;;;  7) change(88-01-15,Schroth), approve(88-02-29,MCR7852),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     Implement Window Mgr portions of Split Screen Display.
;;;     Used some defstruct's suggested by Barry Margolin.
;;;                                                      END HISTORY COMMENTS


;;;	(Multics Emacs screen mgmt, vers. 3)

(%include defmacro)
(declare (macros nil))
(%include backquote)
(%include e-macros)				;for defvar
(%include emacs-internal-macros)
(%include emacs-rdis-dcls)
(%include other_other)

(declare (*expr rdis-instate-split rdis-update-split-struct rdis-assert-not-split-mode))
(declare (*expr decimal-rep del-mark-from-buffer e_lap_$gsubstr empty-buffer-p
	      get-buffer-state go-to-mark go-to-or-create-buffer gstrgl 
	      rdis-cause-full-screen-recomputation set-mark set-mark-here wwtcomp))
;;;
;;;	Declarations.  See main redisplay for meaning.
;;;
;;; (register-option 'pop-up-windows nil) ;moved to e_option_defaults_
(declare (*lexpr display-error display-error-noabort minibuffer-print
	       display-error-remark))
(declare (genprefix /!rdis_wman_))
(declare (special current-buffer minibufferp numarg damaged-flag number-of-lines-in-buffer))
(declare (special screenheight main-window-size default-new-window-size))
(declare (special pop-up-windows rdis-suppress-rupdate tty-no-upmotionp))
(declare (special known-buflist))
(declare (special two-window-mode selected-window modelwindow minibufwindow
	        nwindows nuwindows rdis-splln-mark phony-modeline-edline))

(declare (special  screenlinelen rdis-lru-stack rdis-multiwindowed-buflist
	         rdis-selected-wlist rdis-selected-windowx rdis-locdisp-window
	         current-split nsplits rdis-selected-split split-mode-p))
(declare (array* (fixnum (wman-lrux-array ?))))
;;;
;;;	Window management initialization. Called at end of rdis-init.
;;;

(defun wman-init ()
       (setq nuwindows 1 nwindows 3 selected-window 1 two-window-mode nil)
       (and tty-no-upmotionp (setq pop-up-windows nil))
       (setq rdis-suppress-rupdate nil)
       (setq windows (*array nil t 50.))	;changed to array pointer Aug/85 EDS
       (setq uwindows (*array nil t 50.))
       (*array 'wman-lrux-array 'fixnum screenheight)
       (setq main-window-size (- screenheight 3) default-new-window-size nil)
       (store (windows 0) (make-window numlines main-window-size))
       (store (windows 1) (setq modelwindow
			  (make-window startline (- screenheight 3)
				     numlines 2)))
       (store (windows 2) (setq minibufwindow
			  (make-window startline (- screenheight 1)
				     numlines 1)))
       (setq rdis-splln-mark (make-mark
			 eline (make-eline
			         contents
			         (do ((c "--------" (catenate c c)))
				   ((> (stringlength c) screenlinelen)
				    (substr c 1 (1+ screenlinelen)))))
			 position 0))
       (setq rdis-lru-stack (list 1) rdis-multiwindowed-buflist nil)
       (fillarray uwindows '((nil nil) (0 nil) (nil nil)))
       (setq rdis-selected-wlist (windows 0) rdis-selected-windowx 0)
       (setq rdis-locdisp-window
	   (make-window numlines (numlines (windows 0))))
       (rplac-bufmark (windows 1) (make-mark eline phony-modeline-edline
				     position 0))
       nil)


;;;
;;;
;;;	Window from-editor and from-redisplay updates.
;;;


;; Called at buffer-kill time from buffer-kill


(defun redisplay-purge-buffer (bufnam)
       (prog (orign u)
	   (or (boundp 'nuwindows)(return nil))
a
       (setq orign nuwindows u 1)
b
       (redisplay-purge-buffer-window bufnam u)
       (or (= orign nuwindows)(go a))
       (aos u)
       (and (> u orign)(return nil))
       (go b))))

;;; Re-initting abuffer must flush all marks, which will
;;; not be relocated, and might point to a scrapped temp-seg
;;; from the redisplay.  The next select on that window would
;;; redisplay around that garbage did we not do this.
;;; (see (cond ((bufmark... in select-window).

(defun redisplay-buffer-reinit-purge (bufnam)
       (do u 1 (1+ u)(> u nuwindows)
	 (let ((w (uwind u)))
	      (cond ((eq bufnam (bufsym w))
		   (rplac-bufmark w nil))))))

(defun lruify-current-window ()(rdis-lruify-window selected-window))
(defun lruify-window (u)(rdis-lruify-window u))

(defun find-buffer-in-window (bufnam)
       (let ((found-window (buffer-on-display-in-window bufnam)))
	  (cond (found-window
		(select-window found-window))
	        (pop-up-windows (wman-place-buffer bufnam))
	        (t (select-window (car (last rdis-lru-stack)))
		 (go-to-or-create-buffer bufnam)))
	  (rdis-update-window-struct)))

(defun buffer-on-display-in-window (bufnam)
       (do u 1 (1+ u)(> u nuwindows)
	 (and (eq bufnam (bufsym (uwind u)))
	      (return u))))

;;;
;;;
;;;	Dynamic redisplay-time window maintenance.
;;;

(defun rdis-update-window-struct ()		;Called by redisplay et al.
       (cond (rdis-suppress-rupdate)
	   ((eq current-buffer (bufsym rdis-selected-wlist))
	    (or (bufmark rdis-selected-wlist)
	        (rplac-bufmark rdis-selected-wlist (set-mark)))
	    (let ((m (bufmark rdis-selected-wlist))) ;makes marks
	         (set-mark-here m)))		;update the mark
	   (t (rdis-upd-virtual-window-point rdis-selected-wlist)))
       ;; Update LRU stack
       (setq rdis-lru-stack
	   (cons selected-window
	         (delq		;MULTICS MACLISP DEPENDENCY EQ FIXNUMS
		 selected-window rdis-lru-stack))))


(defun rdis-upd-virtual-window-point (window)
       ;; 85-09-10 EDS to look at windows through all splits
       (cond ((numberp window)(setq window (windows window))))
       (cond ((not (eq current-buffer (bufsym window)))
	    (do ((u 1 (1+ u))		;user window index
	         (testbuf)
	         (oldbuf (bufsym window))
	         (oldoccurs 0)	;Multiplicity of oldbuf
	         (newoccurs 0))	;Multiplicity of newbuf
	        ((= u nuwindows)
	         (setq oldoccurs (1- oldoccurs) newoccurs (1+ newoccurs))
	         (cond ((< oldoccurs 2)
		      (setq rdis-multiwindowed-buflist
			  (delq oldbuf rdis-multiwindowed-buflist))))
	         (cond ((> newoccurs 1)
		      (setq rdis-multiwindowed-buflist
			  (cons current-buffer (delq current-buffer rdis-multiwindowed-buflist))))))
	        (setq testbuf (bufsym (uwind-real-window u)))
	        (cond ((eq testbuf oldbuf)(aos oldoccurs))
		    ((eq testbuf current-buffer)(aos newoccurs))))))
       (del-mark-from-buffer (bufmark window)(bufsym window))
       (rplac-bufsym window current-buffer)
       (rplac-bufmark window (set-mark))))


(defun redisplay-purge-buffer-window (bufnam u)
       (let ((window (uwind-real-window u)))
	  (cond ((eq (bufsym window) bufnam)
	         (cond (pop-up-windows
		       (wman-fenestra-nata-est-virgo u)
		       (cond ((> nuwindows 1)
			    (select-other-window)
			    (delete-window u))))
		     (t (rdis-lruify-window u)
		        (rdis-fenestra-nata-est-virgo window)
		        (rplac-bufmark window nil)))))))
;; Leave bufsym around, no empty windows please.

;;; Some utility functions

(defun uwind-real-window (u)
       ;; returns true window given user window index factoring in splits
       (let ((uw (uwindows u)))
	  (arraycall t
		   (split-windows (uwindow-split uw))	;containing split's window array
		   (uwindow-windowx uw))))	;index of uwindow into same

(defun nuwindows-in-split (nrws)
       ;; computes number of uwindows given number of real windows in split
       ;; user windows in a split are followed by separator windows, hence // 2
       (cond (split-mode-p (// (1+ nrws) 2))
	   (t	       (// (1- nrws) 2))))	;don't count model or minibuf
;;;
;;;
;;;	Called by e_ when a buffer is exited.
;;;

(defun redisplay-leave-buffer ()		;current buffer implied
       (do ((u 1 (1+ u))
	  (slcbuf (bufsym rdis-selected-wlist))
	  (window))
	 ((> u nuwindows))
	 (setq window (uwind-real-window u))
	 (cond ((eq current-buffer (bufsym window))  ;Got one with guy in it
	        (cond ((or (eq window rdis-selected-wlist)     ;Update real guy
		         (not (eq current-buffer slcbuf)))
		     (rdis-bufleave-upd window)
		     (return nil)))))))  ;Dont upd many windows of same.

(defun rdis-bufleave-upd (window)
       (cond ((null (bufmark window))
	    (rplac-bufmark window (set-mark)))
	   (t (let ((m (bufmark window)))
		 (set-mark-here m)))))
;;;

;;;  Window genesis, no-pop up case.

(defun rdis-nata-est-fenestra ()		;Window is born
       (and (or (> nwindows (- (cadr (arraydims windows)) 4))
	      (> nuwindows (- (cadr (arraydims uwindows)) 2)))
	  (display-error "Too many windows, total."))
       (prog (ux wx window nnuw nnw quo)
	   (setq nnuw (1+ nuwindows)
	         nnw (+ 2 nwindows)
	         quo (// main-window-size (nuwindows-in-split nnw)))
	   (or (> quo 2)(display-error "Too many windows for this screen size."))
	   (setq wx (cond (split-mode-p (1- nnw))    ;no minibuf and model
		        (t	  (- nnw 3)))  ;only split has it all
	         ux nnuw
	         window (make-window
		        startline 0 numlines 0 bufmark nil
		        bufsym (make_atom (catenate "Window " (decimal-rep ux) " Default"))
		        window-split current-split))
	   (store (windows (- nnw 1))(windows (- nwindows 1)))
	   (store (windows (- nnw 2))(windows (- nwindows 2)))
	   (store (windows (- nnw 3)) window)
	   (store (windows (- nnw 4)) (wman-create-divider 0))
	   (store (uwindows ux) (make-uwindow windowx wx split current-split))
	   (setq nuwindows nnuw nwindows nnw)
	   (setq two-window-mode t)
	   (rdis-reallocate-screen-evenly)
	   (rdis-lruify-window ux)
	   (rdis-update-split-struct)))

(defun rdis-lruify-window (u)
       (setq rdis-lru-stack
	   (nconc (delq u rdis-lru-stack)(list u))))

(defun rdis-fenestra-nata-est-virgo (w)		;Gets done by cause-full-rc
       (do ((x (startline w)(1+ x))		;in creation case.
	  (ctr (numlines w)(1- ctr)))
	 ((= ctr 0))
	 (store (eline-conts x) 'hphcs)))	;See redisplay, rdis-wdw

(defun rdis-reallocate-screen-evenly ()
       (let ((nuws (nuwindows-in-split nwindows)))
	  (do ((w 0 (1+ w))
	       (startl 0)
	       (real-ws 0)
	       (thisw)
	       (howdeep)
	       (quo (// main-window-size nuws)) ;window + sep line size
	       (rem (\ main-window-size nuws))) ;extra lines
	      ((= real-ws nuws))
	      (setq thisw (windows w))
	      (cond ((eq (bufmark thisw) rdis-splln-mark)
		   (setq howdeep 1))
		  ((< real-ws rem)
		   (setq howdeep quo)
		   (aos real-ws))
		  (t (setq howdeep (1- quo))
		     (aos real-ws)))
	      (and (= real-ws nuwindows)(setq howdeep (1+ howdeep)))
	      (rplac-startline thisw startl)
	      (rplac-numlines thisw howdeep)
	      (setq startl (+ startl howdeep)))
	  (rdis-cause-full-screen-recomputation)))

;;;

;;; Window destruction

(defun remove-window ()			;command
       (rdis-assert-not-split-mode 'remove-window)
       (delete-window (or numarg selected-window)))
	    
;;; Enter one window mode
(defun expand-window-to-whole-screen ()
       (rdis-assert-not-split-mode 'expand-window-to-whole-screen)
       (do ((u 1 (1+ u))
	  (windows-to-go))
	 ((> u nuwindows)
	  (mapc 'rdis-delete-uwindow windows-to-go))
	 (or (= u selected-window)
	     (setq windows-to-go (cons u windows-to-go)))))

(defun delete-window (u)
       (rdis-assert-not-split-mode 'delete-window)
       (cond ((or (< u 1)(> u nuwindows))
	    (display-error "Invalid window number: " (decimal-rep u)))
	   ((not two-window-mode)
	    (display-error "Not in multi-window mode"))
	   ((= u selected-window)
	    (select-other-window)))
       (rdis-delete-uwindow u)
       (rdis-update-split-struct))

(defun rdis-delete-uwindow (u)
       ;; delete user window given index
       (let ((uw (uwindows u))		;link to real window
	   (ocs current-split)		;saved current-split
	   (uws))				;split containing uwindow
	  (setq uws (uwindow-split uw))
	  (and (eq (uwind u) rdis-selected-wlist)
	       (display-error "Attempt to destroy selected window"))
	  (and (= 1 (split-nwindows uws))	;only 1 window in split?
	       (display-error "Attempt to destroy only window in split"))
	  (do uu 0 (1+ uu)(> uu nuwindows)
	      (cond ((> uu u)(store (uwindows (1- uu))(uwindows uu)))))
	  (sos nuwindows)
	  (rdis-instate-split uws)		;switch splits if needed
	  (rdis-delete-rwindow (uwindow-windowx uw))
	  (rdis-instate-split ocs)		;restore real current split
	  (setq rdis-lru-stack (delq u rdis-lru-stack))
	  (and (> selected-window u)(sos selected-window))
	  (setq two-window-mode (> nuwindows 1))
	  (map '(lambda (x)(and (> (car x) u)(rplaca x (1- (car x)))))
	       rdis-lru-stack)))

(defun rdis-delete-rwindow (r)
       (prog (upper lower nlines window scbottom upstairsadd downstairsadd)
	   (setq window (windows r) scbottom (1- main-window-size))
	   (setq upper (startline window) nlines (numlines window)
	         lower (+ upper (1- nlines)) nlines (1+ nlines))
	   (cond ((and (= upper 0)(= lower scbottom))
		(rbarf "rdis-delete-rwindow: deleting all that's left")))
	   (rdis-window-totenpurge window)
	   (cond ((= upper 0)		;This is the top window
		(rdis-remove-divider (1+ r))
		(rdis-adjust-window (1+ r)(- nlines) nlines))
	         ((= lower scbottom)
		(rdis-remove-divider (1- r))
		(sos r)
		(rdis-adjust-window (1- r) 0 nlines))
	         (t (rdis-remove-divider (1+ r))
		  (setq upstairsadd (// nlines 2) downstairsadd (- nlines upstairsadd))
		  (rdis-adjust-window (- r 2) 0 upstairsadd)
		  (rdis-adjust-window (- r 1) upstairsadd 0) ;divider
		  (rdis-adjust-window (+ r 1) (- downstairsadd) downstairsadd)))
	   (rdis-condense-out-window r)
	   (rdis-cause-full-screen-recomputation)))

(defun rdis-remove-divider (w/#)
       (or (eq (bufmark (windows w/#)) rdis-splln-mark)(rbarf "rdis-remove-divider: not a divider: " w/#))
       (rdis-condense-out-window w/#))

(defun rdis-condense-out-window (w/#)
       (do w 0 (1+ w)(= w nwindows)
	 (and (> w w/#)(store (windows (1- w))(windows w))))
       (and (= w/# rdis-selected-windowx)
	  (rbarf "rdis-condense-out-window: called on current: " w/#))
       (and (> rdis-selected-windowx w/#)(sos rdis-selected-windowx))
       (sos nwindows)
       (do ((u 1 (1+ u))
	  (uw))
	 ((> u nuwindows))
	 (setq uw (uwindows u))
	 (and (> (uwindow-windowx uw) w/#)
	      (decf (uwindow-windowx uw)))))

(defun rdis-adjust-window (w addstart addnl)
       (setq w (windows w))
       (rplac-startline w (+ addstart (startline w)))
       (rplac-numlines w (+ addnl (numlines w))))

(defun rdis-window-totenpurge (window)
;;; This thoroughly ingenious hack totally cleans out all traces of the
;;; buffer that was in here and updates the multiwindowed list.
       (let ((ocb current-buffer)
	   (current-buffer (gensym)))
	  (rdis-upd-virtual-window-point window)
	  (del-mark-from-buffer (bufmark window) ocb)))
;;;
;;;
;;;	Demand Window Selection.
;;;

(defun select-window (utag)
       (prog (window)
	   (and minibufferp (display-error "No window selection from minibuffer."))
	   (and (or (< utag 1)
		  (> utag nuwindows))
	        (display-error "Non-existant window number: " (decimal-rep utag)))
;;; This next line is a source of infinite grief and the root of all hair
;;; and bugs.  When not in pop-up mode, it ensures that ^XB/^XO done
;;; "real fast" (redisplayless) indeed updates the new buffer into the
;;; old window, if not the old buffer would not show up in the wlist.
;;; Now in pop-up mode, it is completely wrong, because people
;;; have to find-buffer-in-window current-buffer's, which would tend to update
;;; that buffer into old and new windows.  "What is truth?" -Pilate.

	   (and (or (not pop-up-windows)
		  (eq current-buffer (bufsym rdis-selected-wlist)))
	        (rdis-update-window-struct))
	   (setq selected-window utag)
	   (rdis-instate-split (uwind-split utag))   ;switch splits
	   (setq window (uwind utag))
	   (setq rdis-selected-wlist window
	         rdis-selected-windowx (uwindow-windowx (uwindows utag))
	         rdis-selected-split (window-split window))
	   (go-to-or-create-buffer (bufsym window))
	   (cond ((bufmark window)(go-to-mark (bufmark window))))
	   (rdis-update-window-struct)
	   (setq damaged-flag t)))

(defun create-new-window-and-stay-here ()
       (rdis-assert-not-split-mode 'create-new-window-and-stay-here)
       (rdis-nata-est-fenestra))

(defun create-new-window-and-go-there ()
       (rdis-assert-not-split-mode 'create-new-window-and-go-there)
       (rdis-nata-est-fenestra)
       (rdis-select-lru-window))

(defun select-another-window ()
       (and (not two-window-mode)
	  (display-error "Not in two window mode."))
       (cond ((not numarg)(rdis-select-lru-window))
	   ((or (< numarg 1)(> numarg nuwindows))
	    (display-error "Invalid window number: " (decimal-rep numarg)))
	   (t (select-window numarg))))

(defun rdis-select-lru-window ()
       (or (cdr rdis-lru-stack)(display-error "No alternate window to select."))
       ;; The above error should not happen.
       (select-window (car (last rdis-lru-stack))))


(defun select-other-window ()
       (cond ((> nuwindows 1)
	    (and (cdr rdis-lru-stack)(select-window (cadr rdis-lru-stack))))
	   (t (display-error "Not in 2-window mode"))))


;;;
;;;
;;;	Externally available utilities needed by window editor.
;;;


(defun window-info (u)
       (and (or (< u 1)(> u nuwindows))
	  (display-error "window-info: no such window: " (decimal-rep u)))
       (let ((w (uwind-real-window u)))
	  (list (cons (startline w)(numlines w))     ;bounds
	        (uwindow-windowx (uwindows u))	;internal window index
	        (bufsym w)			;buffer
	        (cond ((null (bufmark w)) nil)	;char string on line
		    (t (let ((s (wwtcomp (car (bufmark w)))))
			  (e_lap_$gsubstr s 0 (gstrgl s)))))
					;split number of the window
	        (cond ((not split-mode-p) 0)
		    (t (do ((s (window-split w))
			  (split-num 0 (1+ split-num)))
			 ((= split-num nsplits)  ;should not get here!!!
			  (display-error "Could not find window split."))
			 (cond ((eq s (splits split-num))
			        (return split-num)))))))))

(defun window-adjust-upper (u deltaf)
       (rdis-assert-not-split-mode 'window-adjust-upper)
       (and (or (< u 2)(> u nuwindows))
	  (display-error "window-adjust-upper: bad window #: " (decimal-rep u)))
       (let ((w (uwindow-windowx (uwindows u))))
	  (rdis-adjust-window (- w 2) 0 deltaf)
	  (rdis-adjust-window (- w 1) deltaf 0)
	  (rdis-adjust-window w deltaf  (- deltaf))
	  (rdis-cause-full-screen-recomputation)
	  (assign-current-wsize w)))

(defun window-adjust-lower (u deltaf)
       (rdis-assert-not-split-mode 'window-adjust-lower)
       (and (or (< u 1)(> u (1- nuwindows)))
	  (display-error "window-adjust-lower: bad window #: " (decimal-rep u)))
       (let ((w (uwindow-windowx (uwindows u))))
	  (rdis-adjust-window w 0 deltaf)
	  (rdis-adjust-window (+ w 1) deltaf 0)
	  (rdis-adjust-window (+ w 2) deltaf (- deltaf))
	  (rdis-cause-full-screen-recomputation)
	  (assign-current-wsize w)))

(defun assign-current-wsize (w)
       (setq w (windows w))
       (and (bufmark w)(putprop (bufsym w)(numlines w) 'window-size)))


;;;
;;;
;;;	Dynamic (pop-up) window policy and implementation department.
;;;

;;; Put buffer buf someplace appropriate on the screen.
;;; This is an esoteric form of select-window.  It is critical to note
;;; that find-buffer-in-window does a rdis-update-window-strct after calling
;;; this.

(defun wman-place-buffer (buf)
       (let ((u (wman-allocate-window (wman-buf-wsize buf))))
	  (and (eq buf (bufsym (uwind u)))
	       (rdis-update-window-struct))	;moby hair.
	  ;;see select-window, same thing.
	  (setq selected-window u
	        rdis-selected-windowx (uwindow-windowx (uwindows u))
	        rdis-selected-wlist (windows rdis-selected-windowx)
	        rdis-selected-split (window-split rdis-selected-wlist))
	  (setq damaged-flag t)
	  (go-to-or-create-buffer buf)))
	  
;;; Find a good place of size size to put a window.

(defun wman-allocate-window (size)
       (cond ((wman-find-unused-window size))	;set.
	   (t (wman-fill-lrux-array)
	      (let ((start (wman-find-rottenest-space (1+ size))))
		 (or (= start 0)
		     (= (+ start size) main-window-size)
		     (setq start (1+ start)))
		 (wman-metamorphose start size)))))

;;; Find out a buffer's wanted window size.

(defun wman-buf-wsize (buf)
       (let ((prop (get buf 'window-size)))
	  (cond ((fixp prop) prop)
	        ((eq prop 'share)
	         (min (// main-window-size 2)
		    (do ((u 1 (1+ u))
		         (m 0))
		        ((> u nuwindows) m)
		        (setq m (max m (numlines (uwind u)))))))
	        ((= nuwindows 1) main-window-size)
	        (default-new-window-size)
	        (t (// main-window-size 2)))))


;;; Find a totally useless window for first choice.

(defun wman-find-unused-window (size)		;Find unused space that fits
       (do ((u 1 (1+ u))			;best.
	  (m main-window-size)
	  (mu nil))
	  ((> u nuwindows) mu)
	 (and (get (bufsym (uwind u)) 'nulls-windows-buffer)
	      (not (< (numlines (uwind u)) size))
	      (< (numlines (uwind u)) m)
	      (setq m (numlines (uwind u)) mu u))))


;;; not used.

(defun wman-find-lruness (u)
       (do ((l rdis-lru-stack (cdr l))
	  (d 1 (1+ d)))
	 ((null l) d)
	 (and (= (car l) u)(return d))))


;;; Set up the array with the LRU depth of each screen line.

(defun wman-fill-lrux-array ()
       (let ((ld (1+ (length rdis-lru-stack))))
	  (fillarray 'wman-lrux-array (list (1+ ld)))
	  (do ((l rdis-lru-stack (cdr l))
	       (d 1 (1+ d)))
	      ((null l) d)
	      (do ((c (numlines (uwind (car l)))(1- c))
		 (lx (startline (uwind (car l)))(1+ lx)))
		((= c 0))
		(store (wman-lrux-array lx) d)))))

(defun wman-find-rottenest-space (height)
       (setq height (min main-window-size height))
       (do ((rotsx 0 (1+ rotsx))		;index of.
	  (best-try-index)
	  (just-how-rotten-was-it 0)
	  (stopx (- main-window-size height)))
	 ((> rotsx stopx) best-try-index)
	 (do ((c height (1- c))
	      (lx rotsx (1+ lx))
	      (total 0))
	     ((= c 0)(cond ((> total just-how-rotten-was-it)
			(setq just-how-rotten-was-it total
			      best-try-index rotsx))))
	     (and (or (null (screen lx))
		    (= 0 (lineln (screen lx))))
		(aos total))		;Counts points!
	     (setq total (+ total (wman-lrux-array lx))))))

;;;
;;;
;;;	wman-metamorphose returns an index (uwindow) for a window
;;;	at line start for size (not including dividers).  He will
;;;	destroy all current windows contained therein, take one over,
;;;	and chop into others to make it so.   He will not leave 0-line
;;;	windows, nor rend an extant window in twain.

(defun wman-metamorphose (start size)
   (rdis-assert-not-split-mode 'pop-up/ windows)
   (prog2 
       (rdis-cause-full-screen-recomputation)
       (prog (mytop mybot histop hisbot ux w try-here dchop w/#)
					;Terminology is geographic
	   (setq mytop (1- start) mybot (+ size start))	;not numeric
	   (setq ux 1)			;loop uwindows
loop
	   (and (> ux nuwindows)(go pass2))
	   (setq w (uwind ux))
	   (setq histop (1- (startline w)) hisbot (+ histop (numlines w) 1))
	   (cond ((not (< histop mybot))(go pass2))  ;clear below us
	         ((not (> hisbot mytop))	;clear above us
		(aos ux))
	         ((and (= hisbot mybot)(= histop mytop))	;'xact match!
		(return ux))		;WOW!
	         ((and (< histop mytop)	;eat up oneliner on top
		     (not (< histop (- mytop 2))))
		(setq mytop histop))
	         ((and (not (< histop mytop))	;completely contained within
		     (not (> hisbot mybot)))	;flush it
		(wman-delete-window ux)
		(or (= ux 1)(sos ux)))
	         ((and (> hisbot mybot)	;Bottom short.
		     (not (> hisbot (+ 2 mybot))))
		(setq mybot hisbot))
	         ((> histop mybot)(rbarf "wman-metamorphose: err 3 "
				   (list ux mytop mybot histop hisbot)))
	         ((and (< histop mytop)(> hisbot mybot))	;dont split window
		(setq mytop (+ mytop (- hisbot mybot)) mybot hisbot))
	         ((and (= (abs (- histop mybot)) 1)  ;dont move 1 up down
		     (> (- mybot mytop) 4))
		(setq mybot histop))
	         ((and (= (abs (- mytop hisbot)) 1)
		     (> (- mybot mytop) 4))
		(setq mytop hisbot))
	         (t (or try-here (setq try-here ux))
		  (aos ux)))
	   (go loop)
;;;
pass2
;;;  Two cases wrt try-here:
;;;    1. We cut out of his bottom and maybe the next guy's top.
;;;    2. We cut out of his top alone.
;;;  There is no case of upper guy's top, or we'd be case 1 on him.

	   (setq ux try-here)		;for typing ease!
	   (setq w/# (uwindow-windowx (uwindows ux)))
	   (setq w (windows w/#))
	   (setq histop (1- (startline w)) hisbot (+ histop (numlines w) 1))
	   (setq size (- mybot mytop 1) start (1+ mytop))
	   (cond ((< histop mytop)		;Case 1
		(setq dchop (- mybot hisbot))
		(and (or (> hisbot mybot)
		         (not (> hisbot mytop)))
		     (rbarf "wman-metamorphose.pass2: err case 1 "
			  (list ux mytop mybot histop hisbot)))
		(wman-push-down-uwnums (1+ ux))
		(rdis-adjust-window w/# 0 (- mytop hisbot))
		(wman-push-down-rwnums (1+ w/#) 2)
		(store (windows (+ 1 w/#))(wman-create-divider mytop))
		(store (windows (+ 2 w/#))
		       (wman-fenestrarum-genetrix start size (1+ ux)))
		(cond ((and (not (= ux (1- nuwindows)))
			  (> dchop 0))
		       (rdis-adjust-window (+ 3 w/#) dchop 0)
		       (rdis-adjust-window (+ 4 w/#) dchop (- dchop))))
		(store (uwindows (1+ ux))
		       (make-uwindow windowx (+ 2 w/#)
				 split (window-split w)))
		(return (1+ ux)))
	         (t			;case 2.
		 (and (or (not (> hisbot mybot))
			(> histop mytop))
		      (rbarf "wman-metamorphose.pass2: err case 2 "
			   (list ux mytop mybot histop hisbot)))
		 (wman-push-down-uwnums ux)
		 (wman-push-down-rwnums w/# 2)
		 (rdis-adjust-window (+ 2 w/#) (1+ size)(- (1+ size)))
		 (store (windows w/#)
		        (wman-fenestrarum-genetrix start size ux))
		 (store (windows (1+ w/#))(wman-create-divider mybot))
		 (store (uwindows ux)
		        (make-uwindow windowx w/# split (window-split w)))
		 (return ux))))
       (setq two-window-mode t)))

;;;
;;;
;;;	Friends and utilities of wman-metamorphose.
;;;

(defun wman-fenestrarum-genetrix (sl nl u/#)
       (let ((sym (maknam (append '(n u l l i t y /. )(explodec u/#)))))
	  (putprop sym t 'nulls-windows-buffer)
	  (make-window startline sl numlines nl bufmark nil bufsym sym window-split current-split)))

(defun wman-create-divider (lx)
       (make-window startline lx numlines 1 bufmark rdis-splln-mark bufsym nil window-split current-split))

(defun wman-delete-window (u)
       (and (= selected-window u)
	  (select-other-window))
       (rdis-delete-uwindow u))

(defun wman-push-down-uwnums (u)
       (map '(lambda (x)(or (< (car x) u)(rplaca x (1+ (car x)))))
	  rdis-lru-stack)
       (or (< selected-window u)(aos selected-window))
       (aos nuwindows)
       (do x nuwindows (1- x)(= x u)
	 (store (uwindows x)(uwindows (1- x))))
       (store (uwindows u) (make-uwindow windowx -1 split nil)))

(defun wman-push-down-rwnums (w/# d)
       (or (< rdis-selected-windowx w/#)
	 (setq rdis-selected-windowx (+ rdis-selected-windowx d)))
       (setq nwindows (+ d nwindows))
       (do x (1- nwindows)(1- x)(= (- x d)(1- w/#))
	 (store (windows x)(windows (- x d))))
       (do u 1 (1+ u)(> u nuwindows)
	 (or (< (uwindow-windowx (uwindows u)) w/#)
	     (incf (uwindow-windowx (uwindows u)) d))))


(defun wman-fenestra-nata-est-virgo (u)
       (setq rdis-lru-stack (delq u rdis-lru-stack))
       (rdis-fenestra-nata-est-virgo (uwind u))
       (store (uwind u)
	    (wman-fenestrarum-genetrix (startline (uwind u))
				 (numlines (uwind u))
				 u))
       (and (= u selected-window)
	  (setq rdis-selected-wlist (uwind u)
	        rdis-selected-split (window-split rdis-selected-wlist))))

(defun assign-buffer-window-size ()
       (putprop current-buffer (numlines rdis-selected-wlist) 'window-size))
;;;
;;;
;;;	Buffer window size hacking primitives.
;;;


;;; Callable interface from editor.

(defun select-buffer-window (buf key)
       (cond (pop-up-windows
	     (putprop buf (select-buffer-window-size-interpreter buf key)
		    'window-size)
	     (find-buffer-in-window buf))
	   (t (go-to-or-create-buffer buf))))

(defun select-buffer-find-window (buf key)
       (cond (pop-up-windows (select-buffer-window buf key))
	   ((eq buf current-buffer)(find-current-buffer-in-window))
	   (t (find-buffer-in-window buf))))

(defun select-buffer-window-size-interpreter (buf size)
       (cond ((and (eq size 'default-cursize)(get buf 'window-size))
	    (setq size (get buf 'window-size)))
	   ((and (eq size 'cursize-not-empty)(empty-buffer-p buf))
	    (setq size nil)))
       (or (fixp size)(memq size '(float nil))
	 (setq size
	       (cond ((memq buf known-buflist)
		    (get-buffer-state buf 'number-of-lines-in-buffer))
		   (t nil))))
       (cond ((fixp size)
	    (and (< size 1)(setq size 1))
	    (and (> size (// (*  main-window-size 3) 5))
	         (setq size nil))))
       (and (not (eq size 'float)) size))

(defun find-current-buffer-in-window ()
       (find-buffer-in-window-noupdate current-buffer))

(defun find-buffer-in-window-noupdate (buf)
       (let ((rdis-suppress-rupdate t))
	  (find-buffer-in-window buf))
       (rdis-update-window-struct))
