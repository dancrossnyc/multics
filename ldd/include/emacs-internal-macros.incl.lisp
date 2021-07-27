;;; BEGIN INCLUDE FILE emacs-internal-macros.incl.lisp

;;; Loads in e_internal_macros_

;;; HISTORY COMMENTS:
;;;  1) change(85-01-06,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-20,Harvey), install(86-08-20,MR12.0-1136):
;;;     Created.
;;;                                                      END HISTORY COMMENTS

(%include defstruct)
(%include setf)

(eval-when (compile eval)
  (or (status feature e-internal-macros)
      (load (catenate (car (namelist (truename infile)))
		  ">e_internal_macros_")))
  (sstatus feature e-internal-macros))

(declare
  (*expr e$get_temporary_seg e$release_temporary_seg
         e_lap_$ggcharn e_lap_$gsubstr e_lap_$return-string e_lap_$rtrim
         e_lap_$segnlindex e_lap_$write-string
         exists-buffer get-buffer-state map-over-emacs-buffers
         order-mark-last))

(declare
  (special buffer-file-dtcm		;DTCM of the file associated with the current buffer
	 buffer-tempsegs		;list of temp segs for current buffer
	 buffer-uid		;Multics UID of segment this buffer is "eq" to
				;for arch. comp, (UID . compname)
	 curline			;current line
	 curlinel			;length of current line
	 curpointpos		;# of chars to left of cursor on line
	 curstuff			;the current line (string or filecons)
	 firstline		;first line of buffer
	 known-buflist		;list of defined buffers
	 lastline			;last line of current buffer
	 minibufferp		;non-nil if in minibuffer
	 tty-no-upmotionp		;non-display terminal
	 work-string		;black-magic string containing open line
	 ))

;;; END INCLUDE FILE emacs-internal-macros.incl.lisp
