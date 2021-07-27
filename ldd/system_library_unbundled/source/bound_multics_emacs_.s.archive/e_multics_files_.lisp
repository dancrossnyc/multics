;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;	Multics File Manipulation


;;; HISTORY COMMENTS:
;;;  1) change(84-01-19,Margolin), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;      GMP, 09/23/78
;;;     Modified 28 July 1979 by GMP to add protected file query
;;;     Modified 24 September 1979 by CRD to fix find-file,
;;;      added archive primitives
;;;     Modified 28 October 1979 by BSG for starnames/archivenames.
;;;     Mod 4/29/80 BSG to finish starnames/archivenames
;;;     Modified 5/8/80 by BSG for tecoish (reasonable) newline handling.
;;;     Modifed 7 May 1981 Soley for object_info_ calls.
;;;     Modified 5 November 1981 Soley for no-newline writes.
;;;     Modified 19 November 1981 Soley to ignore dirs on "emacs **"
;;;     Modified 31 October 1983 Barmar to query in read-file if buffer
;;;              buffer modified, and convert it to defcom.
;;;     Modified 19 January 1984 Barmar to comment out register-option
;;;              forms, as they were moved to e_pathname_defaults_.
;;;  2) change(85-01-27,Margolin), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;     Modified 23 January 1984 Barmar to fix fencepost error in read-in-file,
;;;              affecting files with no trailing newline.  Also cleaned up
;;;              the code (with-mark/save-excursion instead of explicit
;;;              set-mark/release-mark).  Changed find-file-subr-single-entry
;;;              to set fpathname if it finds the segment in an existing
;;;              buffer but fpathname is no longer valid, to not check
;;;              for object seg in this case.
;;;              Changed find-file-get-buffer-given to inform the user when
;;;              it finds such a buffer.
;;;              Changed terminate-file to use terminate_file_.
;;;     Modified 25 December 1984 B. Margolin to move e_pl1_$object_check
;;;              out to e_defpl1_.lisp, use defmacro, delete macros defined
;;;              in e-macros.incl.lisp.
;;;     Modified 6 January 1985 - Barmar - to use filecons defstruct and
;;;              emacs-internal-macros, deleted unnecessary macro defs.
;;;     Modified 27 January 1985 - Barmar - to fix buffer-ends-in-newline?
;;;              to use eline-contents, not eline-conts.
;;;  3) change(86-01-17,Margolin), approve(86-01-17,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Almost totally rewritten in order to support MSFs and checking of file
;;;     DTCM against the buffer before overwriting.  Removed many declarations
;;;     which were already in e-macros or which I moved to
;;;     emacs-internal-macros.
;;;  4) change(86-05-24,Margolin), approve(86-05-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Fixed find-file mode-setting function so it would turn the suffix into
;;;     a symbol before looking for the suffix-mode property.
;;;     Fixed get-filename to set the mark before inserting the pathname.
;;;     Changed read-seg-into-buffer to always insert-string the first line of
;;;     the segment, even if already at beginning of line, to prevent a
;;;     problem with marks moving.
;;;     Added allow-no-match parameter to match-star-name, to allow callers to
;;;     specify whether zero matches should cause an error (uninstalled
;;;     find-lisp-source needs this capability).
;;;     Changed open-output-file to check for a non-zero, non-noentry
;;;     msf-code, and to check for a zero init-code before checking for
;;;     archive_component_modification.
;;;     Changed open-file-reject-starname to use equal instead of =, because
;;;     entry-code-1 and comp-code-1 can be set to symbols.
;;;  5) change(86-09-20,Margolin), approve(86-10-10,MCR7553),
;;;     audit(86-10-17,Lippard), install(86-11-11,MR12.0-1209):
;;;     Changed write-out-string-line to fix a fencepost error when point is
;;;     in the middle of the line.
;;;                                                      END HISTORY COMMENTS


;;; 

(declare (genprefix /!emf_))

(%include e-macros)
(%include emacs-internal-macros)
;;; %include all the library except for format
(%include sharpsign)
(%include setf)
(%include runtime)
(sstatus feature runtime)			;bypass for bug
(%include other_other)
(%include macro_macros)
(%include loop)
(%include destructuring_let)
(%include defun)
(%include defmacro)
(%include backquote)

(declare (macros nil)
         (*expr buffer-kill check_star_name_$entry
	      e_multics_files_util_$force_msf_access e_multics_files_util_$get_dtcm
	      e_multics_files_util_$get_dtcm_file e_multics_files_util_$nth_star_match
	      e_multics_files_util_$restore_msf_access e_multics_files_util_$star_list_cleanup
	      e_multics_files_util_$star_list_init
	      e_pl1_$object_check
	      expand_pathname_$component
	      hcs_$fs_get_mode hcs_$get_max_length_seg
	      hcs_$get_uid_file hcs_$get_uid_seg hcs_$truncate_seg
	      initiate_file_$component insert-new-line
	      msf_manager_$adjust
	      msf_manager_$close msf_manager_$get_ptr msf_manager_$open
	      reinitialize-current-buffer
	      terminate_file_))

(declare
  (*lexpr absolute-pathname close-file expand-pathname))

(defvar (
;;;	 Flags to ease user transition to the "right way"  5/26/80

	 (find-file-entry-names-buffer nil)     ;flag for buffer names same as entry names
	 (std-eob-protocol t)		;flag for tecoish eob
	 ;;as of 8/14/80, e_multics_files_ ignores this
	 read-file-force			;should ^X^R overwrite mod. buffer?
	 (R_ACCESS (dpb #2r100 #o4103 0))	;"100"b
	 (RW_ACCESS (dpb #2r101 #o4103 0))	;"101"b
	 (W_ACCESS_BIN #2r00010)
	 (TERM_FILE_TERM (dpb 1 #o4101 0))	;"001"b
	 (MSFMA_TRUNC_BC (dpb #2r100 #o4103 0)) ;"100"b
	 find-file-check-dtcm		;options
	 save-same-file-check-dtcm write-file-overwrite
	 check-newline add-newline find-file-set-modes
	 ))

;;; 

(defun check-entry-name (name &aux code)
       (unless (zerop (setq code (check_star_name_$entry name)))
	     (report-error code SPACE name)))

(defun check-read-only ()
       (and read-only-flag (report-error 'read-only)))

;;; Returns the file object, unless it is an object segment, in which
;;; case it aborts with an error message.
(defun check-object (file-object)
       (when (and (not (null (fobj-contents file-object)))	;empty MSF isn't object file
	        (= 1 (e_pl1_$object_check (* 9. (fobj-length file-object))
				    (fobj-pointer file-object))))
	   (close-file file-object nil)
	   (report-file-error "File is an object segment:" file-object))
       file-object)

(defmacro lastlinep () '(null (nextline)))


;;; Option to ignore no-newline end of buffer when writing.
;;; (register-option 'check-newline nil) ; The wrong thing. ;moved to e_option_defaults_
;;; (register-option 'add-newline   't)  ; The right thing. ;moved to e_option_defaults_

;;; This predicate returns whether or not the current buffer ends in a
;;; NEWLINE.  We have to special case the silly case wherein we're on
;;; the last line AND the line is open.
(defun buffer-ends-in-newline? ()
       (samepnamep (cond ((and (lastlinep)
			 (eq curstuff work-string))
		      work-string)
		     (t (eline-contents lastline)))
	         NLCHARSTRING))

(defun check-newline-ending ()
       (and check-newline
	  (not (buffer-ends-in-newline?))
	  (not
	    (yesp "This buffer does not end in a newline.  Write anyway? "))
	  (command-quit)))

;;; Release the specified temporary files
(defun release-temp-segments (tempsegptrs)
       (cond (tempsegptrs
	     (mapc 'e$release_temporary_seg
		 tempsegptrs))))


;;; 
;;;
;;;	Lowest level primitives
;;;

;;; First some macros for readable efficiency
(eval-when (compile eval)
	 (setq *bits-per-char* 9.))

(defmacro bc-to-chars (bc)
	`(// (+ ,bc ,(1- #.*bits-per-char*))
	     #.*bits-per-char*))

(defmacro chars-to-bc (chars)
	`(* ,chars #.*bits-per-char*))

(defmacro open-file-check-code (form pathname complain)
	`(let (((value code) ,form))
	      (cond ((= 0 code) value)
		  (t (open-file-error code ,pathname ,complain)))))

(defmacro make-char-pointer (base-pointer char-offset)
	(once-only (char-offset)
		 `(alter-packed-pointer
		    ,base-pointer
		    word-offset (lsh ,char-offset -2)
		    bit-offset (* (logand ,char-offset 3)
			        #.*bits-per-char*))))

(defmacro add-char-offset (pointer chars)
	(once-only (pointer)
		 `(let ((words (pptr-word-offset ,pointer))
		        (bits (pptr-bit-offset ,pointer)))
		       (make-char-pointer
		         ,pointer
		         (+ ,chars (lsh words 2)
			  (// bits #.*bits-per-char*))))))

;;; 

;;; Subr used to open a file used by the visible primitives
;;; Name should be a pathname, access-needed should be either
;;; read -> open for input, archive component permitted
;;; write -> open for output, no archive component
;;; write-force -> open for output, force access if user says OK, still
;;;	         no archive component
;;; write/write-force will create if necessary.  Complain should be
;;; t -> abort command if any error found.
;;; nil -> silently return nil on file error.
;;; noabort -> print error and return nil.
;;; Returns a file object if successful.
(defun open-file (name access-needed &optional (complain t))
       (*catch
         'open-file-error
         (prog
	 ()
	 (let (((path expand-code) (expand-pathname name t)))
	      (cond ((= 0 expand-code) (setq name path))
		  ;;always complain about bad pathname
		  ((eq complain 'noabort)
		   (display-com-error-noabort expand-code name)
		   (return nil))		;but skip the rest of this
		  (t (display-com-error expand-code name))))
	 ;; the caller should have expanded starnames if appropriate
	 (open-file-reject-starname name complain)
	 (return
	   (selectq access-needed
		  (read (open-input-file name complain))
		  (write (open-output-file name complain nil))
		  (write-force (open-output-file name complain 'ask))
		  (t (error "Invalid open-file access-needed:"
			  access-needed 'wrng-type-arg)))))))

(defun open-input-file (pathname complain)
       (let ((dir (pn-directory pathname))
	   (ent (pn-entry pathname))
	   (comp (pn-component pathname))
	   (ptr null-pointer) bc init-code result)
	  (protect
	    (desetq (ptr bc init-code)
		  (initiate_file_$component dir ent comp R_ACCESS))
	    (setq result
		(cond ((= init-code (error_table_ 'dirseg))
		       (open-input-msf pathname dir ent complain))
		      ((= 0 init-code)
		       (open-input-segment pathname ptr bc complain))
		      (t (open-file-error init-code pathname complain))))
	    &failure
	    (terminate_file_ ptr 0 TERM_FILE_TERM))
	  result))

;;; 

(defun open-input-segment (pathname ptr bit-count complain
			      &aux file-obj)
       (setq file-obj
	   (make-file-object
	     path pathname
	     contents (list (make-filecons pointer ptr
				     length (bc-to-chars bit-count)))
	     fcb-ptr nil))
       (let ((uid (open-file-check-code (hcs_$get_uid_seg ptr)
				pathname complain))
	   (comp (pn-component pathname)))
	  (alter-file-object
	    file-obj
	    uid (cond ((nullstringp comp)
		     uid)
		    (t (cons uid comp)))	;unique to the component
	    dtcm (open-file-check-code
		 (e_multics_files_util_$get_dtcm ptr)
		 pathname complain)))
       file-obj)

(defun open-input-msf (pathname dir entry complain
			  &aux
			  file-obj (fcb-ptr null-pointer) msf-code)
       (unless (nullstringp (pn-component pathname))
	     (open-file-error 'error_table_$dirseg pathname complain))   ;good enough
       (protect
         (desetq (fcb-ptr msf-code)
	       (msf_manager_$open dir entry))
         (unless (= msf-code 0)
	       (open-file-error msf-code pathname complain))
         (setq file-obj
	     (make-file-object
	       path pathname
	       fcb-ptr fcb-ptr
	       contents (open-msf-get-components fcb-ptr pathname complain)
	       uid (open-file-check-code (hcs_$get_uid_file dir entry)
				   pathname complain)
	       dtcm (open-file-check-code
		    (e_multics_files_util_$get_dtcm_file dir entry)
		    pathname complain)))
         &failure
         (unless (= fcb-ptr null-pointer)
	       (msf_manager_$close fcb-ptr)))
       file-obj)

(defun open-msf-get-components (fcb-ptr pathname complain)
       (loop for component from 0
	   for (ptr bc code) = (msf_manager_$get_ptr fcb-ptr component 0)
	   while (= code 0)
	   collect (make-filecons pointer ptr
			      length (bc-to-chars bc))
	   finally (unless (= code (error_table_ 'noentry))    ; last component
		         (open-file-error code pathname complain))))

;;; 

;;; This does one non-obvious thing.  It tries to initiate_file_$component
;;; in order to determine whether the output file is an archive component,
;;; by checking for error_table_$archive_component_modification.  It doesn't
;;; merely check for pn-component being non-null, in case we ever implement
;;; the "implicit archive component" feature.
(defun open-output-file (pathname complain force-access
			    &aux
			    dir entry)
       (unless (nullstringp (pn-component pathname))
	     (open-file-error 'error_table_$archive_component_modification
			  pathname complain))
       (setq dir (pn-directory pathname)
	   entry (pn-entry pathname))
       (let ((ptr null-pointer)
	   init-code)
	  (protect
	    (desetq (ptr () init-code)
		  (initiate_file_$component dir entry "" RW_ACCESS))
	    &always
	    (terminate_file_ ptr 0 TERM_FILE_TERM))
	  (cond ((= init-code 0))
	        ((= init-code
		  (error_table_ 'archive_component_modification))
	         (open-file-error init-code pathname complain))))
       (let ((fcb-ptr null-pointer)
	   msf-code)
	  (protect
	    (desetq (fcb-ptr msf-code) (msf_manager_$open dir entry))
	    (cond ((= msf-code 0)
		 (open-output-msf pathname fcb-ptr
			        force-access complain))
		((= msf-code (error_table_ 'noentry))
		 (open-msf-create pathname fcb-ptr complain))
		(t (open-file-error msf-code pathname complain)))
	    &failure
	    (unless (= fcb-ptr null-pointer)
		  (msf_manager_$close fcb-ptr)))))

(defun open-msf-create (pathname fcb-ptr complain)
       (let ((contents
	     (let (((seg-ptr bc ptr-code)
		  (msf_manager_$get_ptr fcb-ptr 0 -1)))	;Create component 0
		(unless (= ptr-code 0)
		        (open-file-error ptr-code pathname complain))
		(list (make-filecons
		        pointer seg-ptr
		        length (bc-to-chars bc))))))
	  (make-file-object
	    path pathname
	    fcb-ptr fcb-ptr
	    contents contents
	    dtcm (open-file-check-code
		 (e_multics_files_util_$get_dtcm
		   (filecons-pointer (car contents)))
		 pathname complain)
	    uid (open-file-check-code
		(hcs_$get_uid_seg
		  (filecons-pointer (car contents)))
		pathname complain))))

;;; 

;;; This opens an output MSF (or SSF) that is known to exist.
(defun open-output-msf (pathname fcb-ptr force-access complain)
       (let* ((contents (open-msf-get-components fcb-ptr pathname complain))
	    (first-component (filecons-pointer (first contents)))
	    (file-obj
	      (make-file-object
	        path pathname
	        fcb-ptr fcb-ptr
	        contents contents)))
	   (protect
	     (open-output-msf-check-access
	       pathname file-obj fcb-ptr first-component force-access
	       complain)
	     (alter-file-object
	       file-obj
	       uid (open-file-check-code (hcs_$get_uid_seg first-component)
				   pathname complain)
	       dtcm (open-file-check-code 
		    (e_multics_files_util_$get_dtcm first-component)
		    pathname complain))
	     &failure
	     (restore-access file-obj))
	   file-obj))

;;; 

(defun open-output-msf-check-access (pathname file-obj fcb-ptr segptr
				      force-access complain)
       (let ((access			;check component 0 access
	     (open-file-check-code (hcs_$fs_get_mode segptr)
			       pathname complain))
	   (orig-access null-pointer))
	  (when (= (logand access W_ACCESS_BIN) 0)   ;no write access
	        (selectq
		force-access
		((t ask)
		 (when (eq force-access 'ask)
		       (ring-tty-bell)
		       (unless
		         (yesp (catenate
			       "Do you want to write to the protected file "
			       (absolute-pathname pathname) "?"))
		         (open-file-error 0 nil complain)))
		 (protect
		   (setq orig-access
		         (open-file-check-code
			 (e_multics_files_util_$force_msf_access
			   fcb-ptr)
			 pathname complain))
		   &failure
		   (unless (= orig-access null-pointer)
			 (e_multics_files_util_$restore_msf_access
			   fcb-ptr orig-access))
		   &success
		   (setf (fobj-original-access file-obj)
		         orig-access)))
		(nil (open-file-error 'error_table_$moderr pathname
				  complain))
		(t (error "Internal error: Invalid open-output-file force-access:" force-access 'wrng-type-arg))))))

(defun restore-access (file-object)
       (let ((orig (fobj-original-access file-object)))
	  (when orig
	        (unless (= null-pointer orig)
		      (e_multics_files_util_$restore_msf_access
		        (fobj-fcb-ptr file-object) orig)))))

(defun open-file-reject-starname (pathname complain)
       (let* ((entry (pn-entry pathname))
	    (comp (pn-component pathname))
	    (entry-code (check_star_name_$entry entry))
	    (comp-code (cond ((nullstringp comp) 0)
			 ((= entry-code 0)	;only bother if entry valid
			  (check_star_name_$entry comp))
			 (t 0)))
	    (entry-code-1
	      (cond ((> entry-code 2) entry-code)
		  ((> entry-code 0) 'error_table_$nostars)
		  (t 0)))
	    (comp-code-1
	      (cond ((> comp-code 2) comp-code)
		  ((> comp-code 0) 'error_table_$nostars)
		  (t 0))))
	   (cond ((not (equal 0 entry-code-1))
		(open-file-error entry-code-1 pathname complain))
	         ((not (equal 0 comp-code-1))
		(open-file-error comp-code-1 pathname complain)))))

(defun open-file-error (error-code pathname complain)
       (cond ((eq complain 'noabort)
	    (report-error-noabort error-code " "
			      (car (absolute-pathname pathname t))))
	   (complain (report-error
		     error-code " " (car (absolute-pathname pathname t)))))
       (*throw 'open-file-error nil))

;;; 

;;; Close the file, restoring access, terminating segments, and
;;; releasing storage.  If set-bc-p is non-nil then the bit count
;;; of the file should be updated from the contents list (it is ignored
;;; if the file is not an MSF/SSF (i.e. a segment/component), which are
;;; only opened for input.  We also update the UID and DTCM, in case
;;; the caller needs the latest values.
(defun close-file (file-object &optional set-bc-p)
       (when file-object
	   (let ((fcb-ptr (fobj-fcb-ptr file-object))
	         (contents (fobj-contents file-object)))
	        (alter-file-object file-object
			       fcb-ptr null-pointer
			       contents nil)
	        (cond ((and fcb-ptr (not (= fcb-ptr null-pointer)))
		     (let (((uid dtcm)
			  (close-msf fcb-ptr contents set-bc-p
				   (fobj-original-access file-object))))
			(when uid
			      (setf (fobj-uid file-object)
				  uid))
			(when dtcm
			      (setf (fobj-dtcm file-object)
				  dtcm))))
		    (t (close-segment contents))))))

;;; When processing set-bc-p, we assume that msf_manager_$adjust
;;; takes care of the non-last components.  The caller should have
;;; filled these components to their max-lengths.  Returns
(defun close-msf (fcb-ptr contents set-bc-p orig-access &aux (result nil))
       (when contents
	   (when set-bc-p
	         (msf_manager_$adjust fcb-ptr (1- (length contents))
				(chars-to-bc
				  (filecons-length
				    (car (last contents))))
				MSFMA_TRUNC_BC))
	   (when (car contents)
	         (let* ((seg (filecons-pointer
			   (car contents)))
		      ((uid uid-code)
		       (hcs_$get_uid_seg seg))
		      ((dtcm dtcm-code)
		       (e_multics_files_util_$get_dtcm seg)))
		     (setq result
			 (list (and (= uid-code 0)
				  uid)
			       (and (= dtcm-code 0)
				  dtcm))))))
       (and orig-access
	  (not (= orig-access null-pointer))
	  (e_multics_files_util_$restore_msf_access
	    fcb-ptr orig-access))
       (msf_manager_$close fcb-ptr)
       result)

;;; Terminate an input segment (output always uses msf_manager_).
(defun close-segment (contents)
       (terminate_file_ (filecons-pointer (car contents)) 0
		    TERM_FILE_TERM))

;;; 

;;;
;;;	File/buffer reading/writing primitives
;;;


;;; Reads the given file-object into the buffer at the cursor
(defun read-file-into-buffer (file-object)
       (minibuffer-remark "Reading...")
       (mapc 'read-seg-into-buffer (fobj-contents file-object))
       (minibuffer-remark ""))		;clear above remark

(defun read-seg-into-buffer (filecons &aux char-count temp-ptr temp-filecons
			        seg-ends-in-nl last-nl-index first-nl-index
			        reverse-nl-index)
       (unless
         (=  (setq char-count (filecons-length filecons))
	   0)				;skip empty segment
         (setq temp-ptr (e$get_temporary_seg)
	     temp-filecons (make-filecons
			 pointer temp-ptr
			 length char-count))
         (push temp-ptr buffer-tempsegs)	;add this one to list of
					;temporary segs
         (e_lap_$write-string filecons temp-ptr 0);copy to temp-seg
         (setq seg-ends-in-nl (= #\newline
			   (e_lap_$ggcharn temp-filecons
				         (1- char-count))))
         (cond (seg-ends-in-nl
	       (setq reverse-nl-index 0
		   last-nl-index char-count))
	     (t (setq reverse-nl-index
		    (e_lap_$reverse-search-string
		      temp-filecons char-count NLCHARSTRING)
		    last-nl-index
		    (cond ((= reverse-nl-index -1) 0) ;no newlines!
			(t (- char-count reverse-nl-index))))))
         (setq first-nl-index
	     (cond ((= last-nl-index 0) 0)	;no newlines
		 (t (1+ (e_lap_$segnlindex temp-ptr last-nl-index 0)))))
         ;; Make sure the loop starts at beginning of line.
         ;; Do it even if already (bolp), because the loop below inserts
         ;; BEFORE the line that is then current, but we want to insert AFTER
         ;; the current point, so that marks are left in the right places.
         (unless (= first-nl-index 0)
	       (insert-string
	         (e_lap_$gsubstr temp-filecons 0 first-nl-index)))
         (do ((char-index first-nl-index (+ char-index line-len))
	    (line-ptr)
	    (line-len))
	   ((>= char-index last-nl-index))	;loop over the full lines
	   (setq line-len
	         (1+ (e_lap_$segnlindex temp-ptr last-nl-index
				  char-index)))
	   (setq line-ptr (make-char-pointer temp-ptr char-index))
	   (insert-new-line (make-filecons	;make representation of line
			  pointer line-ptr
			  length line-len)))
         (unless seg-ends-in-nl
	       (insert-string
	         (e_lap_$gsubstr temp-filecons last-nl-index
			     (- char-count last-nl-index))))))

;;;
;;; Writes the specified region of the buffer to the given file-object.
;;; May move the mark.
(defun write-point-mark-to-file (file-object mark)
       (minibuffer-remark "Writing...")
       (order-mark-last mark)
       (save-excursion
         (do ((comp-number 0 (1+ comp-number)))
	   ((write-point-mark-to-comp		;returns t when done
	      file-object comp-number mark)	;updates file-object
	    (setf (cdr (nthcdr comp-number	;forget about extra components
			   (fobj-contents file-object)))
		nil))))
       (minibuffer-remark ""))		;erase above remark

;;; Writes as much of point-mark as it can to the specified
;;; component of the file.  Leaves point after the portion that was
;;; written.  Returns t if it wrote everything, nil if it filled
;;; the component before finishing.  Updates the length in the filecons
;;; for the component to the amount written.  Updates the contents
;;; list if it must allocate a new component.
;;; For efficiency, these functions know about the various representations
;;; of lines and make use of low-level buffer-management variables (cur*);
;;; the old version did alot of string and mark consing in the name of
;;; modularity.
(defun write-point-mark-to-comp (file-object comp-number mark)
       (let* ((filecons (get-output-component file-object comp-number))
	    (comp-ptr (filecons-pointer filecons))
	    max-length)
	   ;; zero segment for improved paging
	   (let ((trunc-code (hcs_$truncate_seg comp-ptr 0)))
	        (unless (= trunc-code 0)
		      (report-file-error trunc-code file-object)))
	   (let (((maxl maxl-code)
		(hcs_$get_max_length_seg comp-ptr)))
	        (cond ((= maxl-code 0) (setq max-length (* 4 maxl)))     ;word->chars
		    (t (report-file-error maxl-code file-object))))
	   (do ((next-offset 0)
	        (done-flag)
	        (file-left max-length (- max-length next-offset)))
	       (nil)
	       (cond ((stringp curstuff)
		    (desetq (next-offset done-flag)
			  (write-out-string-line
			    comp-ptr next-offset file-left mark)))
		   (t			;curstuff is in temp-seg
		     (desetq (next-offset done-flag)
			   (write-out-filecons-lines
			     comp-ptr next-offset file-left mark))))
	       (when done-flag
		   (setf (filecons-length filecons) next-offset)
		   (and (eq done-flag 'buffer)
		        add-newline (not (bolp)) ;need to add newline
		        (cond ((< next-offset max-length)
			     (setf (filecons-length filecons)
				 (e_lap_$write-string
				   NLCHARSTRING comp-ptr
				   next-offset))
			     (return t))  ;all done
			    (t (return nil))))   ;put nl in next component
		   (return (eq done-flag 'buffer))))))


;;; Write out the appropriate portion of the current line, which is
;;; stored as a string in the buffer, to the segment|offset specified.
;;; Don't write more than file-left chars, and stop at end-mark in the buffer
;;; Returns a list (new-offset done-flag), where done-flag is nil
;;; if the caller should continue writing to the segment, 'segment if
;;; the segment is full, and 'buffer if end-mark was reached.
(defun write-out-string-line (segment offset file-left end-mark
			        &aux
			        char-count (done-flag nil))
       (cond ((mark-on-current-line-p end-mark)
	    (setq done-flag 'buffer
		char-count (- (mark-position end-mark) curpointpos)))
	   (t (setq char-count (- curlinel curpointpos))))
       (when (> char-count file-left)
	   (setq char-count file-left
	         done-flag 'segment))
       (cond ((= 0 char-count))		;nothing left
	   ((and (= curpointpos 0)
	         (= char-count curlinel))	;writing whole line
	    (setq offset (e_lap_$write-string curstuff segment offset))
	    (next-line))
	   (t (setq offset
		  (e_lap_$write-string
		    (substr curstuff (1+ curpointpos) char-count)
		    segment offset))
	      (forward-n-chars char-count)))
       (list offset done-flag))

;;; The current line contents is a temp-seg filecons.  Write out this line
;;; and any following lines that are adjacent in the temp-seg in one
;;; shot (for efficiency).  Otherwise, it is like write-out-string-line.
(defun write-out-filecons-lines (segment offset file-left end-mark
				 &aux
				 beginning (done-flag nil)
				 (char-count 0)
				 (old-cpp curpointpos))
       (setq beginning (add-char-offset (filecons-pointer curstuff)
				curpointpos))
       (cond ((not (= curpointpos old-cpp))
	    (break 'bad-curpointpos)))
       (do ((line-char-count))
	 (())
	 (cond ((mark-on-current-line-p end-mark)
	        (setq line-char-count (- (mark-position end-mark)
				   curpointpos)
		    done-flag 'buffer)
	        (go-to-mark end-mark))
	       (t (setq line-char-count (- curlinel curpointpos))
		(next-line)))
	 (when (> line-char-count file-left)
	       (backward-n-chars (- line-char-count file-left))
	       (setq line-char-count file-left
		   done-flag 'segment))
	 (incf char-count line-char-count)
	 (decf file-left line-char-count)
	 ;; check if we can continue with the next line
	 (when (or done-flag		;nothing left
		 (stringp curstuff)		;not a filecons
		 (not (= (filecons-pointer curstuff)	;not adjacent in
		         (add-char-offset beginning char-count))))     ; temp-seg
	       (return nil)))
       (e_lap_$write-string (make-filecons pointer beginning
				   length char-count)
		        segment offset)
       (list (+ offset char-count) done-flag))

;;; This is the Emacs analogue to msf_manager_$get_ptr.  If it has to
;;; create the component then it will update the file-object's contents
;;; list.  It returns the filecons representing the requested component.
(defun get-output-component (file-object comp-number)
       (or (nth comp-number (fobj-contents file-object))	;existing component
	 (let* (((ptr len code)
	         (msf_manager_$get_ptr (fobj-fcb-ptr file-object)
				 comp-number -1))	;create-if-not-found
	        (filecons (make-filecons pointer ptr
				   length len))
	        (contents-count (length (fobj-contents file-object))))
	      (when (= ptr null-pointer)
		  (report-file-error code file-object))
	      (cond ((= comp-number contents-count)  ;appending next component
		   (setf (fobj-contents file-object)
		         (nconc (fobj-contents file-object)
			      (ncons filecons))))
		  ((< comp-number contents-count)  ;inserting in the middle
		   (setf (nth comp-number (fobj-contents file-object))
		         filecons))
		  (t (setf (fobj-contents file-object)
			 (nconc (fobj-contents file-object)
			        (make-list (- comp-number contents-count))
			        (ncons filecons)))))
	      filecons)))

;;; 

;;;
;;;	User-callable interfaces
;;;


;;; Returns closed file-object if named file exists with specified access
;;; Returns nil if file doesn't exist or wrong access.
;;;  (Access-needed is as for open-file.)
(defun exists-file (name &optional (access-needed 'read)
		     &aux (file-object nil))
       (when (fixp access-needed)		;backward compatibility
	   (setq access-needed
	         (cond ((zerop (logand access-needed W_ACCESS_BIN))
		      'read)
		     (t 'write))))
       (protect
         (setq file-object (open-file name access-needed nil))
         &always
         (close-file file-object))
       file-object)


;;; Returns file-object if named file exists with specified access
;;; Aborts with message if file doesn't exist of wrong access
(defun validate-file (name access-needed &aux (file-object nil))
       (when (fixp access-needed)		;backward compatibility
	   (setq access-needed
	         (cond ((zerop (logand access-needed W_ACCESS_BIN))
		      'read)
		     (t 'write))))
       (protect
         (setq file-object (open-file name access-needed t))
         &always
         (close-file file-object))
       file-object)


;;; Inserts the named file into the buffer at the cursor
(defun file-insert (file-name &aux (file-object nil))
       (check-read-only)
       (protect
         (setq file-object
	     (check-object (open-star-name-single file-name 'read)))
         (read-file-into-buffer file-object)
         &always
         (close-file file-object nil)))
	  

;;; Reads the file into the buffer, destroying previous contents.
;;; file-arg may be either a pathname string or an open file-object.
(defun read-in-file (file-arg &aux file-object)
       (check-read-only)
       (protect
         (setq file-object
	     (check-object
	       (cond ((stringp file-arg)
		    (open-star-name-single file-arg 'read))
		   (t file-arg))))
         (let ((abs-path			;save abolute pathname and UID
	       (absolute-pathname (fobj-path file-object)))
	     (uid (fobj-uid file-object))
	     (dtcm (fobj-dtcm file-object)))
	    (reinitialize-current-buffer)
	    (setq buffer-modified-flag t)	;keep quiet
	    (read-file-into-buffer file-object)
	    (setq buffer-modified-flag nil
		fpathname abs-path
		buffer-file-dtcm dtcm	;let find-file know
		buffer-uid uid)
	  (go-to-beginning-of-buffer))
         &always
         (close-file file-object nil)))

;;; Writes the current region into the named file
(defun write-out-region (file-name &aux (file-object nil))
       (unless der-wahrer-mark
	     (report-error 'mark-not-set))
       (save-excursion
         (with-mark
	 here
	 (go-to-mark der-wahrer-mark)
	 (protect
	   (setq file-object (open-file file-name 'write t))
	   (write-point-mark-to-file file-object here)
	   &success
	   (close-file file-object t)		;update if successful
	   &failure
	   (close-file file-object nil)))))

;;; Writes the entire buffer to the specified file.  A subr for the
;;; following two user-callable subrs.  Access is either write or write-force.
(defun write-buffer-to-file (file-name access &aux (file-object nil))
       (protect
         (setq file-object (open-file file-name access t))
         (save-excursion
	 (go-to-end-of-buffer)
	 (with-mark
	   end
	   (go-to-beginning-of-buffer)	;whole buffer marked
	   (write-point-mark-to-file file-object end)	;wham!
	   (close-file file-object t)))	;update only if successful
         (setq fpathname (absolute-pathname (fobj-path file-object))
	     buffer-uid (fobj-uid file-object)
	     buffer-file-dtcm (fobj-dtcm file-object)
	     buffer-modified-flag nil)	;now corresponds to file
         &failure (close-file file-object nil)))

;;; Writes the current buffer into the named file.
(defun write-out-file (file-name)
         (write-buffer-to-file file-name 'write))	;do the work

;;; Write the current buffer into the named file, forcing access if necessary
(defun write-protected-file (file-name)
       (write-buffer-to-file file-name 'write-force))

;;; 

;;;
;;;	Commands
;;;


;;; Checks for attempt to read/write in minibuffer
(defun check-minibuffer-file-command ()
       (and minibufferp
	  (display-error "No reading/writing, you are in the minibuffer.")))


;;; Should ^X^R be careful about overwriting a modified buffer?
;;; (register-option 'read-file-force nil) ;moved to e_option_defaults_


;;; Reads a file into the buffer
(defcom read-file
        &numarg (&pass)
        &prologue check-minibuffer-file-command	;abort if in minibuffer
        &args ((file-name &string &prompt "Read File: "))
        (cond ((and buffer-modified-flag	;check if overwriting modified bugffer
		(not read-file-force)	;option forces it to read
		(not numarg))		;numarg forces it to read
	     (if (not (yesp "The current buffer has not been written out; read anyway? "))
	         (command-quit))))
        (setq file-name (e_lap_$trim file-name))
        (let ((file-to-read
	      (cond ((not (nullstringp file-name)) file-name)
		  (fpathname fpathname)
		  (t (report-error "You must supply a pathname.")))))
	   (read-in-file file-to-read)))


;;; Inserts a file into the buffer at the cursor
(defcom insert-file
        &prologue check-minibuffer-file-command	;abort if in minibuffer
        &args ((file-name &string &prompt "Insert File: "))
        (setq file-name (e_lap_$trim file-name))
        (cond ((not (nullstringp file-name))
	     (with-mark before-mark
		      (file-insert file-name)
		      (set-the-mark-here before-mark)))
	    (t
	      (display-error "You must supply a pathname."))))


;;; DTCM greater-than (handles unsignedness)
(defmacro dtcm-> (time1 time2)
	(once-only
	  (time1 time2)
	  `(and (not (= ,time1 ,time2))	;check common case first
	        (or (> (lsh ,time1 -1) (lsh ,time2 -1))	;ignore low-order bit
		  (> ,time1 ,time2)))))

(defmacro dtcm-< (time1 time2)
	`(not (dtcm-> ,time1 ,time2)))

;;; Writes the buffer into the default file
(defcom save-same-file
        &numarg &pass
        (check-minibuffer-file-command)		;abort if in minibuffer
        (check-newline-ending)
        (cond (fpathname
	      (let ((output-path (expand-pathname fpathname)))
		 (and save-same-file-check-dtcm
		      (not numarg)
		      buffer-file-dtcm
		      (let ((file-obj (exists-file output-path)))
			 (and file-obj
			      (dtcm-> (fobj-dtcm file-obj)
				    buffer-file-dtcm)))
		      (not
		        (yesp
			(catenate fpathname
				" has changed since last read or written. Save anyway?")))
		      (command-quit))
		 (write-protected-file output-path)))
	    (t (display-error "No default pathname for this buffer."))))

;;; Writes the buffer into a file
(defcom write-file
        &numarg &pass
        &prologue check-minibuffer-file-command	;abort if in minibuffer
        &args ((file-name &string &prompt "Write File: "))
        (check-newline-ending)
        (setq file-name (e_lap_$trim file-name))
        (let (file-obj)
	   (cond ((nullstringp file-name) (save-same-file))    ;no name given, use default
	         ((and (not write-file-overwrite)
		     (not numarg)
		     (setq file-obj (exists-file file-name))
		     (not
		       (yesp
		         (catenate (absolute-pathname
				 (fobj-path file-obj))
			         " already exists. Overwrite it?"))))
		(command-quit))
	         (t (write-protected-file file-name)))))


;;; Insert current buffer's pathname
(defcom get-filename
        &na (&pass)       
        (cond (fpathname
	      (set-the-mark)
	      (insert-string
	        (cond (numarg (get-entryname fpathname))
		    (t fpathname))))
	    (t (display-error "No default pathname for this buffer."))))

(defun get-entryname (pathname)
       (setq pathname (expand-pathname pathname))	;canonicalize
       (let ((ename (pn-entry pathname))
	   (cname (pn-component pathname)))
	  (cond ((nullstringp cname) ename)
	        (t cname))))


;;; option to specify if find-file should set buffer modes from pathname suffix
;;; (register-option 'find-file-set-modes nil) ;moved to e_option_defaults_


;;;
;;;	find-file Command
;;;	Reads file into buffer given by its name
;;;	unless it's already in a buffer
;;;

(defcom find-file
        &prologue check-minibuffer-file-command	;abort if in minibuffer
        &args ((file-name &string &prompt "Find File: "))
        (setq file-name (e_lap_$trim file-name))
        (when (nullstringp file-name)
	    (display-error "You must supply a pathname."))
        (find-file-subr file-name)
        (select-buffer-window current-buffer 'cursize-not-empty))

(defun find-file-subr (file-name)
       (dolist (one-file-name (match-star-name file-name nil))
	     (*catch 'find-file-skip-file
		   (find-file-subr-single-entry one-file-name))))

(defun find-file-subr-single-entry (pathname &aux
				     (file nil) uid dtcm
				     abs-path default-buffer
				     buffer-given)
       (protect
         (setq file (open-file pathname 'read nil)     ;don't complain, we will create new buffer
	     uid (and file (fobj-uid file))
	     dtcm (cond (file (fobj-dtcm file))
		      (t 0))		;nonexistent file is VERY old
	     abs-path (absolute-pathname pathname)
	     default-buffer (find-file-default-buffer pathname)
	     buffer-given (find-file-choose-buffer uid dtcm default-buffer
					   pathname))
         ;; If we're going to read in a new file, make sure it isn't object seg
         (and file
	    (or (not (exists-buffer buffer-given))
	        (empty-buffer-p buffer-given))
	    (check-object file))
         ;; Now have the buffer to use in buffer-given.
         (go-to-or-create-buffer buffer-given)
         (cond ((empty-buffer-p buffer-given)
	      ;;empty buffer, read file if found
	      (cond (file (read-in-file file))
		  (t (open-file pathname 'read 'noabort)    ;to get error message.
		     (setq fpathname abs-path)))
	      (find-file-set-buffer-mode pathname))
	     ;; using existing buffer
	     (t ;;see if fpathname is invalid.
	        (let ((old-file (exists-file fpathname 'read)))
		   (and file		;old name unused now
		        (equal (fobj-uid file) (fobj-uid old-file))    ;name has moved!
		        (setq fpathname abs-path)))))	;Use the new pathname
         &failure
         (close-file file)))

(defun find-file-choose-buffer (uid dtcm default-buffer pathname)
       (let ((buffer-given nil)
	   (buffer-list nil))
	  (when uid
	        (setq buffer-list
		    (find-file-find-buffers-containing-file uid dtcm))
	        (cond ((null buffer-list))
		    ((null (cdr buffer-list)) ;only one
		     (setq buffer-given (first buffer-list)) ;use it
		     (minibuffer-remark "Buffer " buffer-given
				    " contains "
				    (absolute-pathname pathname)))
		    (t (setq buffer-given
			   (find-file-disambiguate-buffer default-buffer
						    buffer-list)))))
	  (when (null buffer-given)
	        (setq buffer-given default-buffer))
	  ;; decide if this is a reasonable buffer
	  (do ((potential-buffer buffer-given))
	      ((or (not (exists-buffer potential-buffer)) ;new buffer OK
		 (empty-buffer-p potential-buffer))	;reuse empty OK
	       potential-buffer)
	      (cond ((memq potential-buffer buffer-list)	;already in this buffer: MAYBE
		   (let ((result		;check for old version
			 (find-file-check-modified potential-buffer
					       dtcm pathname))
		         keyword)
		        (cond ((atom result)	;chose a new buffer
			     (setq potential-buffer result))
			    ((eq (setq keyword (car result))
			         'overwrite)     ;said to overwrite this one
			     (buffer-kill potential-buffer)
			     (return potential-buffer))
			    ((eq keyword 'use) (return potential-buffer))
			    ((eq (car result) 'skip)
			     (*throw 'find-file-skip-file nil)) ;said to skip this one
			    (t (error "Invalid result from find-file-check-modified." result 'fail-act)))))
		  ((and (eq potential-buffer default-buffer) ;default buffer didn't pass above
		        (let ((newbuf (find-file-alternate-buffer pathname)))
			   (and (neq newbuf potential-buffer)	;don't loop
			        (progn (setq potential-buffer newbuf)
				     t)))))    ;to skip next cond-clause
		  (t (setq potential-buffer
			 (find-file-get-new-buffer potential-buffer)))))))
;;;
;;;
;;; Friends of find-file
;;;

;;; Return buffer symbol given file name

(defun find-file-default-buffer (file-name)
       (setq file-name (get-entryname file-name))
       (make_atom
         (cond (find-file-entry-names-buffer file-name)
	     (t (first-entryname-component file-name)))))

(defun first-entryname-component (entryname)
       (let ((dot-index (index entryname ".")))
	  (cond ((< dot-index 2)		;no dot or starts with dot
	         entryname)			;so leave alone
	        (t (substr entryname 1 (1- dot-index))))))

;;; Return alternate buffer name (i.e. whole entry name) given file name

(defun find-file-alternate-buffer (file-name)
       (make_atom (get-entryname file-name)))

;;; Find all emacs buffers containing a specified file

(defun find-file-find-buffers-containing-file (uid dtcm)
       (let* ((lists (list nil nil))		;(uid-matches dtcm-matches)
	    (environment `(,uid ,dtcm .,lists)))
	   (map-over-emacs-buffers #'find-file-collect-buffer environment)
	   (or (second lists)		;if any DTCM match, then only return those
	       (first lists))))		;otherwise, return any matching files

(defun find-file-collect-buffer (buffer (uid dtcm . lists)) ;
       (when (and (not (empty-buffer-p buffer))
	        (equal (get-buffer-state buffer 'buffer-uid)
		     uid))
	   (push buffer (first lists))
	   (when (eq dtcm
		   (get-buffer-state buffer 'buffer-file-dtcm))
	         (push buffer (second lists)))))

;;; Display all the buffers containing a file, and make the user pick one

(defun find-file-disambiguate-buffer (default-buffer buffer-list)
       (init-local-displays)
       (local-display-generator-nnl "Buffers containing this file:")
       (local-display-generator-nnl "")
       (mapc 'find-file-display-one-buffer buffer-list)
       (local-display-generator-nnl "-------------------------")
       (let ((completion-list buffer-list)
	   (answer (intern-minibuffer-response "Buffer: " NL)))
	  (cond ((eq answer '||)		;null response
	         default-buffer)
	        (t answer))))

;;; Display a single buffer (subroutine of find-file-disambiguate-buffer)

(defun find-file-display-one-buffer (buffer)
       (let ((current-buffer-prefix " ")
	   (modified-prefix " ")
	   (pad (substr "                         "
		      1
		      (max (- 25. (stringlength buffer)) 1)))
	   (path (get-buffer-state buffer 'fpathname)))
	  (and (eq current-buffer buffer)
	       (setq current-buffer-prefix ">"))
	  (and (get-buffer-state buffer 'buffer-modified-flag)
	       (setq modified-prefix "*"))
	  (cond (path
		(local-display-generator-nnl
		  (catenate current-buffer-prefix
			  modified-prefix
			  buffer
			  pad
			  path)))
	        (t (local-display-generator-nnl
		   (catenate current-buffer-prefix
			   modified-prefix
			   buffer))))))

;;; Tell the user the buffer he wants is in use, and get a new one from him

(defun find-file-get-new-buffer (old-buffer)
       (let ((answer))
	  (ring-tty-bell)
	  (setq answer (intern-minibuffer-response
		       (catenate "Buffer " old-buffer
			       " is already in use.  New buffer: ")
		       NL))
	  (cond ((nullstringp answer)
	         old-buffer)
	        (t answer))))

;;; Set the major mode from the file name

(defun find-file-set-buffer-mode (file-name)
       (when find-file-set-modes
	   (let* ((entry (get-entryname file-name))
		(entry-len (stringlength entry))
		(dot-index (1- (e_lap_$reverse-search-string
			       entry entry-len ".")))
		(suffix (cond ((< dot-index 0)     ;no suffix, or null suffix
			     '(nil))	;not nil, in case of foo.nil
			    (t (substr entry
				     (- entry-len dot-index))))))
	         (when (atom suffix)
		     (setq suffix (make_atom suffix))
		     (let ((mode-fun
			   (or (get suffix 'suffix-mode)
			       (make_atom (catenate suffix "-mode")))))
			(when (getl mode-fun '(expr subr autoload))
			      (funcall mode-fun)))))))

;;; Check whether the file has been modified since the buffer was read/written
;;; and query the user if so.  Returns either a buffer (a symbol), the list
;;; (overwrite) if the caller should overwrite the specified buffer, (skip)
;;; if the caller should skip this file altogether, or (use) if the caller
;;; should just go to the specified buffer.
(defun find-file-check-modified (buffer file-dtcm pathname)
       (cond
         ((not find-file-check-dtcm) '(use))     ;user opts not to check
         ((dtcm-< (or file-dtcm 0)		;file not more recent than buffer
	        (or (get-buffer-state buffer 'buffer-file-dtcm)
		  0))
	'(use))
         (t (ring-tty-bell)
	  (let (((display help)
	         (find-file-check-mod-get-display-lists buffer pathname)))
	       (init-local-displays)
	       (mapc 'local-display-generator-nnl display)
	       (end-local-displays)
	       (let*
	         ((completion-list
		  '(Overwrite overwrite Use use New new Skip skip
			    Help help ?))
		(answer
		  (do ((ans (find-file-check-mod-query) (find-file-check-mod-query)))
		      ((memq ans completion-list)
		       ans)
		      (ring-tty-bell)
		      (init-local-displays)
		      (local-display-generator-nnl "Invalid response.  Type ""help"" or ""?"" for assistance.")
		      (end-local-displays))))
	         (selectq
		 answer
		 (overwrite '(overwrite))
		 (skip '(skip))
		 (use '(use))
		 (new
		   (let ((completion-list nil))
		        (do ((ans (intern-minibuffer-response "New buffer: ")
			        (intern-minibuffer-response "Invalid response.  New buffer: ")))
			  ((neq ans '||) ans)
			  (ring-tty-bell))))
		 ((help ?)
		  (find-file-check-mod-help help)
		  ;; recurse to try again (too bad no tail-recursion optimization)
		  (find-file-check-modified buffer file-dtcm pathname))))))))

(defun find-file-check-mod-get-display-lists (buffer pathname)
       (setq pathname (absolute-pathname pathname))
       (cond
         ((get-buffer-state buffer 'buffer-modified-flag)
	`((,(catenate "Modified buffer " buffer
		    " contains an old version of")
	    ,pathname)
	  (,(catenate "Since buffer " buffer
		    " was last saved or read, the file")
	    ,(catenate pathname " has been modified.")
	    "The buffer HAS ALSO been modified since then.")))
         (t `((,(catenate "Buffer " buffer " contains an old version of")
	      ,pathname)
	    (,(catenate "Since buffer " buffer
		      " was last saved or read, the file")
	      ,(catenate pathname " has been modified.")
	      "The buffer HAS NOT been modified since then.")))))

(defun find-file-check-mod-query ()
       (intern
         (lowercase
	 (e_lap_$rtrim
	   (minibuffer-response
	     "Select ""overwrite"", ""use"", ""skip"", ""new"" buffer, or ""help"": ")))))

(defun find-file-check-mod-help (display-list)
       (init-local-displays)
       (mapc 'local-display-generator-nnl
	   display-list)
       (mapc 'local-display-generator-nnl
	   '("" "Respond with one of:"
	        "  overwrite - to reread the file into this buffer"
	        "  use	- to use this buffer as is"
	        "  new	- to select a new buffer"
	        "  skip	- to skip the current file"))
       (end-local-displays))
;;; 

;;;
;;;	Miscellaneous functions
;;;

;;; expand-pathname takes either a pathname structure or a relative pathname
;;; character string, and returns a pathname structure.  If dont-abort is
;;; non-nil, then it returns a list of the pathname and an error code.
;;; This is the definition of "pathname" as
;;; input to all other functions in this module: a pathname is anything
;;; acceptable to expand-pathname.  They should all call expand-pathname
;;; doing anything, so that they will be generic.

(defun expand-pathname (pathname &optional dont-abort)
       (cond ((or (stringp pathname) (symbolp pathname))
	    (let (((dir entry comp code)
		 (expand_pathname_$component pathname)))
	         (cond ((= 0 code)
		      (let ((pathname
			    (make-pathname directory (e_lap_$rtrim dir)
				         entry (e_lap_$rtrim entry)
				         component (e_lap_$rtrim comp))))
			 (cond (dont-abort (list pathname 0))
			       (t pathname))))
		     (dont-abort (list (make-pathname) code))
		     (t (report-error code " " pathname)))))
	   ((atom pathname)			;dont-abort doesn't stop wrong-type-arg checking.
	    (error "The pathname is not a list or string." pathname 'wrng-type-arg))
	   (dont-abort (list pathname 0))
	   (t pathname)))

;;; absolute-pathname takes a pathname and returns a list of the character
;;; string representation of the absolute path and a standard Multics error
;;; code.  dont-abort is as in expand-pathname.

(defun absolute-pathname (orig-pathname &optional dont-abort)
       (let ((pathname (expand-pathname orig-pathname dont-abort))
	   (code 0))
	  (when dont-abort
	        (desetq (pathname code) pathname))
	  (cond ((= 0 code)
	         (let ((string (pn-abs-path pathname)))	;check cache
		    (when (null string)
			(setq string
			      (e_lap_$rtrim (pathname_$component
					  (pn-directory pathname)
					  (pn-entry pathname)
					  (pn-component pathname))))
			(setf (pn-abs-path pathname) string))
		    (cond (dont-abort (list string 0))
			(t string))))
	        (dont-abort (list "" code))
	        (t (report-error code " " orig-pathname)))))


;;; An interface to report-error for file errors.
(defun report-file-error (code file-obj)
       (report-error code SPACE
		 (car (absolute-pathname (fobj-path file-obj)
				     t))))

;;; Converts file-cons to a string
(defun filerep-to-string (file-cons)
       (e_lap_$return-string (filecons-pointer file-cons)
		         0 (filecons-length file-cons)))


;;; Loads a file of LISP using "working" directory
(defun loadfile (file-name)
       (let ((file-exists (validate-file file-name 'read)))	;need at least r, but don't need e
	  (and file-exists
	       (errset (load (fobj-abs-path file-exists))))))


;;; Loads a file from EMACS library
(defun loadlib (file-name)
       (loadfile (make-pathname directory env-dir entry file-name)))


;;; Specify that given function is autoloaded from EMACS library
(defun set-autoload-lib (function file-name)
       (putprop function (pathname_ env-dir file-name) 'autoload))

;;; 
;;; Interfaces to the starname matcher in e_multics_files_util_.

;;; This matches the starname, and opens it if it only matches one entry.
(defun open-star-name-single (starname mode)
       (let ((matches (match-star-name starname nil)))
	  (cond ((null (cdr matches))
	         (open-file (first matches) mode))
	        (t (report-error "Multiple-files not allowed here: " starname)))))

;;; This returns a list of pathnames matching the starname.  It aborts
;;; with error_table_$nomatch if there are no matches at all, so it will
;;; never return nil, unless allow-no-match is non-nil.
(defun match-star-name (starname allow-no-match)
       (let* ((path (expand-pathname starname))
	    (dir (pn-directory path))
	    (ent (pn-entry path))
	    (comp (pn-component path))
	    (data-ptr null-pointer)
	    code count)
         (protect
	 (desetq (data-ptr count code)
	         (e_multics_files_util_$star_list_init
		 dir ent comp))
	 (or (= code 0)
	     allow-no-match
	     (report-error code SPACE (absolute-pathname path)))
	 (loop for index from 1 to count
	       collecting
	       (let (((new-ent new-comp)
		    (e_multics_files_util_$nth_star_match
		      data-ptr index)))
		  (make-pathname directory (e_lap_$rtrim dir)
			       entry (e_lap_$rtrim new-ent)
			       component (e_lap_$rtrim new-comp))))
	 &always
	 (e_multics_files_util_$star_list_cleanup data-ptr))))
