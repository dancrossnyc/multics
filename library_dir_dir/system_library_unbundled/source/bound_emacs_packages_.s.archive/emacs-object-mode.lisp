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
;;; Emacs Object Mode, because it was necessary.


;;; HISTORY COMMENTS:
;;;  1) change(81-05-05,Soley), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;     Originally written.
;;;  2) change(86-02-24,Margolin), approve(86-02-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Converted to use the new e_multics_files_ primitives.  Changed the #
;;;     read-macro to the more standard #o macro.  Changed lsh/boole
;;;     combinations into ldb.
;;;                                                      END HISTORY COMMENTS

;;;

(%include e-macros)
(%include emacs-internal-macros)
(%include defun)
(%include other_other)
(%include sharpsign)
(%include runtime)
(eval-when (eval compile load) (sstatus feature runtime))	; write-around bug

(declare (special object-mode-count object-mode-chars last-input-char
	        known-buflist NL object-mode-total)
         (*lexpr absolute-pathname close-file open-file)
         (*expr check-minibuffer-file-command
	      open-star-name-single))

;;; Returns octal string representation, padded on the left with 0.

(defun pad-left-octal (number)
       (let ((rep (octal-rep number)))
	  (cond ((= (stringlength rep) 3) rep)
	        ((= (stringlength rep) 2) (catenate "0" rep))
	        ('else (catenate "00" rep)))))

(defun pad-left-6octal (number)
       (let ((rep (octal-rep number)))
	  (catenate (substr "000000" 1 (- 6. (stringlength rep)))
		  rep)))

;;; Returns octal string representation.

(defun octal-rep (x)
       (let ((base 8.) (*nopoint t))
	  (maknam (explodec x))))

;;; This is the end-of-file-function for object-mode-read-file.

(defun object-mode-eoffn (a b) a b (throw 'done object-mode-tag))

;;; This functions reads in a file, inserting the object in object-mode
;;; format into the current buffer.  Doesn't check pathname at all.

(defun object-mode-read-file (pathname)
       (let ((read-only-flag nil))
	  (destroy-buffer-contents)
	  (setq object-mode-count 0 object-mode-chars ()
	        fpathname (e_lap_$rtrim pathname) object-mode-total 0)
	  (let ((fo (open pathname '(in ascii fixnum))))
	       (eoffn fo 'object-mode-eoffn)
	       (minibuffer-remark "Reading...")
	       (catch
	         (do () (()) (object-mode-read-word fo))
	         object-mode-tag)
	       (close fo))
	  (or (zerop object-mode-count) (object-mode-finish-line))
	  (go-to-beginning-of-buffer)
	  (setq buffer-modified-flag nil)))

;;; Function to read in one word from the input file and update
;;; the current line with that information.

(defun object-mode-read-word (file)
       (let ((word (in file)))
	  (setq object-mode-count (1+ object-mode-count))
	  (let ((c1 (ldb #o3311 word))
	        (c2 (ldb #o2211 word))
	        (c3 (ldb #o1111 word))
	        (c4 (ldb #o0011 word)))
	       (setq object-mode-chars
		   (list* c4 c3 c2 c1 object-mode-chars))
	       (insert-string
	         (catenate (pad-left-octal c1) " "
		         (pad-left-octal c2) " "
		         (pad-left-octal c3) " "
		         (pad-left-octal c4) "   "))))
       (and (= object-mode-count 3) (object-mode-finish-line)))

;;; Finish off a line of object.

(defun object-mode-finish-line ()
       (save-excursion
         (go-to-beginning-of-line)
         (insert-string (pad-left-6octal object-mode-total))
         (insert-string "   "))
       (setq object-mode-total (+ object-mode-total 3))
       (or (= object-mode-count 3)
	 (do n object-mode-count (1+ n) (> n 2)
	     (insert-string "                  ")))
       (setq object-mode-count 0)
       (do ((n 0 (1+ n))
	  (chars (nreverse object-mode-chars) (cdr chars)))
	 ((or (null chars) (= n 12.)))
	 (and (zerop (\ n 4)) (insert-string " "))
	 (let ((this (car chars)))
	      (object-mode-insert-letter this)))
       (setq object-mode-chars ())
       (new-line))

;;; To write an object file.  Real work done below: this just insures
;;; access, etc.

(defun object-mode-write-file (file-name &aux (file-object nil))
       (protect
         (setq file-object (open-file file-name 'write-force))
         (object-mode-write (absolute-pathname (fobj-path file-object)))
         &always
         ;;restore access, bit-count set by "close" below
         ;; this kludge necessary because "close" may also
         ;; terminate the segment
         (let ((new-fo (open-file file-name 'write nil)))
	    (when new-fo
		(setf (fobj-original-access new-fo)	;copy access restoration info
		      (fobj-original-access file-object))    ;to useful file object
		(close-file new-fo nil)	;restore access
		(setf (fobj-original-access file-object) nil)))
         (close-file file-object nil)))		;clean up FCB

;;; Function to output an object-code buffer.  Besides the side
;;; effect of writing out the buffer, returns 4 times the amount
;;; of words in the file.

(defun object-mode-write (file)
       (save-excursion
         (let ((fo (open file '(out ascii fixnum))))
	    (go-to-beginning-of-buffer)
	    (setq object-mode-count 0)
	    (minibuffer-remark "Writing...")
	    (let ((total-words 1))
	         (catch
		 (do () (())
		     (object-mode-write-word fo)
		     (setq total-words (1+ total-words)))
		 object-mode-tag)
	         (setq buffer-modified-flag nil 
		     fpathname (e_lap_$rtrim file))
	         (minibuffer-remark "Written.")
	         (close fo)
	         (* 4. total-words)))))

;;; Reads the octal word at the point in the buffer, and moves
;;; forward to the next group.

(defun object-mode-get-octal ()
       (with-mark beginning-of-word
	        (forward-word)
	        (prog1 (readlist
		       (exploden
		         (point-mark-to-string beginning-of-word)))
		     (forward-char))))

;;; Function to output a single word of a file, given that we
;;; are at the beginning of the representation of that word
;;; in the buffer.

(defun object-mode-write-word (file)
       (and (zerop (cur-hpos)) (go-to-hpos 9.))
       (out file
	  (+ (lsh (object-mode-get-octal) 27.)
	     (lsh (object-mode-get-octal) 18.)
	     (lsh (object-mode-get-octal) 9.)
	     (object-mode-get-octal)))
       (forward-char) (forward-char)
       (setq object-mode-count (1+ object-mode-count))
       (cond ((= 3 object-mode-count)
	    (setq object-mode-count 0)
	    (next-line)))
       (cond ((or (line-is-blank) (eolp) (looking-at " "))
	    (throw 'done object-mode-tag))))

;;; Insert letter in letters column.

(defun object-mode-insert-letter (number)
       (cond ((and (> number #o37) (< number #o177))
	    (insert-char (ItoC number)))
	   ('else (insert-char "."))))

;;; Give an error message stipulating that we're in
;;; a non-editable column.

(defun object-mode-bad-column ()
       (display-error "Nothing to edit in this column."))

;;; Decides what type of column we are currently in:
;;; 'numbers => the numeric kind of column.
;;; 'letters => the alphabetic kind of column.
;;; If in a bad column type, gives an error message.

(defun object-mode-column-type ()
       (let ((h (cur-hpos)))
	  (cond ((< h 9.) (object-mode-bad-column))
	        ((or (> h 77.) (eolp)) (object-mode-bad-column))
	        ((> h 63.)
	         (cond ((member h '(68. 73.)) (object-mode-bad-column))
		     ('else 'letters)))
	        ((eq (curchar) '/ ) (object-mode-bad-column))
	        ('else 'numbers))))

;;; Given that we are IN a numeric column, this updates this
;;; group of numbers AND the associated ascii to the right.  We
;;; assume that the new number has been inserted already.

(defun object-mode-update-number ()
       (save-excursion
         (forward-char)
         (backward-word)
         (let ((h (- (cur-hpos) 9.))
	     (number (with-mark beginning-of-word
			    (forward-word)
			    (readlist
			      (exploden
			        (point-mark-to-string
				beginning-of-word))))))
	    (go-to-hpos (+ 64.
		         (* (// h 18.) 5.)
		         (// (\ h 18.) 4.)))
	    (delete-char)
	    (object-mode-insert-letter number))))

;;; Given that we are IN a letter column, this updates this
;;; letter AND the associated numbers to the right.  We
;;; assume that the new letter has been inserted already.

(defun object-mode-update-letter ()
       (save-excursion
         (let ((h (- (cur-hpos) 64.))
	     (number (CtoI (curchar))))
	    (delete-char)
	    (object-mode-insert-letter number)
	    (go-to-hpos (+ 9. (* 18. (// h 5.)) (* 4. (\ h 5))))
	    (delete-word)
	    (insert-string (pad-left-octal number)))))

;;; Replacement for self-insert.

(defun object-mode-self-insert ()
       (cond ((eq (object-mode-column-type) 'letters)
	    (let ((read-only-flag nil))
	         (delete-char)
	         (insert-char last-input-char)
	         (backward-char)
	         (object-mode-update-letter)))
	   ((member last-input-char '(/0 /1 /2 /3 /4 /5 /6 /7))
	    (let ((read-only-flag nil))
	         (delete-char)
	         (insert-char last-input-char)
	         (backward-char)
	         (object-mode-update-number)))
	   ('else
	     (display-error
	       "You may only enter an octal number in this column.")))
       (object-mode-forward-char))

;;; Replacement for quote-char.

(defun object-mode-quote-char ()
       (let ((last-input-char (make_atom (ItoC (get-char)))))
	  (object-mode-self-insert)))

;;; Save-same-file for object mode.

(defcom object-mode-save-same-file
        (check-minibuffer-file-command)
        (or fpathname
	  (display-error "No default pathname for this buffer."))
        (object-mode-write-file fpathname))

;;; write-file for object mode.

(defcom object-mode-write-buffer
        &args ((file &prompt "Write Object File: "
		 &default &eval
		 (or fpathname
		     (display-error
		       "No default pathname for this buffer."))))
        (check-minibuffer-file-command)
        (or file
	  (display-error "No default pathname for this buffer."))
        (object-mode-write-file file))

;;; Command to read a file in in object mode.

(defcom object-mode-find-file
        &args ((name &prompt "Find Object File: "
		 &default &eval
		 (display-error "You must supply a pathname.")))
        (let ((in (open-star-name-single name 'read))) ;Check existence/access
	   (close-file in nil)
	   (setq name (fobj-path in))
	   (unless (nullstringp (pn-component name))
		 (report-error 'error_table_$archive_pathname))
	   (go-to-or-create-buffer
	     (object-mode-pick-buffer (pn-entry name)))
	   (object-mode-read-file (absolute-pathname name))
	   (object-mode)))
	   
;;; Pick a good buffer to go to.

(defun object-mode-pick-buffer (buffer)
       (cond ((memq (make_atom buffer) known-buflist)
	    (ring-tty-bell)
	    (object-mode-pick-buffer
	      (minibuf-response
	        (catenate "Buffer " buffer
		        " is already in use.  New buffer: ")
	        NL)))
	   ('else (make_atom buffer))))

;;; Same as read-file for object-mode.  When using this,
;;; we can assume you are in an object-mode buffer.

(defcom object-mode-read-command
        &prologue &eval (or (eq current-buffer-mode 'Object)
		        (display-error "You must be in Object mode."))
        &args ((name &prompt "Read Object File: "
		 &default
		 &eval (or fpathname
			 (display-error
			   "No default pathname for this buffer."))))
        (let ((in (open-star-name-single name 'read)))
	   (close-file in nil)
	   (setq name (fobj-path in))
	   (unless (nullstringp (pn-component name))
		 (report-error 'error_table_$archive_pathname))
	   (object-mode-read-file (absolute-pathname name))))

;;; Go forward one character in an interesting way.

(defcom object-mode-forward-char
        (forward-char)
        (let ((h (cur-hpos)))
	   (cond ((< h 60.) (skip-over-whitespace-in-line))
	         ((= h 60.) (next-line) (go-to-hpos 9.))
	         ((member h '(68. 73.)) (forward-char))
	         ((eolp) (next-line) (go-to-hpos 64.)))))

;;; Instate object mode.

(defun object-mode ()
       (setq current-buffer-mode 'Object)
       (setq read-only-flag 't)
       (map-over-emacs-commands
	'(lambda (symbol function junk) junk
	         (and (eq function 'self-insert)
		    (set-key symbol 'object-mode-self-insert)))
	())
       (set-key "^M"    'object-mode-self-insert)
       (set-key "^Q"    'object-mode-quote-char)
       (set-key "^X-^S" 'object-mode-save-same-file)
       (set-key "^X-^W" 'object-mode-write-buffer)
       (set-key "^X-^R" 'object-mode-read-command))
