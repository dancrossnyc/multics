;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1985 *
;;; *                                                         *
;;; ***********************************************************

;;; Macros used internally in Emacs.

;;; HISTORY COMMENTS:
;;;  1) change(85-01-05,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Created.  Mark and eline definitions taken from e_basic_.lisp, and
;;;     turned into defstructs.
;;;  2) change(86-02-24,Margolin), approve(86-02-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Added file-object, pathname, and packed-pointer structures, and
;;;     associated macros.
;;;                                                      END HISTORY COMMENTS

(%include sharpsign)
(%include defstruct)
(%include defmacro)
(declare (macros t))

;;;	The "eline" (editor-line) datatype is constructed as such:
;;;
;;;	(line-contents . (previous-line . next-line))
;;;
;;;	previous-line and next-line are other editor lines; line-contents
;;;	is either a Lisp string or a "filecons", which is constructed
;;;	as follows:
;;;
;;;	(char-ptr . linel)
;;;
;;;	char-ptr is a fixnum-encoded pointer to the starting character
;;;	of a line in a temp-seg into which e_pl1_ copied a file at read-in
;;;	time.  linel is the length of that line, including the mandatory
;;;	newline at the end.
(defstruct (eline
	   (:type tree)
	   (:conc-name))
	 (contents nil)
	 (previous nil)
	 (next nil))

;;; Some special cases
(defmacro prevline ()
	'(eline-previous curline))
(defmacro nextline ()
	'(eline-next curline))
;;; No need for the old curelevator, it was only directly referenced in
;;; one place, and that was fixed.

;;; If this format ever changes, e_lap_.lap will have to be changed.
;;; It expects a cons.
(defstruct (filecons
	   (:type tree)
	   (:conc-name))
	 pointer
	 length)

;;; The format of a mark is (eline . position)
(defstruct (mark
	   (:type tree)
	   (:conc-name))
	 eline
	 position)

;;;
;;; The Multics file interface in EMACS operates on a "file-object".  A 
;;; file-object is the following list:
;;;
;;;	(CONTENTS ABSPATH UID FCB_PTR)
;;;
;;; where:
;;;
;;;   CONTENTS
;;;	a list of fileconses (ptr .length) to the segments that
;;;	make up the file (one if it is an archive component or SSF,
;;;	multiple if it is an MSF),
;;;   ABSPATH
;;;	is the absolute pathname of the Multics file (segment),
;;;   UID
;;;	is the Multics unique-id of the file and is used by find-file,
;;;   FCB_PTR
;;;	is a pointer to the msf_manager_ file control block for the file.
;;;   DTCM
;;;	is a fixnum representing the time the file was last modified.
;;;   ORIGINAL-ACCESS
;;;	if non-nil is a pointer to a data structure used to restore
;;;	access after it was forced.

(defstruct (file-object
	   (:type list)
	   (:conc-name fobj-))
	 contents
	 path
	 uid
	 (fcb-ptr nil)
	 dtcm
	 (original-access nil))

;;; A couple of special cases for segment 0
(defmacro fobj-seg0 (file-object)
	`(first (fobj-contents ,file-object)))
(defmacro fobj-pointer (file-object)
	`(filecons-pointer (fobj-seg0 ,file-object)))
(defmacro fobj-length (file-object)
	`(filecons-length (fobj-seg0 ,file-object)))
;;; And a common combination
(defmacro fobj-abs-path (file-object)
	`(absolute-pathname (fobj-path ,file-object)))

;;; A PATHNAME consists of a directory, an entryname, and an archive component
;;; name.  If it isn't an archive component, pn-component is the null string.
;;; The expand-pathname and absolute-pathname functions in e_multics_files_
;;; translate from and to a single character string.  Absolute-pathname
;;; encaches the absolute pathname in the abs-path component, to speed up
;;; repetitive calls.

(defstruct (pathname
	   (:type list)
	   (:conc-name pn-))
	 (directory "")
	 (entry "")
	 (component "")
	 (abs-path nil))

;;; 

;;; The packed-pointer defstruct is usable for manipulating
;;; pointers expressed as a Lisp fixnum.

(defstruct (packed-pointer
	   (:type fixnum)
	   (:conc-name pptr-))
	 ((bit-offset #o3606)
	  (seg-number #o2214)
	  (word-offset #o0022)))
