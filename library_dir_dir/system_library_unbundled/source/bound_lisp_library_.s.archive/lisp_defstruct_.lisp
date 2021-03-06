;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;;; -*- Mode:Lisp; Package:SI; Lowercase:True -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;	** (c) Copyright 1981 Massachusetts Institute of Technology **

;The master copy of this file is in MC:ALAN;NSTRUCT >
;The current PDP10 MacLisp copy is in MC:ALAN;STRUCT >
;The current Lisp machine copy is in AI:LISPM2;STRUCT >
;The current Multics MacLisp copy is in >udd>Mathlab>Bawden>defstruct.lisp
;  on MIT-Multics
;The current VMS-NIL copy is in [NIL.SRC.SPEC]STRUCT.LSP on HTJR

;*****  READ THIS PLEASE!  *****
;If you are thinking of munging anything in this file you might want to
;consider finding me (ALAN) and asking me to mung it for you.  There is more
;than one copy of this file in the world (it runs in PDP10 and Multics MacLisp,
;NIL, Franz, PSL and on LispMachines) and whatever amazing features you are
;considering adding might be usefull to those people as well.  If you still
;cannot contain yourself long enough to find me, AT LEAST send me a piece of
;mail describing what you did and why.  Thanks for reading this flame.
;				Alan Bawden (ALAN@MC)

(eval-when (eval compile)
   (sstatus nofeature MacLisp-10))

(%include sharpsign)
(%include defmacro)
(%include other_other)
(%include defstruct)

(declare (genprefix defstruct-internal-)
         (*expr dpb ldb)
         (macros t))

(eval-when (eval compile)
  (setsyntax #/: (ascii #\space) nil))


(eval-when (eval compile load)

#+MacLisp
(defun defstruct-retry-keyword (x)
  (let ((l (exploden x)))
    (if (= (car l) #/:)
	(implode (cdr l))
	x)))

#+LispM
(defun defstruct-retry-keyword (x)
  (intern (get-pname x) si:pkg-user-package))

#+NIL
(defmacro defstruct-retry-keyword (x)
  `(to-keyword ,x))

);End of eval-when (eval compile load)

;;; Eval this before attempting incremental compilation
(eval-when (eval compile)

#+MacLisp-10
(defmacro append-symbols args
  (do ((l (reverse args) (cdr l))
       (x)
       (a nil (if (or (atom x)
		      (not (eq (car x) 'quote)))
		  (if (null a)
		      `(exploden ,x)
		      `(nconc (exploden ,x) ,a))
		  (let ((l (exploden (cadr x))))
		    (cond ((null a) `',l)
			  ((= 1 (length l)) `(cons ,(car l) ,a))
			  (t `(append ',l ,a)))))))
      ((null l) `(implode ,a))
    (setq x (car l))))

#+Multics
(defmacro append-symbols args
  `(make_atom (catenate ,@args)))

#+LispM
(defmacro append-symbols args
  `(intern (string-append ,@args)))

#+NIL
(defmacro append-symbols args
  `(symbolconc ,@args))

(defmacro defstruct-putprop (sym val ind)
  `(push `(defprop ,,sym ,,val ,,ind) returns))

#+Multics
;;;lcp gobbles (defprop ... macro) at compile time, so we have to use
;;;putprop to be certain macro definitions make it into the object:
(defmacro defstruct-put-macro (sym fcn)
  `(push `(putprop ',,sym ',,fcn 'macro) returns))

#+MacLisp-10
(defmacro defstruct-put-macro (sym fcn)
  `(push `(defprop ,,sym ,,fcn macro) returns))

#+LispM
(defmacro defstruct-put-macro (sym fcn)
  (setq fcn (if (and (not (atom fcn))
		     (eq (car fcn) 'quote))
		`'(macro . ,(cadr fcn))
		`(cons 'macro ,fcn)))
  `(push `(fdefine ',,sym ',,fcn t) returns))

#+NIL
(defmacro defstruct-put-macro (sym fcn)
  `(push `(add-macro-definition ',,sym ',,fcn) returns))
							
(defmacro make-empty () `'%%defstruct-empty%%)

(defmacro emptyp (x) `(eq ,x '%%defstruct-empty%%))

;;;Here we must deal with the fact that error reporting works
;;;differently everywhere!

#+MacLisp-10
;;;first arg is ALWAYS a symbol or a quoted symbol:
(defmacro defstruct-error (message &rest args)
  (let* ((chars (nconc (exploden (if (atom message)
				     message
				     (cadr message)))
		       '(#/.)))		;"Bad frob" => "Bad frob."
	 (new-message
	  (maknam (if (null args)
		      chars
		      (let ((c (car chars)))	;"Bad frob." => "-- bad frob."
			(or (< c #/A)
			    (> c #/Z)
			    (rplaca chars (+ c #o40)))
			(append '(#/- #/- #\space) chars))))))
  `(error ',new-message
	  ,@(cond ((null args) `())
		  ((null (cdr args)) `(,(car args)))
		  (t `((list ,@args)))))))

#+Multics
;;;first arg is ALWAYS a string:
(defmacro defstruct-error (message &rest args)
  `(error ,(catenate "defstruct: "
		     message
		     (if (null args)
			 "."
			 ": "))
	  ,@(cond ((null args) `())
		  ((null (cdr args)) `(,(car args)))
		  (t `((list ,@args))))))

#+(or LispM NIL)
;;;first arg is ALWAYS a string:
(defmacro defstruct-error (message &rest args)
  (do ((l args (cdr l))
       (fs "")
       (na nil))
      ((null l)
      `(ferror nil
	       ,(string-append message
			       (if (null args)
				   "."
				   (string-append ":" fs)))
	       ,.(nreverse na)))
    (cond ((and (not (atom (car l)))
		(eq (caar l) 'quote)
		(symbolp (cadar l)))
	   (setq fs (string-append fs " " (string-downcase (cadar l)))))
	  (t
	   (push (car l) na)
	   (setq fs (string-append fs " ~S"))))))

);End of eval-when (eval compile)

;;;If you mung the the ordering af any of the slots in this structure,
;;;be sure to change the version slot and the definition of the function
;;;get-defstruct-description.  Munging the defstruct-slot-description 
;;;structure should also cause you to change the version "number" in this
;;;manner.
(defstruct (defstruct-description
	     (:type :list)
	     (:default-pointer description)
	     (:conc-name defstruct-description-)
	     (:alterant ())
	  #+stingy-defstruct
	     (:eval-when (eval compile)))
  (version 'one)
  type
  dummy ;used to be the displace function
  slot-alist
  named-p
  constructors
  (default-pointer nil)
  (but-first nil)
  size
  (property-alist nil)
  ;;end of "expand-time" slots
  name
  include
  (initial-offset 0)
  (eval-when '(eval compile load))
  alterant
  (conc-name nil)
  (callable-accessors #-(or LispM NIL) nil #+(or LispM NIL) t)
  (size-macro nil)
  (size-symbol nil)
  (predicate nil)
  (copier nil)
  (print nil)
  )

(defun get-defstruct-description (name)
  (let ((description (get name 'defstruct-description)))
    (cond ((null description)
	   (defstruct-error
	     "A structure with this name has not been defined" name))
	  ((not (eq (defstruct-description-version) 'one))
	   (defstruct-error "The internal description of this structure is
incompatible with the currently loaded version of defstruct,
you will need to recompile its definition"
		  name))
	  (t description))))

;;;See note above defstruct-description structure before munging this one.
(defstruct (defstruct-slot-description
	     (:type :list)
	     (:default-pointer slot-description)
	     (:conc-name defstruct-slot-description-)
	     (:alterant ())
	  #+stingy-defstruct
	     (:eval-when (eval compile)))
  number
  (ppss nil)
  init-code
  (type 'notype)
  (property-alist nil)
  ref-macro-name
  )

;;;Perhaps this structure wants a version slot too?
(defstruct (defstruct-type-description
	     (:type :list)
	     (:default-pointer type-description)
	     (:conc-name defstruct-type-description-)
	     (:alterant ())
	  #+stingy-defstruct
	     (:eval-when (eval compile)))
  ref-expander
  ref-no-args
  cons-expander
  cons-flavor
  (cons-keywords nil)
  (named-type nil)
  (overhead 0)
  (defstruct-expander nil)
  (predicate nil)
  (copier nil)
  )

;; (DEFSTRUCT (<name> . <options>) . <slots>) or (DEFSTRUCT <name> . <slots>)
;;
;; <options> is of the form (<option> <option> (<option> <val>) ...)
;;
;; <slots> is of the form (<slot> (<slot> <initial-value>) ...)
;;
;; Options:
;;   :TYPE defaults to HUNK
;;   :CONSTRUCTOR defaults to "MAKE-<name>"
;;   :DEFAULT-POINTER defaults to empty (if no <val> given defaults to "<name>")
;;   :CONC-NAME defaults to empty (if no <val> given defaults to "<name>-")
;;   :SIZE-SYMBOL defaults to empty (if no <val> given defaults to "<name>-SIZE")
;;   :SIZE-MACRO defaults to empty (if no <val> given defaults to "<name>-SIZE")
;;   :ALTERANT defaults to "ALTER-<name>"
;;   :BUT-FIRST must have a <val> given
;;   :INCLUDE must have a <val> given
;;   :PROPERTY (:property foo bar) gives the structure a foo
;;     property of bar.  (:property foo) gives a foo property of T.
;;   :INITIAL-OFFSET can cause defstruct to skip over that many slots.
;;   :NAMED takes no value.  Tries to make the structure a named type.
;;   :CALLABLE-ACCESSORS defaults to true on the LispMachine and NIL.  False
;;     elsewhere. 
;;   :EVAL-WHEN defaults to (eval compile load).  <val> must be given.
;;   :PREDICATE defaults to empty (if no <val> given defaults
;;     to "<name>-P").  Generates a predicate if possible.
;;   :COPIER defaults to empty (if no <val> given defaults to
;;     "COPY-<name>").  Generates a function to copy this structure.
;;   :PRINT (:print "#<spaceship at ~S by ~S>" (x-pos spaceship)
;;     (y-pos spaceship))  The name of the structure is used as
;;     the variable.
;;   <type> any type name can be used without a <val> instead of
;;     saying (:TYPE <type>) 
;;   <other> any symbol with a non-nil :defstruct-option property.  You say
;;     (<other> <val>) and the effect is that of (:property <other> <val>)
;;
;; Symbol properties used:
;;   DEFSTRUCT-TYPE-DESCRIPTION each type has one, it is a type-description.
;;   DEFSTRUCT-NAME each constructor, alterant and size macro
;;     has one, it is a name. 
;;   DEFSTRUCT-DESCRIPTION each name has one, it is a description (see below).
;;   DEFSTRUCT-SLOT each accesor has one, it is of the form: (<name> . <slot>)
;;   :DEFSTRUCT-OPTION if a symbol FOO has this property then it can be used as
;;     an option giving the structure a FOO property of the value (or T).

#+LispM
(defprop defstruct "Structure" definition-type-name)

;;;The order of forms returned by defstruct is sometimes critical.  Keep this
;;;in mind when munging this code:
(defmacro defstruct (options &body items)
  (let* ((description (defstruct-parse-options options))
	 (type-description (get (defstruct-description-type)
				'defstruct-type-description))
	 (name (defstruct-description-name))
	 (new-slots (defstruct-parse-items items description))
	 (returns nil))
    (push `',name returns)
    ;;This must be last, since to compile it might require that the structure
    ;;already be operable:
    (cond ((defstruct-description-print)
	   (push (defstruct-define-printer name (defstruct-description-print))
		 returns)))
    ;;Keep this as close to last as possible:
    (cond ((defstruct-type-description-defstruct-expander)
	   (setq returns (append (funcall (defstruct-type-description-defstruct-expander)
					  description)
				 returns))))
 #+LispM
    (push `(record-source-file-name ',name 'defstruct) returns)
    (let ((alterant (defstruct-description-alterant))
	  (size-macro (defstruct-description-size-macro))
	  (size-symbol (defstruct-description-size-symbol))
	  (predicate (defstruct-description-predicate))
	  (copier (defstruct-description-copier)))
      (cond (predicate
	     (push (funcall (or (defstruct-type-description-predicate)
				(defstruct-error
				  "This defstruct type cannot produce a predicate"
				  (defstruct-description-type) 'in name))
			    description
			    predicate)
		   returns)))
      (cond (copier
	     (push
	       (let ((copy-fun (defstruct-type-description-copier)))
		 (cond (copy-fun
			(funcall copy-fun description copier))
		       ((not (= 1 (defstruct-type-description-ref-no-args)))
			(defstruct-error
			  "This defstruct type cannot produce a copying function"
			  (defstruct-description-type) 'in name))
		       (t (do ((i (1- (defstruct-description-size)) (1- i))
			       (l nil (cons (cons i (funcall (defstruct-type-description-ref-expander)
							     i description 'x))
					    l)))
			      ((< i 0)
			       `(defun ,copier (x)
				  ,(invoke-defstruct-constructor-expander
				     description type-description l nil)))))))
	       returns)))
      (cond (alterant
	     (defstruct-put-macro alterant 'defstruct-expand-alter-macro)
	     (defstruct-putprop alterant name 'defstruct-name)))
      (cond (size-macro
	     (defstruct-put-macro size-macro 'defstruct-expand-size-macro)
	     (defstruct-putprop size-macro name 'defstruct-name)))
      (cond (size-symbol
	     (push `(defconst ,size-symbol
		      ,(+ (defstruct-description-size)
			  (defstruct-type-description-overhead)))
		   returns))))
    (defstruct-putprop name description 'defstruct-description)
    (do ((cs (defstruct-description-constructors) (cdr cs)))
	((null cs))
      (defstruct-put-macro (caar cs) 'defstruct-expand-cons-macro)
      (defstruct-putprop (caar cs) name 'defstruct-name))
    `(eval-when ,(defstruct-description-eval-when)
		,.(defstruct-define-ref-macros new-slots description)
		,.returns)))

;;;General philosophy on the :print option is to not bother the
;;;user if printing cannot be controled.  This allows for
;;;portability without pain.  This may prove to be a bogus philoshphy.
#+MacLisp-10
(defun defstruct-define-printer (name rest)
  (let ((stream (gensym)))
    `(defun (,name named-hunk-printer) (,name ,stream)
       (?format ,stream ,@rest))))

#+LispM
(defun defstruct-define-printer (name rest)
  (let ((op (gensym))
	(args (gensym)))
    `(defun (,name named-structure-invoke) (,op ,name &rest ,args)
       (selectq ,op
	 (:print-self
	   (if print-readably (print-not-readable ,name))
	   (format (car ,args) ,@rest))
	 (:which-operations '(:print-self))
	 (t (ferror nil "Illegal operation ~S" ,op))))))

#+NIL
(defun defstruct-define-printer (name rest)
  (let ((method-function-name (symbolconc name "->PRINT-SELF#METHOD"))
	(stream-var (gensym))
	(gubble (gensym)))
    `(progn 'compile
	(defun ,method-function-name (,name () () ,stream-var &rest ,gubble)
	  ,gubble	;ignored
	  (format ,stream-var ,@rest))
	(add-flavor-method-info ',name ':print-self ',method-function-name))))

#-(or LispM MacLisp-10 NIL)
(defun defstruct-define-printer (name rest)
  `(comment ,name ,@rest))

(defun defstruct-parse-options (options)
  (let ((name (if (atom options) options (car options)))
	(type nil)
	(constructors (make-empty))
	(alterant (make-empty))
	(included nil)
	(named-p nil)
	(but-first nil)
	(description (make-defstruct-description)))
    (setf (defstruct-description-name) name)
    (do ((op) (val) (vals)
	 (options (if (atom options) nil (cdr options))
		  (cdr options)))
	((null options))
      (if (atom (setq op (car options)))
	  (setq vals nil)
	  (setq op (prog1 (car op) (setq vals (cdr op)))))
      (setq val (if (null vals) (make-empty) (car vals)))
AGAIN (selectq op
	(:type
	 (if (emptyp val)
	     (defstruct-error
	       "The type option to defstruct must have a value given"
	       name))
	 (setq type val))
	(:named
	 (or (emptyp val)
	     (defstruct-error
	       "The named option to defstruct doesn't take a value" name))
	 (setq named-p t))
	(:default-pointer
	 (setf (defstruct-description-default-pointer)
	       (if (emptyp val) name val)))
	(:conc-name
	 (setf (defstruct-description-conc-name)
	       (if (emptyp val)
		   (append-symbols name '-)
		   val)))
	(:print
	 (if (emptyp val)
	     (defstruct-error
	       "The print option to defstruct requires a value"
	       name))
	 (setf (defstruct-description-print) vals))
	(:include
	 (if (emptyp val)
	     (defstruct-error
	       "The include option to defstruct requires a value"
	       name))
	 (setq included val)
	 (setf (defstruct-description-include) vals))
	(:predicate
	 (setf (defstruct-description-predicate)
	       (if (emptyp val)
		   (append-symbols name '-p)
		   val)))
	(:constructor
	 (cond ((null val)
		(setq constructors nil))
	       (t
		(and (emptyp val)
		     (setq val (append-symbols 'make- name)))
		(setq val (cons val (cdr vals)))
		(if (emptyp constructors)
		    (setq constructors (list val))
		    (push val constructors)))))
	(:copier
	 (setf (defstruct-description-copier)
	       (if (emptyp val)
		   (append-symbols 'copy- name)
		   val)))
	(:eval-when
	 (and (emptyp val)
	      (defstruct-error
		"The eval-when option to defstruct requires a value"
		name))
	 (setf (defstruct-description-eval-when) val))
	(:alterant
	 (setq alterant val))
	(:but-first
	 (if (emptyp val)
	     (defstruct-error
	       "The but-first option to defstruct must have a value given"
	       name))
	 (setq but-first val)
	 (setf (defstruct-description-but-first) val))
	(:size-macro
	 (setf (defstruct-description-size-macro)
	       (if (emptyp val)
		   (append-symbols name '-size)
		   val)))
	(:size-symbol
	 (setf (defstruct-description-size-symbol)
	       (if (emptyp val)
		   (append-symbols name '-size)
		   val)))
	(:callable-accessors
	 (setf (defstruct-description-callable-accessors)
	       (if (emptyp val) t val)))
	(:property
	 (if (emptyp val)
	     (defstruct-error
	       "The property option to defstruct requires a value"
	       name))
	 (push (cons val (if (null (cdr vals)) t (cadr vals)))
	       (defstruct-description-property-alist)))
	(:initial-offset
	 (and (or (emptyp val)
		  (not (fixp val)))
	      (defstruct-error
		"The initial-offset option to defstruct requires a fixnum"
		name))
	 (setf (defstruct-description-initial-offset) val))
	(t
	 (cond ((get op 'defstruct-type-description)
		(or (emptyp val)
		    (defstruct-error
		      "defstruct type used as an option with a value"
		      op 'in name))
		(setq type op))
	       ((get op ':defstruct-option)
		(push (cons op (if (emptyp val) t val))
		      (defstruct-description-property-alist)))
	       (t
		(let ((new (defstruct-retry-keyword op)))
		  (cond ((not (eq new op))
			 (setq op new)
			 (go AGAIN)))
		  (defstruct-error
		    "defstruct doesn't understand this option"
		    op 'in name)))))))
    (cond ((emptyp constructors)
	   (setq constructors
		 (list (cons (append-symbols 'make- name)
			     nil)))))
    (setf (defstruct-description-constructors) constructors)
    (cond ((emptyp alterant)
	   (setq alterant
		 (append-symbols 'alter- name))))
    (setf (defstruct-description-alterant) alterant)
    (cond ((not (null type))
	   (let ((type-description
		  (or (get type 'defstruct-type-description)
		      (let ((new (defstruct-retry-keyword type)))
			(cond ((eq type new) nil)
			      (t
			       (setq type new)
			       (get type 'defstruct-type-description))))
		      (defstruct-error
			"Unknown type in defstruct"
			type 'in name))))
	     (if named-p
		 (setq type
		       (or (defstruct-type-description-named-type)
			   (defstruct-error
			    "There is no way to make this defstruct type named"
			    type 'in name)))))))
    (cond (included
	   (let ((d (get-defstruct-description included)))
	     (if (null type)
		 (setq type (defstruct-description-type d))
		 (or (eq type (defstruct-description-type d))
		     (defstruct-error
		       "defstruct types must agree for include option"
		       included 'included 'by name)))
	     (and named-p
		  (not (eq type (defstruct-type-description-named-type
				  (or (get type 'defstruct-type-description)
				      (defstruct-error
					"Unknown type in defstruct"
					type 'in name 'including included)))))
		  (defstruct-error
		    "Included defstruct's type isn't a named type"
		    included 'included 'by name))
	     (if (null but-first)
		 (setf (defstruct-description-but-first)
		       (defstruct-description-but-first d))
		 (or (equal but-first (defstruct-description-but-first d))
		     (defstruct-error
		       "but-first options must agree for include option"
		       included 'included 'by name)))))
	  ((null type)
	   (setq type
	     (cond (named-p
		    #+MacLisp-10 ':named-hunk
		    #+Multics ':named-list
		    #+LispM ':named-array
		    #+NIL ':extend)
		   (t
		    #+MacLisp-10 ':hunk
		    #+Multics ':list
		    #+LispM ':array
		    #+NIL ':vector)))))
    (let ((type-description (or (get type 'defstruct-type-description)
				(defstruct-error
				  "Undefined defstruct type"
				  type 'in name))))
      (setf (defstruct-description-type) type)
      (setf (defstruct-description-named-p)
	    (eq (defstruct-type-description-named-type) type)))
    description))

(defun defstruct-parse-items (items description)
  (let ((name (defstruct-description-name))
	(offset (defstruct-description-initial-offset))
	(include (defstruct-description-include))
	(o-slot-alist nil)
	(conc-name (defstruct-description-conc-name)))
    (or (null include)
	(let ((d (get (car include) 'defstruct-description)))
	  (setq offset (+ offset (defstruct-description-size d))) 
	  (setq o-slot-alist
		(subst nil nil (defstruct-description-slot-alist d)))
	  (do ((l (cdr include) (cdr l))
	       (it) (val))
	      ((null l))
	    (cond ((atom (setq it (car l)))
		   (setq val (make-empty)))
		  (t
		   (setq val (cadr it))
		   (setq it (car it))))
	    (let ((slot-description (cdr (assq it o-slot-alist))))
	      (and (null slot-description)
		   (defstruct-error
		     "Unknown slot in included defstruct"
		     it 'in include 'included 'by name))
	      (setf (defstruct-slot-description-init-code) val)))))
    (do ((i offset (1+ i))
	 (l items (cdr l))
	 (slot-alist nil)
	 #+MacLisp-10 (chars (exploden conc-name)))
	((null l)
	 (setq slot-alist (nreverse slot-alist))
	 (setf (defstruct-description-size) i)
	 (setf (defstruct-description-slot-alist)
	       (nconc o-slot-alist slot-alist))
	 slot-alist)
      (cond ((atom (car l))
	     (push (defstruct-parse-one-field
		     (car l) i nil nil conc-name #+MacLisp-10 chars)
		   slot-alist))
	    ((atom (caar l))
	     (push (defstruct-parse-one-field
		     (caar l) i nil (cdar l) conc-name #+MacLisp-10 chars)
		   slot-alist))
	    (t
	     (do ((ll (car l) (cdr ll)))
		 ((null ll))
	       (push (defstruct-parse-one-field
		       (caar ll) i (cadar ll)
		       (cddar ll) conc-name #+MacLisp-10 chars)
		     slot-alist)))))))

(defun defstruct-parse-one-field (it number ppss rest conc-name #+MacLisp-10 chars)
  (let ((mname (if conc-name #+MacLisp-10 (implode (append chars (exploden it)))
			     #-MacLisp-10 (append-symbols conc-name it)
		   it)))
    (cons it (make-defstruct-slot-description
	       number number
	       ppss ppss
	       init-code (if (null rest) (make-empty) (car rest))
	       ref-macro-name mname))))

(defun defstruct-define-ref-macros (new-slots description)
  (let ((name (defstruct-description-name))
	(returns nil))
    (if (not (defstruct-description-callable-accessors))
	(do ((l new-slots (cdr l))
	     (mname))
	    ((null l))
	  (setq mname (defstruct-slot-description-ref-macro-name (cdar l)))
	  (defstruct-put-macro mname 'defstruct-expand-ref-macro)
	  (defstruct-putprop mname (cons name (caar l)) 'defstruct-slot))
	(let ((type-description
		(get (defstruct-description-type)
		     'defstruct-type-description)))
	  (let ((code (defstruct-type-description-ref-expander))
		(n (defstruct-type-description-ref-no-args))
	     #+LispM
		(parent `(,name defstruct))
		(but-first (defstruct-description-but-first))
		(default-pointer (defstruct-description-default-pointer)))
	    (do ((args nil (cons (gensym) args))
		 (i n (1- i)))
		((< i 2)
		 ;;Last arg (if it exists) is name of structure,
		 ;; for documentation purposes.
		 (and (= i 1)
		      (setq args (cons name args)))
		 (let ((body (cons (if but-first
				       `(,but-first ,(car args))
				       (car args))
				   (cdr args))))
		   (and default-pointer
			(setq args `((,(car args) ,default-pointer)
				     &optional ,@(cdr args))))
		   (setq args (reverse args))
		   (setq body (reverse body))
		   (do ((l new-slots (cdr l))
			(mname))
		       ((null l))
		     (setq mname (defstruct-slot-description-ref-macro-name
				   (cdar l)))
		     #+MacLisp 
		     ;;This must come BEFORE the defun. THINK!
		     (defstruct-put-macro mname 'defstruct-expand-ref-macro)
		     (let ((ref (lexpr-funcall
				  code
				  (defstruct-slot-description-number (cdar l))
				  description
				  body))
			   (ppss (defstruct-slot-description-ppss (cdar l))))
		       (push `(#+LispM defsubst-with-parent
			       #+NIL defsubst
			       #-(or LispM NIL) defun
			         ,mname #+LispM ,parent ,args
				 ,(if (null ppss) ref `(ldb ,ppss ,ref)))
			   returns))
		     (defstruct-putprop mname
					(cons name (caar l))
					'defstruct-slot))))))))
    returns))

#+LispM 
(defprop defstruct-expand-cons-macro
	 defstruct-function-parent
	 macroexpander-function-parent)

#+LispM 
(defprop defstruct-expand-size-macro
	 defstruct-function-parent
	 macroexpander-function-parent)

#+LispM 
(defprop defstruct-expand-alter-macro
	 defstruct-function-parent
	 macroexpander-function-parent)

#+LispM 
(defprop defstruct-expand-ref-macro 
	 defstruct-function-parent
	 macroexpander-function-parent)

#+LispM
(defun defstruct-function-parent (sym)
  (values (or (get sym 'defstruct-name)
	      (car (get sym 'defstruct-slot)))
	  'defstruct))

(defun defstruct-expand-size-macro (x)
  (let ((description (get-defstruct-description (get (car x) 'defstruct-name))))
    (let ((type-description (or (get (defstruct-description-type)
				     'defstruct-type-description)
				(defstruct-error
				  "Unknown defstruct type"
				  (defstruct-description-type)))))
      (+ (defstruct-description-size)
	 (defstruct-type-description-overhead)))))

(defun defstruct-expand-ref-macro (x)
  (let* ((pair (get (car x) 'defstruct-slot))
	 (description (get-defstruct-description (car pair)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (code (defstruct-type-description-ref-expander))
	 (n (defstruct-type-description-ref-no-args))
	 (args (reverse (cdr x)))
	 (nargs (length args))
	 (default (defstruct-description-default-pointer))
	 (but-first (defstruct-description-but-first)))
    (cond ((= n nargs)
	   (and but-first
		(rplaca args `(,but-first ,(car args)))))
	  ((and (= n (1+ nargs)) default)
	   (setq args (cons (if but-first
				`(,but-first ,default)
				default)
			    args)))
	  (t
	   (defstruct-error
	     "Wrong number of args to an accessor macro" x)))
    (let* ((slot-description 
	     (cdr (or (assq (cdr pair)
			    (defstruct-description-slot-alist))
		      (defstruct-error
			"This slot no longer exists in this structure"
			(cdr pair) 'in (car pair)))))
	    (ref (lexpr-funcall
		   code
		   (defstruct-slot-description-number)
		   description
		   (nreverse args)))
	    (ppss (defstruct-slot-description-ppss)))
      (if (null ppss)
	  ref
	  `(ldb ,ppss ,ref)))))

(defun defstruct-parse-setq-style-slots (l slots others x)
  (do ((l l (cddr l))
       (kludge (cons nil nil)))
      ((null l) kludge)
    (or (and (cdr l)
	     (symbolp (car l)))
	(defstruct-error
	  "Bad argument list to constructor or alterant macro" x))
    (defstruct-make-init-dsc kludge (car l) (cadr l) slots others x)))

(defun defstruct-make-init-dsc (kludge name code slots others x)
  (let ((p (assq name slots)))
    (if (null p)
	(if (memq name others)
	    (push (cons name code) (cdr kludge))
	    (let ((new (defstruct-retry-keyword name)))
	      (if (memq new others)
		  (push (cons new code) (cdr kludge))
		  (defstruct-error
		    "Unknown slot to constructor or alterant macro"
		    name 'in x))))
	(let* ((slot-description (cdr p))
	       (number (defstruct-slot-description-number))
	       (ppss (defstruct-slot-description-ppss))
	       (dsc (assoc number (car kludge))))
	  (cond ((null dsc)
		 (setq dsc (list* number nil (make-empty) 0 0 nil))
		 (push dsc (car kludge))))
	  (cond ((null ppss)
		 (setf (car (cddr dsc)) code)
		 (setf (cadr dsc) t))
		(t (cond ((and (numberp ppss) (numberp code))
			  (setf (ldb ppss (cadr (cddr dsc))) -1)
			  (setf (ldb ppss (caddr (cddr dsc))) code))
			 (t
			  (push (cons ppss code) (cdddr (cddr dsc)))))
		   (or (eq t (cadr dsc))
		       (push name (cadr dsc)))))))))

(defun defstruct-code-from-dsc (dsc)
  (let ((code (car (cddr dsc)))
	(mask (cadr (cddr dsc)))
	(bits (caddr (cddr dsc))))
    (if (emptyp code)
	(setq code bits)
	(or (zerop mask)
	    (setq code (if (numberp code)
			   (boole 7 bits (boole 2 mask code))
			   (if (zerop (logand mask
					      (1+ (logior mask (1- mask)))))
			       (let ((ss (haulong (boole 2 mask (1- mask)))))
				 `(dpb ,(lsh bits (- ss))
				       ,(logior (lsh ss 6)
						(logand #o77
							(- (haulong mask) ss)))
				       ,code))
			       `(boole 7 ,bits (boole 2 ,mask ,code)))))))
    (do ((l (cdddr (cddr dsc)) (cdr l)))
	((null l))
      (setq code `(dpb ,(cdar l) ,(caar l) ,code)))
    code))

(defun defstruct-expand-cons-macro (x)
  (let* ((description (get-defstruct-description (get (car x) 'defstruct-name)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (slot-alist (defstruct-description-slot-alist))
	 (cons-keywords (defstruct-type-description-cons-keywords))
	 (kludge nil)
	 (constructor-description 
	   (cdr (or (assq (car x) (defstruct-description-constructors))
		    (defstruct-error
		      "This constructor is no longer defined for this structure"
		      (car x) 'in (defstruct-description-name)))))
	 (aux nil)
	 (aux-init nil))
     (if (null constructor-description)
	 (setq kludge (defstruct-parse-setq-style-slots (cdr x)
							slot-alist
							cons-keywords
							x))
	 (prog (args l)
	       (setq kludge (cons nil nil))
	       (setq args (cdr x))
	       (setq l (car constructor-description))
	     R (cond ((null l)
		      (if (null args)
			  (return nil)
			  (go barf-tma)))
		     ((atom l) (go barf))
		     ((eq (car l) '&optional) (go O))
		     ((eq (car l) '&rest) (go S))
		     ((eq (car l) '&aux) (go A))
		     ((null args) (go barf-tfa)))
	       (defstruct-make-init-dsc kludge
					(pop l)
					(pop args)
					slot-alist
					cons-keywords
					x)
	       (go R)
	     O (and (null args) (go OD))
	       (pop l)
	       (cond ((null l) (go barf-tma))
		     ((atom l) (go barf))
		     ((eq (car l) '&optional) (go barf))
		     ((eq (car l) '&rest) (go S))
		     ((eq (car l) '&aux) (go barf-tma)))
	       (defstruct-make-init-dsc kludge
					(if (atom (car l)) (car l) (caar l))
					(pop args)
					slot-alist
					cons-keywords
					x)
	       (go O)
	    OD (pop l)
	       (cond ((null l) (return nil))
		     ((atom l) (go barf))
		     ((eq (car l) '&optional) (go barf))
		     ((eq (car l) '&rest) (go S))
		     ((eq (car l) '&aux) (go A)))
	       (or (atom (car l))
		   (defstruct-make-init-dsc kludge
					    (caar l)
					    (cadar l)
					    slot-alist
					    cons-keywords
					    x))
	       (go OD)
	     S (and (atom (cdr l)) (go barf))
	       (defstruct-make-init-dsc kludge
					(cadr l)
					`(list ,@args)
					slot-alist
					cons-keywords
					x)
	       (setq l (cddr l))
	       (and (null l) (return nil))
	       (and (atom l) (go barf))
	       (or (eq (car l) '&aux) (go barf))
	     A (pop l)
	       (cond ((null l) (return nil))
		     ((atom l) (go barf))
		     ((atom (car l))
		      (push (car l) aux)
		      (push (make-empty) aux-init))
		     (t
		      (push (caar l) aux)
		      (push (cadar l) aux-init)))
	       (go A)
	  barf (defstruct-error
		 "Bad format for defstruct constructor arglist"
		 `(,(car x) ,@(car constructor-description)))
      barf-tfa (defstruct-error "Too few arguments to constructor macro" x)
      barf-tma (defstruct-error "Too many arguments to constructor macro" x)))
     (do ((l slot-alist (cdr l)))
	 ((null l))
       (let* ((name (caar l))
	      (slot-description (cdar l))
	      (code (do ((aux aux (cdr aux))
			 (aux-init aux-init (cdr aux-init)))
			((null aux) (defstruct-slot-description-init-code))
		      (and (eq name (car aux)) (return (car aux-init)))))
	      (ppss (defstruct-slot-description-ppss)))
	 (or (and (emptyp code) (null ppss))
	     (let* ((number (defstruct-slot-description-number))
		    (dsc (assoc number (car kludge))))
	       (cond ((null dsc)
		      (setq dsc (list* number nil (make-empty) 0 0 nil))
		      (push dsc (car kludge))))
	       (cond ((emptyp code))
		     ((eq t (cadr dsc)))
		     ((null ppss)
		      (and (emptyp (car (cddr dsc)))
			   (setf (car (cddr dsc)) code)))
		     ((memq name (cadr dsc)))
		     ((and (numberp ppss) (numberp code))
		      (setf (ldb ppss (cadr (cddr dsc))) -1)
		      (setf (ldb ppss (caddr (cddr dsc))) code))
		     (t
		      (push (cons ppss code) (cdddr (cddr dsc)))))))))
     (do ((l (car kludge) (cdr l)))
	 ((null l))
       (rplacd (car l) (defstruct-code-from-dsc (car l))))
     (invoke-defstruct-constructor-expander
       description type-description
       (car kludge) (cdr kludge))))

(defun invoke-defstruct-constructor-expander (description type-description arg etc)
  (funcall (defstruct-type-description-cons-expander)
	   (selectq (defstruct-type-description-cons-flavor)
	     (:list
	      (do ((l nil (cons nil l))
		   (i (defstruct-description-size) (1- i)))
		  ((= i 0)
		   (do ((arg arg (cdr arg)))
		       ((null arg))
		     (setf (nth (caar arg) l) (cdar arg)))
		   l)))
	     (:alist arg)
	     (t
	      (defstruct-error
		"Unknown constructor kind in this defstruct type"
		(defstruct-description-type))))
	   description etc))

(defun defstruct-expand-alter-macro (x)
  (let* ((description (get-defstruct-description (get (car x) 'defstruct-name)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (ref-code (defstruct-type-description-ref-expander))
	 (ref-nargs (defstruct-type-description-ref-no-args)))
    (do ((l (car (defstruct-parse-setq-style-slots 
		   (nthcdr (1+ ref-nargs) x)
		   (defstruct-description-slot-alist)
		   nil
		   x))
	    (cdr l))
	 (but-first (defstruct-description-but-first))
	 (body nil)
	 (avars (do ((i 0 (1+ i))
		     (l nil (cons (gensym) l)))
		    ((= i ref-nargs) l)))
	 (vars nil)
	 (vals nil))
	((null l)
	 `((lambda ,avars
	     ,@(if (null vars)
		   body
		   `(((lambda ,vars ,@body) ,.vals))))
	   ,@(do ((i (1- ref-nargs) (1- i))
		  (l `(,(if but-first
			    `(,but-first ,(nth ref-nargs x))
			    (nth ref-nargs x)))
		     (cons (nth i x) l)))
		 ((= i 0) l))))
      (let ((ref (lexpr-funcall ref-code (caar l) description avars)))
	(and (emptyp (car (cddr (car l))))
	     (setf (car (cddr (car l))) ref))
	(let ((code (defstruct-code-from-dsc (car l))))
	  (if (null (cdr l))
	      (push `(setf ,ref ,code) body)
	      (let ((sym (gensym)))
		(push `(setf ,ref ,sym) body)
		(push sym vars)
		(push code vals))))))))

(defmacro defstruct-define-type (type &body options)
  (do ((options options (cdr options))
       (op) (args)
       (type-description (make-defstruct-type-description))
       (cons-expander nil)
       (ref-expander nil)
       (returns `(',type)))
      ((null options)
       (or cons-expander
	   (defstruct-error "No cons option in defstruct-define-type" type))
       (or ref-expander
	   (defstruct-error "No ref option in defstruct-define-type" type))
       `(progn 'compile
	       ,cons-expander
	       ,ref-expander
	       (defprop ,type ,type-description defstruct-type-description)
	       ,@returns))
    (cond ((atom (setq op (car options)))
	   (setq args nil))
	  (t
	   (setq args (cdr op))
	   (setq op (car op))))
 AGAIN
    (selectq op
      (:cons
        (or (> (length args) 2)
	    (defstruct-error
	      "Bad cons option in defstruct-define-type"
	      (car options) 'in type))
	(let ((n (length (car args)))
	      (name (append-symbols type '-defstruct-cons)))
	  (or (= n 3)
	      (defstruct-error
		"Bad cons option in defstruct-define-type"
		(car options) 'in type))
	  (setf (defstruct-type-description-cons-flavor)
		(defstruct-retry-keyword (cadr args)))
	  (setf (defstruct-type-description-cons-expander) name)
	  (setq cons-expander `(defun ,name ,(car args)
				 ,@(cddr args)))))
      (:ref
        (or (> (length args) 1)
	    (defstruct-error
	      "Bad ref option in defstruct-define-type"
	      (car options) 'in type))
	(let ((n (length (car args)))
	      (name (append-symbols type '-defstruct-ref)))
	  (or (> n 2)
	      (defstruct-error
		"Bad ref option in defstruct-define-type"
		(car options) 'in type))
	  (setf (defstruct-type-description-ref-no-args) (- n 2))
	  (setf (defstruct-type-description-ref-expander) name)
	  (setq ref-expander `(defun ,name ,(car args)
				,@(cdr args)))))
      (:predicate
        (or (> (length args) 1)
	    (defstruct-error
	      "Bad predicate option in defstruct-define-type"
	      (car options) 'in type))
        (let ((name (append-symbols type '-defstruct-predicate)))
	  (setf (defstruct-type-description-predicate) name)
	  (push `(defun ,name ,(car args)
		   ,@(cdr args))
		returns)))
      (:copier
        (or (> (length args) 1)
	    (defstruct-error
	      "Bad copier option in defstruct-define-type"
	      (car options) 'in type))
        (let ((name (append-symbols type '-defstruct-copier)))
	  (setf (defstruct-type-description-copier) name)
	  (push `(defun ,name ,(car args)
		   ,@(cdr args))
		returns)))
      (:overhead
        (setf (defstruct-type-description-overhead)
	      (if (null args)
		  (defstruct-error
		    "Bad option to defstruct-define-type"
		    (car options) 'in type)
		  (car args))))
      (:named
        (setf (defstruct-type-description-named-type)
	      (if (null args)
		  type
		  (car args))))
      (:keywords
        (setf (defstruct-type-description-cons-keywords) args))
      (:defstruct
        (or (> (length args) 1)
	    (defstruct-error
	      "Bad defstruct option in defstruct-define-type"
	      (car options) 'in type))
	(let ((name (append-symbols type '-defstruct-expand)))
	  (setf (defstruct-type-description-defstruct-expander) name)
	  (push `(defun ,name ,@args) returns)))
      (t
       (let ((new (defstruct-retry-keyword op)))
	 (cond ((not (eq op new))
		(setq op new)
		(go AGAIN)))
	 (defstruct-error
	   "Unknown option to defstruct-define-type"
	   op 'in type))))))

#+LispM
(defprop :make-array t :defstruct-option)

#+LispM
(defstruct-define-type :array
  (:named :named-array)
  (:keywords :make-array)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
			       description etc nil nil nil 1))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,n)))

#+MacLisp
(defstruct-define-type :array
  (:cons (arg description etc) :alist
    etc
    (maclisp-array-for-defstruct arg description 't))
  (:ref (n description arg)
    description		;ignored
    `(arraycall t ,arg ,n)))

#+NIL
(defstruct-define-type :array
  (:cons (arg description etc) :alist
    etc
    (NIL-array-for-defstruct arg description))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,n)))

#+LispM
(defstruct-define-type :named-array
  (:keywords :make-array)
  :named (:overhead 1)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,(1+ i)))
			       description etc nil t nil 1))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,(1+ n)))
  (:predicate (description name)
    `(defsubst ,name (x)
       (typep x ',(defstruct-description-name)))))

#+LispM
(defstruct-define-type :fixnum-array
  (:keywords :make-array)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
			       description etc 'art-32b nil nil 1))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,n)))

#+MacLisp
(defstruct-define-type :fixnum-array
  (:cons (arg description etc) :alist
    etc
    (maclisp-array-for-defstruct arg description 'fixnum))
  (:ref (n description arg)
    description		;ignored
    `(arraycall fixnum ,arg ,n)))

#+LispM
(defstruct-define-type :flonum-array
  (:keywords :make-array)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
			       description etc 'art-float nil nil 1))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,n)))

#+MacLisp
(defstruct-define-type :flonum-array
  (:cons (arg description etc) :alist
    etc
    (maclisp-array-for-defstruct arg description 'flonum))
  (:ref (n description arg)
    description		;ignored
    `(arraycall flonum ,arg ,n)))

#+MacLisp-10
(defstruct-define-type :un-gc-array
  (:cons (arg description etc) :alist
    etc			;ignored
    (maclisp-array-for-defstruct arg description nil))
  (:ref (n description arg)
    description		;ignored
    `(arraycall nil ,arg ,n)))

#+LispM
(defstruct-define-type :array-leader
  (:named :named-array-leader)
  (:keywords :make-array)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i)
				       `(store-array-leader ,v ,a ,i))
			       description etc nil nil t 1))
  (:ref (n description arg)
    description		;ignored
    `(array-leader ,arg ,n)))

#+LispM
(defstruct-define-type :named-array-leader
  (:keywords :make-array)
  :named (:overhead 1)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct
      arg
      #'(lambda (v a i)
	  `(store-array-leader ,v ,a ,(if (zerop i)
					  0
					  (1+ i))))
      description etc nil t t 1))
  (:ref (n description arg)
    description		;ignored
    (if (zerop n)
	`(array-leader ,arg 0)
	`(array-leader ,arg ,(1+ n))))
  (:predicate (description name)
    `(defsubst ,name (x)
       (typep x ',(defstruct-description-name)))))

#+LispM
(defprop :times t :defstruct-option)

#+LispM
(defstruct-define-type :grouped-array
  (:keywords :make-array :times)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct
      arg
      #'(lambda (v a i) `(aset ,v ,a ,i))
      description etc nil nil nil
      (or (cdr (or (assq ':times etc)
		   (assq ':times (defstruct-description-property-alist))))
	  1)))
  (:ref (n description index arg)
    description		;ignored
    (cond ((numberp index)
	   `(aref ,arg ,(+ n index)))
	  ((zerop n)
	   `(aref ,arg ,index))
	  (t `(aref ,arg (+ ,n ,index))))))

#+LispM
(defun lispm-array-for-defstruct (arg cons-init description etc type named-p leader-p times)
  (let ((p (cons nil nil))
	(no-op nil))
    (defstruct-grok-make-array-args
      (cdr (assq ':make-array (defstruct-description-property-alist)))
      p)
    (defstruct-grok-make-array-args
      (cdr (assq ':make-array etc))
      p)
    (and type (putprop p type ':type))
    (and named-p (putprop p `',(defstruct-description-name) ':named-structure-symbol))
    (putprop p
	     (let ((size (if named-p
			     (1+ (defstruct-description-size))
			     (defstruct-description-size))))
	       (if (numberp times)
		   (* size times)
		   `(* ,size ,times)))	     
	     (if leader-p ':leader-length ':dimensions))
    (or leader-p
	(if (get p ':initial-value)
	    (setq no-op (make-empty))
	    (let ((type (get p ':type)))
	      (or (atom type)
		  (not (eq (car type) 'quote))
		  (setq type (cadr type)))
	      (caseq type
		((nil art-q art-q-list))
		((art-32b art-16b art-8b art-4b art-2b art-1b art-string)
		 (setq no-op 0))
		((art-float) (setq no-op 0.0))
		(t (setq no-op (make-empty)))))))
    (do ((creator
	   (let ((dims (remprop p ':dimensions)))
	     (do ((l (cdr p) (cddr l)))
		 ((null l))
	       (rplaca l `',(car l)))
	     `(make-array ,(if (null dims) 0 (car dims)) ,@(cdr p))))
	 (var (gensym))
	 (set-ups nil (if (equal (cdar l) no-op)
			  set-ups
			  (cons (funcall cons-init (cdar l) var (caar l))
				set-ups)))
	 (l arg (cdr l)))
	((null l)
	 (if set-ups
	     `((lambda (,var)
		 ,@(nreverse set-ups)
		 ,var)
	       ,creator)
	     creator)))))

#+LispM
(defun defstruct-grok-make-array-args (args p)
  (do ((l args (cddr l)))
      ((null l) p)
    (if (or (null (cdr l))
	    (not (memq (car l) '(:area :type :displaced-to :leader-list
				 :leader-length :displaced-index-offset
				 :named-structure-symbol :dimensions
				 :length :initial-value))))
	(defstruct-error
	  "defstruct can't grok these make-array arguments"
	  args))
    (putprop p
	     (cadr l)
	     (if (eq (car l) ':length)
		 ':dimensions
		 (car l)))))

#+NIL
(defun NIL-array-for-defstruct (arg description)
  (do ((creator `(make-array ',(defstruct-description-size)))
       (var (gensym))
       (set-ups nil (if (null (cdar l))
			set-ups
			(cons `(aset ,(cdar l) ,var ,(caar l))
			      set-ups)))
       (l arg (cdr l)))
      ((null l)
       (if set-ups
	   `((lambda (,var)
	       ,@(nreverse set-ups)
	       ,var)
	     ,creator)
	   creator))))

#+MacLisp
(defun maclisp-array-for-defstruct (arg description type)
  (do ((creator `(array nil ,type ,(defstruct-description-size)))
       (var (gensym))
       (no-op (caseq type
		(fixnum 0)
		(flonum 0.0)
		((t nil) nil)))
       (set-ups nil (if (equal (cdar l) no-op)
			set-ups
			(cons `(store (arraycall ,type ,var ,(caar l))
				      ,(cdar l))
			      set-ups)))
       (l arg (cdr l)))
      ((null l)
       (if set-ups
	   `((lambda (,var)
	       ,@(nreverse set-ups)
	       ,var)
	     ,creator)
	   creator))))

#+(or MacLisp-10 NIL)
(defprop :sfa-function t :defstruct-option)

#+(or MacLisp-10 NIL)
(defprop :sfa-name t :defstruct-option)

#+(or MacLisp-10 NIL)
(defstruct-define-type :sfa
  (:keywords :sfa-function :sfa-name)
  (:cons (arg description etc) :alist
    (do ((creator `(sfa-create ,(or (cdr (or (assq ':sfa-function etc)
					     (assq ':sfa-function (defstruct-description-property-alist))))
				     `',(defstruct-description-name))
			       ,(defstruct-description-size)
			       ,(or (cdr (or (assq ':sfa-name etc)
					     (assq ':sfa-name (defstruct-description-property-alist))))
				    `',(defstruct-description-name))))
	 (l arg (cdr l))
	 (var (gensym))
	 (set-ups nil (if (null (cdar l))
			  set-ups
			  (cons `(sfa-store ,var ,(caar l)
					    ,(cdar l))
				set-ups))))
	((null l)
	 (if set-ups
	     `((lambda (,var)
		 ,@(nreverse set-ups)
		 ,var)
	       ,creator)
	     creator))))
  (:ref (n description arg)
    description		;ignored
    `(sfa-get ,arg ,n))
  (:predicate (description name)
    `(defun ,name (x)
       (and (sfap x)
	    (eq (sfa-get x 'pname)
		,(or (cdr (assq ':sfa-name (defstruct-description-property-alist)))
		     `',(defstruct-description-name)))))))

#+MacLisp-10
(defstruct-define-type :hunk
  (:named :named-hunk)
  (:cons (arg description etc) :list
    description		;ignored
    etc			;ignored
    (if arg
	`(hunk ,.(nconc (cdr arg) (ncons (car arg))))
	(defstruct-error "No slots in hunk type defstruct")))
  (:ref (n description arg)
    description		;ignored
    `(cxr ,n ,arg)))

#+MacLisp-10
(defstruct-define-type :named-hunk
  :named (:overhead 1)
  (:cons (arg description etc) :list
    etc			;ignored
    (if arg
	`(hunk ',(defstruct-description-name)
	       ,.(nconc (cdr arg) (ncons (car arg))))
	`(hunk ',(defstruct-description-name) nil)))
  (:ref (n description arg)
    description		;ignored
    (cond ((= n 0) `(cxr 0 ,arg))
	  (t `(cxr ,(1+ n) ,arg))))
  (:predicate (description name)
    `(defun ,name (x)
       (and (hunkp x)
	    (eq (car x) ',(defstruct-description-name))))))

#+(or MacLisp-10 NIL)
(defstruct-define-type :vector
  (:named :named-vector)
  (:cons (arg description etc) :list
    description		;ignored
    etc			;ignored
    `(vector ,@arg))
  (:ref (n description arg)
    description		;ignored
    `(vref ,arg ,n)))

#+(or MacLisp-10 NIL)
(defstruct-define-type :named-vector
  :named (:overhead 1)
  (:cons (arg description etc) :list
    etc			;ignored
    `(vector ',(defstruct-description-name) ,@arg))
  (:ref (n description arg)
    description		;ignored
    `(vref ,arg ,(1+ n)))
  (:predicate (description name)
    `(defun ,name (x)
       (and (vectorp x)
	    (eq (vref x 0) ',(defstruct-description-name))))))

#+NIL
(defprop :class-symbol t :defstruct-option)

#+NIL
(defstruct-define-type :extend
  :named
  (:defstruct (description)
    (if (assq ':class-symbol (defstruct-description-property-alist))
	;; if class-symbol is given then assume user is setting up
	;; his own class.
	()
	(let* ((name (defstruct-description-name))
	       (class-symbol (append-symbols name '-class)))
	  (push (cons ':class-symbol class-symbol)
		(defstruct-description-property-alist))
	  `((defstruct-class-setup ,name ,class-symbol)))))
  (:cons (arg description etc) :alist
    etc			;ignored
    (do ((l arg (cdr l))
	 (creator `(si:make-extend
		    ,(defstruct-description-size)
		    ,(cdr (assq ':class-symbol
				(defstruct-description-property-alist)))))
	 (var (gensym))
	 (set-ups () (if (null (cdar l))
			 set-ups
			 (cons `(si:xset ,var ,(caar l) ,(cdar l))
			       set-ups))))
	((null l)
	 (if set-ups
	     `((lambda (,var)
		 ,.(nreverse set-ups)
		 ,var)
	       ,creator)
	     creator))))
  (:ref (n description arg)
    description		;ignored
    `(si:xref ,arg ,n))
  (:predicate (description name)
    `(defsubst ,name (x)
       (of-type x ',(defstruct-description-name)))))

(defstruct-define-type :list
  (:named :named-list)
  (:cons (arg description etc) :list
    description		;ignored
    etc			;ignored
    `(list ,.arg))
  (:ref (n description arg)
    description		;ignored
 #+Multics
    `(,(let ((i (\ n 4)))
	 (cond ((= i 0) 'car)
	       ((= i 1) 'cadr)
	       ((= i 2) 'caddr)
	       (t 'cadddr)))
      ,(do ((a arg `(cddddr ,a))
	    (i (// n 4) (1- i)))
	   ((= i 0) a)))
 #-Multics
    `(nth ,n ,arg))
  (:copier (description name)
    (do ((l `((car x)) (cons `(prog1 (car x) (setq x (cdr x))) l))
	 (i (defstruct-description-size) (1- i)))
	((<= i 1)
	 `(defun ,name (x)
	    (list ,@l))))))

(defstruct-define-type :named-list
  :named (:overhead 1)
  (:cons (arg description etc) :list
    etc			;ignored
    `(list ',(defstruct-description-name) ,.arg))
  (:ref (n description arg)
    description		;ignored
 #+Multics
    `(,(let ((i (\ (1+ n) 4)))
	 (cond ((= i 0) 'car)
	       ((= i 1) 'cadr)
	       ((= i 2) 'caddr)
	       (t 'cadddr)))
      ,(do ((a arg `(cddddr ,a))
	    (i (// (1+ n) 4) (1- i)))
	   ((= i 0) a)))
 #-Multics
    `(nth ,(1+ n) ,arg))
  (:predicate (description name)
    `(defun ,name (x)
       (and
      #-MacLisp-10
	 (not (atom x))
      #+MacLisp-10	;Watch out for hunks!
         (eq (typep x) 'list)
	 (eq (car x) ',(defstruct-description-name)))))
  (:copier (description name)
    (do ((l `((car x)) (cons `(prog1 (car x) (setq x (cdr x))) l))
	 (i (defstruct-description-size) (1- i)))
	((<= i 1)
	 `(defun ,name (x)
	    (setq x (cdr x))
	    (list ',(defstruct-description-name) ,@l))))))

(defstruct-define-type :list*
  (:cons (arg description etc) :list
    description		;ignored
    etc			;ignored
    `(list* ,.arg))
  (:ref (n description arg)
    (let ((size (1- (defstruct-description-size))))
   #+Multics
      (do ((a arg `(cddddr ,a))
	   (i (// n 4) (1- i)))
	  ((= i 0)
	   (let* ((i (\ n 4))
		  (a (cond ((= i 0) a)
			   ((= i 1) `(cdr ,a))
			   ((= i 2) `(cddr ,a))
			   (t `(cdddr ,a)))))
	     (if (< n size) `(car ,a) a))))
   #-Multics
      (if (< n size)
	  `(nth ,n ,arg)
	  `(nthcdr ,n ,arg))))
  (:defstruct (description)
    (and (defstruct-description-include)
	 (defstruct-error
	   "Structure of type list* cannot include another"
	   (defstruct-description-name)))
    nil)
  (:copier (description name)
    (do ((l `(x) (cons `(prog1 (car x) (setq x (cdr x))) l))
	 (i (defstruct-description-size) (1- i)))
	((<= i 1)
	 `(defun ,name (x)
	    (list* ,@l))))))

(defstruct-define-type :tree
  (:cons (arg description etc) :list
    etc			;ignored
    (if (null arg) (defstruct-error
		     "defstruct cannot make an empty tree"
		     (defstruct-description-name)))
    (make-tree-for-defstruct arg (defstruct-description-size)))
  (:ref (n description arg)
    (do ((size (defstruct-description-size))
	 (a arg)
	 (tem))
	(nil)
      (cond ((= size 1) (return a))
	    ((< n (setq tem (// size 2)))
	     (setq a `(car ,a))
	     (setq size tem))
	    (t (setq a `(cdr ,a))
	       (setq size (- size tem))
	       (setq n (- n tem))))))
  (:defstruct (description)
    (and (defstruct-description-include)
	 (defstruct-error
	   "Structure of type tree cannot include another"
	   (defstruct-description-name)))
    nil)
  (:copier (description name)
    `(defun ,name (x)
       ,(copy-tree-for-defstruct nil (defstruct-description-size)))))

(defun make-tree-for-defstruct (arg size)
  (cond ((= size 1) (car arg))
	((= size 2) `(cons ,(car arg) ,(cadr arg)))
	(t (do ((a (cdr arg) (cdr a))
		(m (// size 2))
		(n (1- (// size 2)) (1- n)))
	       ((zerop n)
		`(cons ,(make-tree-for-defstruct arg m)
		       ,(make-tree-for-defstruct a (- size m))))))))

(defun copy-tree-for-defstruct (popx? size)
  (cond ((= size 1)
	 (if popx?
	     `(prog1 (car x) (setq x (cdr x)))
	     `x))
	((= size 2)
	 (if popx?
	     `((lambda (x) (cons (car x) (cdr x)))
	       (prog1 (car x) (setq x (cdr x))))
	     `(cons (car x) (cdr x))))
	(popx?
	 `((lambda (x)
	     (cons ,(copy-tree-for-defstruct t (// size 2))
		   ,(copy-tree-for-defstruct nil (- size (// size 2)))))
	   (prog1 (car x) (setq x (cdr x)))))
	(t
	 `(cons ,(copy-tree-for-defstruct t (// size 2))
		,(copy-tree-for-defstruct nil (- size (// size 2)))))))

(defstruct-define-type :fixnum
  (:cons (arg description etc) :list
    etc			;ignored
    (and (or (null arg)
	     (not (null (cdr arg))))
	 (defstruct-error
	   "Structure of type fixnum must have exactly 1 slot to be constructable"
	   (defstruct-description-name)))
    (car arg))
  (:ref (n description arg)
    n			;ignored
    description		;ignored
    arg))

#+Multics
(defprop :external-ptr t :defstruct-option)

#+Multics
(defstruct-define-type :external
  (:keywords :external-ptr)
  (:cons (arg description etc) :alist
    (let ((ptr (cdr (or (assq ':external-ptr etc)
			(assq ':external-ptr
			      (defstruct-description-property-alist))
			(defstruct-error
			  "No pointer given for external array"
			  (defstruct-description-name))))))
      (do ((creator `(array nil external ,ptr ,(defstruct-description-size)))
	   (var (gensym))
	   (alist arg (cdr alist))
	   (inits nil (cons `(store (arraycall fixnum ,var ,(caar alist))
				    ,(cdar alist))
			    inits)))
	  ((null alist)
	   (if (null inits)
	       creator
	       `((lambda (,var) ,.inits ,var)
		 ,creator))))))
  (:ref (n description arg)
    description	;ignored
    `(arraycall fixnum ,arg ,n)))

(defvar *defstruct-examine&deposit-arg*)

(defun defstruct-examine (*defstruct-examine&deposit-arg*
			  name slot-name)
  (eval (list (defstruct-slot-description-ref-macro-name
		(defstruct-examine&deposit-find-slot-description
		  name slot-name))
	      '*defstruct-examine&deposit-arg*)))

(defvar *defstruct-examine&deposit-val*)

(defun defstruct-deposit (*defstruct-examine&deposit-val*
			  *defstruct-examine&deposit-arg*
			  name slot-name)
  (eval (list 'setf
	      (list (defstruct-slot-description-ref-macro-name
		     (defstruct-examine&deposit-find-slot-description
		       name slot-name))
		    '*defstruct-examine&deposit-arg*)
	      '*defstruct-examine&deposit-val*)))

#+LispM
(defun defstruct-get-locative (*defstruct-examine&deposit-arg*
			       name slot-name)
  (let ((slot-description (defstruct-examine&deposit-find-slot-description
			    name slot-name)))
    (or (null (defstruct-slot-description-ppss))
	(defstruct-error
	  "You cannot get a locative to a byte field"
	  slot-name 'in name))
    (eval (list 'locf
		(list (defstruct-slot-description-ref-macro-name)
		      '*defstruct-examine&deposit-arg*)))))

(defun defstruct-examine&deposit-find-slot-description (name slot-name)
  (let ((description (get-defstruct-description name)))
    (let ((slot-description
	    (cdr (or (assq slot-name (defstruct-description-slot-alist))
		     (defstruct-error
		       "No such slot in this structure"
		       slot-name 'in name))))
	  (type-description
	    (or (get (defstruct-description-type) 'defstruct-type-description)
		(defstruct-error
		  "Undefined defstruct type"
		  (defstruct-description-type)))))
      (or (= (defstruct-type-description-ref-no-args) 1)
	  (defstruct-error
	    "defstruct-examine and defstruct-deposit cannot handle structures of this type"
	    (defstruct-description-type)))
      slot-description)))

#+MacLisp-10
(defprop defstruct
	 #.(and (status feature MacLisp-10)
		(caddr (truename infile)))
	 version)

(sstatus feature defstruct)
