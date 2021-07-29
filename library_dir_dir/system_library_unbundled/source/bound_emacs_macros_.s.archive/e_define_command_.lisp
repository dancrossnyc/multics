;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1979 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;
;;;
;;;       Emacs Command Definition Macro
;;;
;;;
;;; Initial coding:  August-September 1979 by GMP.
;;; Modified:	 7 May 1981 Soley to fix minor bugs.
;;; Modified:	 Fall 1981 Soley for &nobreak and &completions.
;;; Modified:	 31 March 1982 Soley for &undo-function, clean up.
;;; Modified:	 15 May 1982 by J. Spencer Love for &epilogue, &cleanup.
;;;
;;;
;;; Syntax:
;;;	  (define-command function-name
;;;		        forms)
;;;
;;;	  (defcom function-name forms)
;;;
;;; Keywords in forms:
;;;
;;; &arguments ARGS,  &args ARGS,  &a ARGS
;;;
;;; &cleanup FUNCTION
;;;
;;; &documentation STRING,  &doc STRING
;;;
;;; &epilogue SYMBOL
;;;
;;; &negative-function FUNCTION,  &nf FUNCTION
;;;
;;; &numeric-argument SPEC,  &numarg SPEC,  &na SPEC
;;;
;;; &numeric-function SYMBOL
;;;
;;; &no-break
;;;
;;; &prologue SYMBOL
;;;
;;; &undo-function SPEC,  &undo SPEC,  &inverse SPEC
;;;
;;; Terms:
;;;	ARGS is a list of argument specifications.
;;;	STRING is a character string in doublequotes.
;;;	SYMBOL is the name of a function.
;;;	FUNCTION is either a SYMBOL, or &code FORMS &end_code.
;;;	VALUE is either a CONSTANT or &eval FORM.
;;;	CONSTANT is either a STRING, a number, or a quoted FORM.
;;;	FORM is something for lisp to evaluate.
;;;
;;; Format of an argument specification:
;;;
;;; Format of &numeric-argument FORM:
;;;	&numeric-argument is followed by a keyword or by a list which
;;;	contains one or more keywords.  The keywords which may appear
;;;	after &numeric-argument are &pass, &ignore, &reject and &repeat.
;;;	The additional keywords which may appear in the list are
;;;	&lower-bound (&lb) and &upper-bound (&ub).  These two keywords
;;;	are followed by a VALUE.  &reject is the default unless bounds
;;;	are specified, in which case &pass is the default.
;;;

(declare (macros t)
         (*lexpr dc-error dc-error-argument dcev-error)
         (special encoded-values))

(%include backquote)

;;; Macro to define an Emacs command

(defprop define-command define-command/ MACRO macro)
(defprop defcom define-command/ MACRO macro)

;;; Macro to define a synonym of an Emacs command
;;; Syntax: (defcom-synonym synonym command)

(defun define-command-synonym macro (form)
       (let ((synonym (cadr form))
	   (command (caddr form)))
	  `(progn 'compile
		(defprop ,synonym ,command editor-command)
		(defprop ,synonym ,command expr))))

(defprop defcom-synonym define-command-synonym macro)

;;;
;;; Function that parses a command definition
;;;

(defun define-command/ MACRO (the-form)
       (let ((function-name (cadr the-form))       ;first arg must be name
             (the-function)
	   (prologue-function)
	   (epilogue-function)
	   (numeric-function)
             (negative-function)
	   (cleanup-function)
	   (undo-function)
	   (undo-info)
             (argument-info)
	   (numeric-arg-info)
             (documentation))

            (or (symbolp function-name)
	      (dc-error "" "Function name must be a symbol."))
	  (setq encoded-values nil)

	  (do ((form (cddr the-form) rest-of-form)
                 (current)
	       (no-break nil)
                 (rest-of-form))
                ((null form)                      ;until nothing left
                 (dc-build-result-list function-name (nreverse the-function)
			         prologue-function epilogue-function
			         numeric-function negative-function
                                       argument-info numeric-arg-info
			         documentation no-break undo-function
			         undo-info cleanup-function))

	      (setq current (car form) rest-of-form (cdr form))

                (cond

	        ;; Prologue function.
	        ((eq current '&prologue)
	         (and prologue-function
		    (dc-error-duplicate-key function-name current))
	         (let ((x (dc-parse-encoded-value function-name 0
					  rest-of-form current
					  'symbolp
					  "a function name")))
		    (setq prologue-function (cadar x)    ;want the symbol
			rest-of-form (cdr x))))

	        ;; Epilogue function.
	        ((eq current '&epilogue)
	         (and epilogue-function
		    (dc-error-duplicate-key function-name current))
	         (let ((x (dc-parse-encoded-value function-name 0
					  rest-of-form current
					  'symbolp
					  "a function name")))
		    (setq epilogue-function (cadar x)    ;want the symbol
			rest-of-form (cdr x))))

	        ;; Cleanup function.
	        ((eq current '&cleanup)
	         (and cleanup-function
		    (dc-error-duplicate-key function-name current))
	         (let ((x (dc-parse-encoded-value function-name 0
					  rest-of-form current
					  'symbolp
					  "a function name")))
		    (setq cleanup-function (cadar x)    ;want the symbol
			rest-of-form (cdr x))))

	        ;; Numeric function.
	        ((eq current '&numeric-function)
	         (and numeric-function
		    (dc-error-duplicate-key function-name current))
	         (let ((x (dc-parse-encoded-value function-name 0
					  rest-of-form current
					  'symbolp
					  "a function name")))
		    (setq numeric-function (cadar x)    ;want the symbol
			rest-of-form (cdr x))))

	        ;; Negative function.
	        ((memq current '(&negative-function &nf))
	         (and negative-function
		    (dc-error-duplicate-key function-name
				        '&negative-function))
	         (cond ((symbolp (car rest-of-form))
		      (cond ((eq (car rest-of-form) '&code)
			   (setq rest-of-form (cdr rest-of-form))
			   (do	;get the function body
			     ((nf (car rest-of-form)
				(car rest-of-form)))
			     ((or (null nf)
				(and (symbolp nf)
				     (samepnamep (substr nf 1 1)
					       "&")))
			      (and (eq nf '&end-code)
				 (setq rest-of-form
				       (cdr rest-of-form))))
			     (setq negative-function
				 (nconc negative-function 
				        (list nf))
				 rest-of-form (cdr rest-of-form))))
			  ((samepnamep (substr (car rest-of-form)
					   1 1)
				     "&")
			   (dc-error function-name
				   "Unknown keyword after "
				   "&negative-function: "
				   (car rest-of-form)))
			  (t		;function name
			    (setq negative-function
				(car rest-of-form)
				rest-of-form (cdr rest-of-form)))))
		     (t		;bad syntax
		       (dc-error function-name
			       "&negative-function must be followed "
			       "by a function name or &code."))))

	        ;; Undo function.
	        ((memq current '(&undo-function &undo &inverse))
	         (and (or undo-function undo-info)
		    (dc-error-duplicate-key function-name current))
	         (cond ((member (car rest-of-form) '(&pass (&pass)))
		      (setq undo-info '&pass
			  rest-of-form (cdr rest-of-form)))
		     ((member (car rest-of-form) '(&ignore (&ignore)))
		      (setq undo-info '&ignore
			  rest-of-form (cdr rest-of-form)))
		     ((or (eq (car rest-of-form) '&reject)
			(equal (car rest-of-form) '(&reject)))
		      (setq undo-info '&reject
			  rest-of-form (cdr rest-of-form)))
		     ((not (symbolp (car rest-of-form)))
		      (dc-error
		        function-name
		        "&undo-function must be followed "
		        "by a function name, &pass, &reject, "
		        "&ignore, or &code."))
		     ((eq (car rest-of-form) '&code)
		      (setq rest-of-form (cdr rest-of-form))
		      (do	;get the function body
		        ((nf (car rest-of-form)
			   (car rest-of-form)))
		        ((or (null nf)
			   (and (symbolp nf)
			        (samepnamep (substr nf 1 1) "&")))
		         (and (eq nf '&end-code)
			    (setq rest-of-form
				(cdr rest-of-form))))
		        (setq undo-function
			    (nconc undo-function (list nf))
			    rest-of-form (cdr rest-of-form))))
		     ((samepnamep (substr (car rest-of-form) 1 1) "&")
		      (dc-error function-name
			      "Unknown keyword after "
			      "&undo-function: "
			      (car rest-of-form)))
		     (t		;function name
		       (setq undo-function (car rest-of-form)
			   rest-of-form (cdr rest-of-form)))))

	        ;; Numeric argument.
	        ((memq current '(&numeric-argument &numarg &na))
	         (and numeric-arg-info
		    (dc-error-duplicate-key function-name
				        '&numeric-argument))
	         (setq numeric-arg-info
		     (dc-parse-numeric-arg-info function-name
					  (car rest-of-form))
		     rest-of-form (cdr rest-of-form)))

	        ;; Signal echnego that this doesn't cause a break.
	        ((eq current '&no-break) (setq no-break t))

	        ;; Arguments.
	        ((memq current '(&arguments &args &a))
	         (and argument-info
		    (dc-error-duplicate-key function-name '&arguments))
	         (setq argument-info
		     (dc-parse-arguments function-name (car rest-of-form))
		     rest-of-form (cdr rest-of-form)))

	        ;; Documentation.
	        ((memq current '(&documentation &doc))
	         (and documentation
		    (dc-error-duplicate-key function-name '&documentation))
	         (setq documentation (car rest-of-form)
		     rest-of-form (cdr rest-of-form))
	         (or (stringp documentation)
		   (dc-error function-name
			   "&documentation must be followed "
			   "by a string.")))

	        ;; Unknown.  Might be the actual function.....
	        ((and (symbolp current)
		    (samepnamep (substr current 1 1) "&"))
	         (dc-error function-name "Unrecoginzed keyword: " current))

	        ;; Yup, it's the function.
	        (t (setq the-function (cons current the-function)))))))

;;;
;;; Parse an encoded value: an encoded value is either a constant or
;;;  &eval followed by a form to evaluate at runtime
;;;

(defun dc-parse-encoded-value (function-name arg-no rest-of-form
			 qualifier-name value-typep value-name)
       (let ((type 'quote)                        ;just a value
	   (value (car rest-of-form)))
	  (cond ((eq value '&eval)
	         (let ((eval-name
		       (make_atom (catenate function-name "-$-"
				        (dc-decimal arg-no)
				        "-$-" qualifier-name)))
		     (function))
		    (or (cdr rest-of-form)	;if nothing follows it
		        (dcev-error function-name arg-no qualifier-name
				" &eval must be followed by a form "
				"or &code."))
		    (cond ((eq (cadr rest-of-form) '&code)
			 (setq rest-of-form (cddr rest-of-form))
			 (do		;construct the function
			   ((nf (car rest-of-form) (car rest-of-form)))
			   ((or (null nf)	;either nothing left
			        (and (symbolp nf)
				   (samepnamep (substr nf 1 1) "&")))
			    (and (eq nf '&end-code)
			         (setq rest-of-form (cdr rest-of-form))))
			   (setq function (nconc function (list nf))
			         rest-of-form (cdr rest-of-form))))
			((and (symbolp (cadr rest-of-form))
			      (samepnamep (substr (cadr rest-of-form)
					      1 1)
				        "&"))
			 (dcev-error function-name arg-no qualifier-name
				   " Unknown keyword following &eval: "
				   (cadr rest-of-form)))
			(t (setq function (list (cadr rest-of-form))
			         rest-of-form (cddr rest-of-form))))
		    (setq encoded-values
			`(,.encoded-values
			  (defun ,eval-name ()
			         . ,function))
			type 'eval
			value eval-name)))	;get name in right place
	        (t                              ;simple value, check type
		(or (funcall value-typep value)
		    (dcev-error function-name arg-no qualifier-name
			      " must be followed by " value-name
			      " or &eval."))
		(setq rest-of-form (cdr rest-of-form))))
	  (cons (list type value) rest-of-form)))

;;;
;;; Parse specifications for handling of numeric argument by this command.
;;;

(defun dc-parse-numeric-arg-info (function-name the-form)
       (do ((form the-form rest-of-form)
            (processing-type)
            (lower) (upper)
            (phrase) (rest-of-form))
           ((null form)			;until all parsed
            (and (eq processing-type '&ignore)	;ignore the argument
                 (or lower upper)
	       (dc-error function-name
		       "&ignore may not be used with other "
		       "&numeric-argument qualifiers."))
	  (and (eq processing-type '&reject)	;reject numeric arguments
                 (or lower upper)
	       (dc-error function-name
                           "&reject may not be used with other "
		       "&numeric-argument qualifiers."))
	  (and lower (eq (car lower) 'quote)
	       upper (eq (car upper) 'quote)
	       (< (cadr upper) (cadr lower))    ;invalid range
	       (dc-error function-name
		       "Invalid numeric argument range "
		       (dc-decimal (cadr lower))
		       ":" (dc-decimal (cadr upper))))
	  (and (null processing-type)
	       (or lower upper)
	       (setq processing-type '&pass))
	  (cons (or processing-type '&reject)	;supply default if needed
	        (and (or lower upper) (cons lower upper))))
	 ;;
	 ;; Find next token to be digested.
	 ;;
	 (cond ((not (atom form))
	        (setq phrase (car form)
		      rest-of-form (cdr form)))
	       ((not (eq form the-form))
	        (dc-error function-name
		        "Malformed list following &numeric-argument."))
	       (t (setq phrase form
		    rest-of-form nil)))
	 ;;
	 ;; Analyze token.
	 ;;
	 (cond ((memq phrase '(&pass &repeat &ignore &reject))
                  (and processing-type
                       (dc-error function-name
			   "Only one of &pass, &repeat, &ignore, or "
			   "&reject may appear after "
			   "&numeric-argument."))
	        (setq processing-type phrase))
	       ((memq phrase '(&lower-bound &lb))
	        (and lower
		   (dc-error function-name
			   "The key &lower-bound may only appear once "
			   "after &numeric-argument."))
	        (let ((x (dc-parse-encoded-value function-name 0 rest-of-form
					 '&lower-bound 'fixp 
					 "an integer")))
		   (setq lower (car x) rest-of-form (cdr x))))
	       ((memq phrase '(&upper-bound &ub))
	        (and upper
		   (dc-error function-name
			   "The key &upper-bound may only appear once "
			   "after &numeric-argument."))
	        (let ((x (dc-parse-encoded-value function-name 0 rest-of-form
					 '&upper-bound 'fixp
					 "an integer")))
		   (setq upper (car x) rest-of-form (cdr x))))
	       (t (dc-error function-name
			"Unrecognized keyword following "
			"&numeric-argument: " phrase)))))

;;;
;;; Parse the list of argument specifications.
;;;

(defun dc-parse-arguments (function-name the-form)
       (do ((form the-form (cdr form))
            (argument-list)
            (arg-no 1 (1+ arg-no)))               ;for error messages
           ((null form)			;done when out of forms
	  (nreverse argument-list))
           (let ((phrase (car form)))
                (let ((x (dcpa-single-argument function-name
				       (null (cdr form))
				       arg-no phrase)))
		 (setq argument-list (cons x argument-list))))))

;;;
;;; Parse a single argument specification.
;;;

(defun dcpa-single-argument (function-name last-argp arg-no specification)
       (let ((name)                               ;argument symbol
             (data-type)                          ;datatype of argument
             (default-value)                      ;default value
             (prompt-info)                        ;prompt string and terminator
             (range-info)                         ;range for integers
	   (completion-info)		;for completer command
             (validation-info))                   ;acceptable values of symbols
            (cond ((symbolp specification)        ;simple case
	         (setq name specification))
                  (t                              ;more complex
                    (setq name (car specification))
                    (or (symbolp name)
		    (dc-error-argument function-name arg-no
				   "No name specified."))
		(do ((qualifiers (cdr specification) rest-of-qualifiers)
		     (current) (rest-of-qualifiers))
		    ((null qualifiers))       ;until nothing left
		    (setq current (car qualifiers)
			rest-of-qualifiers (cdr qualifiers))
		    (cond
		      ((or (not (symbolp current))
			 (not (samepnamep (substr current 1 1) "&")))
		       (dc-error-argument function-name arg-no
				      "An & construct was expected, "
				      "but not found.")))
		    (cond
		      ((memq current '(&rest-as-string &rest-as-list))
		       (and (or data-type prompt-info default-value
			      range-info completion-info validation-info
			      rest-of-qualifiers)
			  (dc-error-argument
			    function-name arg-no current
			    " may not appear with any other argument "
			    "qualifiers."))
		       (or last-argp
			 (dc-error-argument function-name arg-no
					"The argument with "
					current " must be last."))
		       (setq data-type current))
		      ((memq current '(&completions &completion &comp))
		       (and completion-info
			  (dc-error-argument-duplicate-key
			    function-name arg-no '&completions))
		       (setq completion-info (car rest-of-qualifiers)
			   rest-of-qualifiers (cdr rest-of-qualifiers)))
		      ((memq current '(&string &symbol &integer))
		       (and data-type
			  (dc-error-argument
			    function-name arg-no
			    "Only one of &string, &symbol, and "
			    "&integer may be used."))
		       (setq data-type current))
		      ((eq current '&prompt)   ;prompt string
		       (and prompt-info
			  (dc-error-argument-duplicate-key
			    function-name arg-no '&prompt))
		       (let ((x (dcpa-parse-prompt function-name arg-no
					     rest-of-qualifiers)))
			  (setq prompt-info (car x)
			        rest-of-qualifiers (cdr x))))
		      ((eq current '&default)  ;default value
		       (and default-value
			  (dc-error-argument-duplicate-key
			    function-name arg-no '&default))
		       (let ((x (dc-parse-encoded-value
			        function-name arg-no rest-of-qualifiers
			        '&default 'atom "a value")))
			  (setq default-value (car x)
			        rest-of-qualifiers (cdr x))))
		      (t
		        (dc-error-argument function-name arg-no
				       "Unrecognized keyword: "
				       current))))))

	  ;; Perform consistency checks and construct value
	  (or data-type (setq data-type '&string))
	  (and validation-info (not (eq data-type '&symbol))
	       (dc-error-argument function-name arg-no
                                    "&valid may only be specified for "
			      "&symbol arguments."))
	  (and range-info (not (eq data-type '&integer))
	       (dc-error-argument function-name arg-no
			      "Numeric ranges may only be specified "
			      "for &integer arguments."))
	  (or prompt-info default-value
	      (setq prompt-info (cons (list 'quote (catenate name ": "))
				(ascii 012)))) ;end with newline
	  (cons name
	        (list
		(boole 7                  ;fixnum describing argument
		       (cond
		         ((eq data-type '&string) 0)
		         ((eq data-type '&symbol) (lsh 100000 18.))
		         ((eq data-type '&integer) (lsh 200000 18.))
		         ((eq data-type '&rest-as-string) (lsh 300000 18.))
		         ((eq data-type '&rest-as-list) (lsh 400000 18.))
		         (t (lsh 700000 18.)))
		       (cond (prompt-info (lsh 040000 18.))   ;have prompt
			   (t 0))
		       (cond (default-value (lsh 020000 18.)) (t 0))
		       (cond ((or range-info validation-info)
			    (lsh 010000 18.))
			   (t 0)))
		prompt-info
		default-value
		(cond ((eq data-type '&integer) range-info)
		      (t validation-info))
		completion-info))))

;;;
;;; Parse prompt specification
;;;

(defun dcpa-parse-prompt (function-name arg-no rest-of-qualifiers)
       (let ((x (dc-parse-encoded-value function-name arg-no rest-of-qualifiers
                                        '&prompt 'stringp "a string")))
            (let ((string (car x))
                  (rest (cdr x))
                  (term))
                 (setq term (car rest))           ;get possible terminator
                 (cond ((and term                 ;there's something
                             (symbolp term)
                             (not (samepnamep (substr term 1 1) "&")))
                        (cond ((memq term '(NL ESC))
                               (setq term (cond ((eq term 'NL) (ascii 012))
                                                (t (ascii 033)))
                                     rest (cdr rest)))
                              (t
                                (dc-error-argument function-name arg-no
					 "Prompt terminator must "
					 "be NL or ESC."))))
		   (t (setq term (ascii 012))))
                 (cons (cons string term)
                       rest))))

;;; Create result of define-command macro
(defun dc-build-result-list (function-name the-function prologue-function
		         epilogue-function numeric-function
		         negative-function argument-info numeric-arg-info
		         documentation no-break undo-function undo-info
		         cleanup-function)
       ;;
       ;; Check numeric function conflicts.
       ;;
       (and numeric-function negative-function
	  (dc-error-conflict function-name "&numeric-function"
			 "&negative-function"))
       (and numeric-function numeric-arg-info
	  (dc-error-conflict function-name "&numeric-function"
			 "&numeric-argument"))
       (and numeric-function
	  (setq numeric-arg-info '(&pass)))
       ;;
       ;; Check for negative function conflicts.
       ;;
       (and negative-function
	  (null numeric-arg-info)
	  (dc-error-required function-name "&negative-function"
			 "&numeric-argument"))
       (and negative-function
            (eq (car numeric-arg-info) '&ignore)	;But ignore argument!
	  (dc-error-conflict function-name "&negative-function"
			 "&numeric-argument (&ignore)"))
       (and negative-function
	  (eq (car numeric-arg-info) '&reject)	;But reject argument!
            (dc-error-conflict function-name "&negative-function"
			 "&numeric-argument (&reject)"))
       ;;
       ;; Default numeric argument handling.
       ;;
       (or numeric-arg-info (setq numeric-arg-info '(&reject)))
       ;;
       ;; Construct flag word for execute-new-command.
       ;;
       (let ((result ())
	   (editor-command-value
               (boole 7			;Logical OR.
		  (cond (argument-info
			(boole 7		;Logical OR.
			       (lsh 400000 18.)
			       (boole 1 (length argument-info) 777777)))
		        (t 0))                ;No arguments.
		  (cond (negative-function (lsh 200000 18.)) (t 0))
		  (cond ((cdr numeric-arg-info) (lsh 100000 18.)) (t 0))
		  (let ((type (car numeric-arg-info)))
		       (cond ((eq type '&pass) 0)
			   ((eq type '&repeat) (lsh 010000 18.))
			   ((eq type '&ignore) (lsh 020000 18.))
			   ((eq type '&reject) (lsh 030000 18.))
			   (t               ;Unknown type.
			     (lsh 070000 18.))))
		  (cond (prologue-function (lsh 004000 18.)) (t 0))
		  (cond (epilogue-function (lsh 002000 18.)) (t 0))
		  (cond (numeric-function (lsh 001000 18.)) (t 0))
		  (cond (undo-function (lsh 000400 18.)) (t 0))
		  (cond ((eq undo-info '&pass) (lsh 000200 18.)) (t 0))
		  (cond ((eq undo-info '&ignore) (lsh 000100 18.))
		        (t 0))
		  (cond (cleanup-function (lsh 000040 18.)) (t 0)))))
	  ;;
	  ;; Build output structure.
	  ;;
	  (setq result
	        `((putprop ',function-name ,editor-command-value
		         'editor-command)
		. ,result))

	  (and no-break
	       (setq result
		   `((setq nobreak-functions
			 (cons ',function-name nobreak-functions))
		     . ,result)))

	  (setq result
	        `((defun ,function-name
		       ,(mapcar '(lambda (x) (car x)) argument-info)
		       . ,the-function)
		. ,result))

	  (and prologue-function
	       (setq result
		   `((putprop ',function-name ',prologue-function
			    'ed-prologue-function)
		     . ,result)))

	  (and epilogue-function
	       (setq result
		   `((putprop ',function-name ',epilogue-function
			    'ed-epilogue-function)
		     . ,result)))

	  (and cleanup-function
	       (setq result
		   `((putprop ',function-name ',cleanup-function
			    'ed-cleanup-function)
		     . ,result)))

	  (and numeric-function
	       (setq result
		   `((putprop ',function-name ',numeric-function
			    'ed-numeric-function)
		     . ,result)))

	  (and negative-function
	       (cond ((symbolp negative-function)
		    (setq result
			`((putprop ',function-name ',negative-function
				 'ed-negative-function)
			  . ,result)))
		   (t (let ((nf-name
			    (make_atom
			      (catenate function-name
				      "-$-negative-function"))))
			 (setq result
			       `((defun ,nf-name
				      ,(mapcar '(lambda (x) (car x))
					    argument-info)
				      . ,negative-function)
			         (putprop ',function-name
				        ',nf-name
				        'ed-negative-function)
			         . ,result))))))

	  (and undo-function
	       (cond ((symbolp undo-function)
		    (setq result
			`((putprop ',function-name ',undo-function
				 'ed-undo-function)
			  . ,result)))
		   (t (let ((un-name
			    (make_atom
			      (catenate function-name
				      "-$-undo-function"))))
			 (setq result
			       `((defun ,un-name
				      ,(mapcar '(lambda (x) (car x))
					    argument-info)
				      . ,undo-function)
			         (putprop ',function-name
				        ',un-name
				        'ed-undo-function)
			         . ,result))))))

	  (let ((range (cdr numeric-arg-info)))
	       (and range
		  (setq result
		        `((putprop ',function-name ',range
			         'ed-numeric-range)
			. ,result))))

	  (and argument-info
                 (setq result
		   `((putprop ',function-name
			    ',(mapcar '(lambda (x) (cdr x))
				    argument-info)
			    'ed-argument-list)
		     . ,result)))

	  (and encoded-values
	       (setq result (nconc result encoded-values)))

            (and documentation
	       (setq result
		   `((putprop ',function-name ,documentation
			    'documentation)
		     . ,result)))

	  `(progn 'compile . ,result)))

;;; Error reporting functions

(defun dc-error n
       (cond ((= (stringlength (arg 1)) 0)
              (error
	      (apply
	        'catenate
	        (cons "define-command: " (listify (- 1 n))))))
	   (t (error
	        (apply
		'catenate
		(cons (catenate
		        "define-command: In definition of " (arg 1) ". ")
		      (mapcar 'dc-decimal (listify (- 1 n)))))))))

(defun dc-error-duplicate-key (function-name key-name)
       (dc-error function-name
                 "The key " key-name " may only appear once."))


(defun dc-error-conflict (function-name key-1 key-2)
       (dc-error function-name
	       "The keys " key-1 " and " key-2 " are mutually exclusive."))


(defun dc-error-required (function-name given-key missing-key)
       (dc-error function-name
	       "Use of the key " given-key " requires that the key "
	       missing-key " also be specified."))


(defun dc-error-argument n
       (error (apply 'catenate
                     (cons (catenate
		         "define-command: In definition of argument #"
		         (dc-decimal (arg 2)) " of " (arg 1) ". ")
		       (listify (- 2 n))))))


(defun dc-error-argument-duplicate-key (function-name arg-no key-name)
       (dc-error-argument function-name arg-no
                          "The key " key-name " may only appear once."))


(defun dcev-error n
       (cond ((= (arg 2) 0) (apply 'dc-error (listify n)))
             (t (apply 'dc-error-argument (listify n)))))


(defun dc-decimal (x)
       (let ((base 10.) (ibase 10.) (*nopoint t))
	  (maknam (exploden x))))
