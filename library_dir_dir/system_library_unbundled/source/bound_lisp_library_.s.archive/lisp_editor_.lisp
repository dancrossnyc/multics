;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;;; LISP Editor.
;;; Written 74.04.22 by DAM

;;;in this editor, internal variables and functions
;;; have names that start with a tilde (~).
;;; Tilde is defined as a macro character which changes the names to something else.
;;; The one exception is the variable, it, which is the current item.

;;;COMMANDS:
;;;
;;;	Note:  (n),(m) represent decimal numbers.  (S) represents a read-able
;;;	       S-expression, which is evaluated if a colon precedes the command.
;;;
;;; a	go to the car (of the current item).
;;; (n)a 	do a (n) times.  It stops when it hits an atom.
;;; b	go backward from the current item (following print-order).
;;; (n)b	do it (n) times.  It stops when it gets all the way up to the top.
;;; d	go to the cdr.
;;; (n)d	do it (n) times.  It stops when it hits an atom.
;;; f	go forward from the current item (in print-order).
;;; (n)f	do it (n) times.  It stops when it has wrapped around to the beginning.
;;; g	prettily print the current item (Grind).

;;; h	go to the head of the list containing current item.
;;; (n)h	do this (n) times.
;;; i(S)	insert (S) in the list which is the current item, at the front.
;;; j(S)	jump to position labelled (S), saved by rp command.
;;; k	delete the first thing in the current list.
;;; (n)k	delete (n) times.
;;; l	go to the last part of the current list.
;;;	lt prints "(...)"
;;;	lu gets to the last cons in the current list.
;;; m(S)	macro.  apply the function (S) to one argument, the current item.
;;;	it can use the various special variables declared below.
;;; n	repeat the previous search (find Next instance.) Takes numeric arguments like s.
;;;	the search is repeated from the point at which it matched.
;;; p	print the current item.
;;; q	leave the editor.
;;; ra(S)	rplaca the current item to (S).
;;; rd(S)	rplacd the current item to (S).
;;; re(S)	replaces the current item with (S).
;;; ri(S)	"remember it."  setq the variable (S) to the current item.
;;; rp(S)	"remember place."  save the current place, and label it (S).  j gets back there.
;;; s(S)	search for the next instance of (S), looking in print-order.
;;;	thus if not found in current item, can go up.
;;;	match is by equal except ?? inside (S) matches anything.
;;;	~maxlevel controls how deep it can car/cdr before it dies.
;;; 	after finding it, it does h once so you can see the surrounding context.
;;;	the 0s command is useful if you want it to stay at the place where it matched
;;; (n)s(S)  same as s(S) except after finding, it goes u (n) times.
;;; (m),(n)s(S) also binds ~maxlevel to (n).
;;; t	print the current item with limited length and depth.
;;; (n)t	print the current item, limiting length to (n) and depth to default.
;;; (m),(n)t  print the current item, limiting length to (m) and depth to (n).
;;; u	go up from the current item.  (i.e. to that item whose car or
;;;	cdr is the current item.)
;;; (n)u	go up (n) times.  Note that it won't go up past what you started editing.
;;; w	"where."  prints a skeleton of the whole thing being edited,
;;;	leading down to the current item.
;;; x(S)	execute (evaluate) (S) as LISP code.


;;; ENTRIES TO THE EDITOR:
;;;
;;;	(editf fn) - fexpr
;;;	 edits the function fn.  The definition is copied and only
;;;	 updated on the property list after the q command, so it
;;;	 is possible to ctrl/G out.
;;;
;;;	(editp atom property) - fexpr
;;;	 edits the property property of atom.  Same copy-update as editf.
;;;
;;;	(editv atom) - fexpr
;;;	 edits the value of atom.  Same copy-update as editf.
;;;
;;;	(edit S-expr) - expr
;;;	 edits arbitrary data, in place, without copying, and returns it.

;;;for losing systems that don't have strings, make a macro character:

(declare (eval (read)))

;;*eval (read)

(or (memq 'string (status features))
    (setsyntax 42 'macro '(lambda nil
                             (do ((s) (c (tyi) (tyi)))
                                 ((= c 42)
                                  (list 'quote (maknam (nreverse s))))
			 (setq s (cons c s)) ))))





;;; This macro implements the naming convention for internal editor functions and variables.
;;; currently they are on the regular obarray but the names begin and end with a plus sign.

(declare (eval (read)))

(setsyntax '/~ 'macro '(lambda nil (implode (cons '+ (nconc (exploden (read)) '(+))))))

;;; declaration of how the editor's place is remembered.

(declare (special it ~stack))	;it=current item
			;~stack = list of dotted pairs (it.car) or (it.cdr).

;;; routines to move around in the current item.
;;; they return non-nil if they find they can't move.

(defun ~car nil
    (or (atom it)	;if done car'ing, stop and return t
        (progn
	(setq ~stack (cons (cons it 'car) ~stack))
	(setq it (car it))
	nil)))

(defun ~cdr nil
    (or (atom it)
        (progn
	(setq ~stack (cons (cons it 'cdr) ~stack))
	(setq it (cdr it))
	nil)))

(defun ~up nil
    (or (null ~stack)	;if can't go up any fiurther, return t
        (progn
	(setq it (caar ~stack))
	(setq ~stack (cdr ~stack))
	nil)))

(defun ~fwd nil		;go forward, in print-order
    (cond ((not (atom it))	;can go down
	 (~car))
	((~fwd1)) ))	;can't go down, try to go up and forward

(defun ~fwd1 nil
    (cond ((null ~stack))	;can't go up cause there ain't nothing there.
	((eq (cdar ~stack) 'cdr)	;got here by cdr, keep going up.
	 (~up)
	 (~fwd1))
	(t		;got here by car, go up and over.
	 (~up)
	 (and (~cdr)	;go up, try to go over
	      (~fwd1) ))))	;can't, go fwd again. (end of list)

(defun ~back nil		;go backward, in print-order
    (cond ((null ~stack))		;error return, we are all the way back.
	((eq (cdar ~stack) 'car)	;got here by car, back up.
	 (~up))
	(t			;got here by cdr, go back and down.
	 (setq it (caaar ~stack))
	 (rplacd (car ~stack) 'car)
	 nil)))			;success return.

(defun ~listop nil		;get to top (head) of list containing current item.
 (or (~up)		;go up into containing list.
    (cond ((null ~stack) t)	;done if at top. (and can't go farther)
	((eq (cdar ~stack) 'car) nil)	;done if this is a list element, not partial list.
	((~listop)) )))	;this is a partial list, go up and try again.

(defun ~last nil		;get to end of current list.
    (cond ((atom it))	;no conses.
	(t (~cdr)		;not end, take cdr and
	   (~last))))	;loop until end of list.


(declare (special ~arg ~colon))

(defun ~arg (number default)		;get numeric argument
    (do ((number number (1- number))
         (argl (reverse ~arg) (and argl (cdr argl))))
        ((= number 1)
         (cond (argl (car argl))
               (default))) ))

(defun ~read nil			;read S-expression argument.
    (cond (~colon (eval (read)))
	((read)) ))


(declare (special prinlevel prinlength ~prinlevel ~prinlength) (fixnum prinlevel prinlength ~prinlevel ~prinlength (tyi)))
 (setq ~prinlevel 3 ~prinlength 4)

(defun ~print (prinlevel prinlength)
      (cond ((and ~stack (eq (cdar ~stack) 'cdr))		;this is a partial list.
	   (princ "(...")					;print it specially.
	   (~print2)
	   (princ ")"))
	  (t (prin1 it)))	;normal data, just print it.
	)

(defun ~print2 nil		;routine to print interior of partial list.
    ((lambda (prinlevel z)
;	(mapc '(lambda (x) (princ " ") (prin1 x))
;	      it)
	(do x it (cdr x) (atom x)	;mapc questionable because list is dotted
		(princ " ") (prin1 (car x)))
	(cond (z
		(princ " . ")
		(prin1 z)))
	)
     (and prinlevel (1- prinlevel))
     (cond ((atom it) it) ((cdr (last it)))) ))

(defun ~erase macro (x)
    (cond ((memq 'its (status features))
           '(cursorpos 'C))
	(t '(tyo 14))))

(defun ~clear macro (x)
    (cond ((memq 'newio (status features))
	 '(clear-input nil))
	(nil)))

;;;search routine


(declare (special ~search-arg ~search-loc ~maxlevel) (fixnum ~maxlevel))
(setq ~search-loc nil ~search-arg nil)

(declare (eval (read)))
 (or (memq 'string (status features))
     (defun macro stringp (x) nil))	;make dummy stringp if there are no strings.


(defun ~compare (x y level)		;compares two items, using ?? and care with level.
				;x is the pattern, in which ?? may appear.
    (cond ((eq x '/?/?)
	 t)			;?? matches anything - return t.
	((numberp x)		;numbers are compared with equal
	 (equal x y))
	((stringp x)		;so are strings
	 (equal x y))
	((atom x)			;other atoms are compared with eq
	 (eq x y))
	((atom y)			;make sure both are lists
	 nil)			;(atom can't match list)
	((> level ~maxlevel)	;if we are too deep, they can't be equal.
	 nil)
	((and (~compare (car x) (car y) (1+ level))
	      (~compare (cdr x) (cdr y) (1+ level))))))


(defun ~search (x)			;x is item to be searched for.
    (do ((~maxlevel (~arg 2 300.))) nil
     loop
	(and (~fwd) (error "S: fail"))
	(or (~compare x it (length ~stack))
	    (go loop))	;not found, keep searching.
	(setq ~search-loc (cons it ~stack))	;remember for N command.
	(do i (~arg 1 1) (1- i) (< i 1)
	   (~listop))
	))

;;;;command - interface routines

(defun editf fexpr (x)
    (setq x (~editp (car x) '(expr fexpr macro)))
    (cond ((atom x) x)	;won
	('(undefined function)) )) ;lost

(defun editp fexpr (x)
    (~editp (car x) (cadr x)))

(defun ~editp (f p)
    (and (atom p) (setq p (list p)))
    (setq p (getl f p))
    (cond ((null p) '(property not found))
	(t
	  (rplaca (cdr p)
		((lambda (it ~stack) (~edit))
		     (subst nil nil (cadr p))
		     nil))
	  'LISP)))

(defun editv fexpr (x)
    (cond ((not (boundp (setq x (car x))))
	 '(variable is undefined))
	(t (set x ((lambda (it ~stack) (~edit))
			(subst nil nil (eval x)) nil))
	   'LISP)))

(defun edit (x)
    ((lambda (it ~stack) (~edit))
	x nil))

;;;main editor

(declare (*expr sprinter))

(defun ~edit nil		;called with it, ~stack bound to initial values.
  (terpri)
  (do ((x) (save-it) (save-stack) (places) (~colon) (~arg) (cmd)) (nil)	;do forever
    (setq cmd (tyi))
    (and (> cmd 140) (setq cmd (- cmd 40)))	;monocase
		;the following cond, which does it all, is enclosed in an errset.
    (setq save-it it save-stack ~stack)
    (or (errset (progn
    (cond ((= cmd 40))			;ignore space,
	((= cmd 12))			;linefeed,
	((= cmd 15))			;carriage-return,
	((= cmd 14)			;newpage - erase the screen.
	 (~erase))
	((and (> cmd 57) (< cmd 72))		;digit - accumulate number.
	 (or ~arg (setq ~arg (list 0)))
	 (rplaca ~arg (+ (* (car ~arg) 10.) cmd -60))
	 (go numeric-value))
	((= cmd 72)			;colon
	 (setq ~colon t)
	 (go numeric-value))
	((= cmd 54)			;comma
	 (setq ~arg (cons 0 ~arg))		;begin new numeric argument.
	 (go numeric-value))
	((setq x (assoc cmd '((101 . ~car)	;motion commands: a,b,d,f,u.
			  (102 . ~back)
			  (104 . ~cdr)
			  (106 . ~fwd)
			  (110 . ~listop)
			  (125 . ~up)) ))
	 (do i (~arg 1 1) (1- i) (< i 1)
		(and ((cdr x)) (return nil)) ))	;do (n) times, or until it says stop.
	((= cmd 107)			;g command - grind it
	 (terpri)
	 (sprinter it)
	 (terpri))
	((= cmd 111)			;i command - insert in list
	 (or ~stack (error "Can't insert!"))
	 (setq x (cons (~read) it))		;new front of list, will replace 'it'
	 (setq ~search-loc nil)		;clobbering.
	 (and (eq (caaar ~stack) it)
	      (rplaca (caar ~stack) x))
	 (and (eq (cdaar ~stack) it)
	      (rplacd (caar ~stack) x))
		;should patch it to x everywhere it appears in places, but not with subst.
		;cases are:  has become inaccessible, has become inaccessible through different path.
	 (setq it x))
	((= cmd 112)			;j command - today we choose places.
	 (setq x (assq (~read) places))	;find saved place.
	 (or x (error "No RP command was done with that tag."))
	 (setq ~search-loc nil)		;clobbering.
	 (setq it (cadr x) ~stack (cddr x)))
	((= cmd 113)			;k command - delete from list.
	 (or ~stack (error "Can't kill."))
	 (setq ~search-loc nil)		;clobbering.
	 (do ((i (~arg 1 1) (1- i))
	      (x it (cdr x)))
	     ((or (< i 1) (atom x))
	      (and (eq it (caaar ~stack))
		 (rplaca (caar ~stack) x))
	      (and (eq it (cdaar ~stack))
		 (rplacd (caar ~stack) x)) 
	      (setq it x) ))
		;should also patch places as with I command.
	       )
	((= cmd 114)			;l command - last of list.
	 (~last))
	((= cmd 115)			;m command - macro.
	 (funcall (~read) it))
	((= cmd 116)			;n command - repeat search.
	 (or ~search-arg (error "No search to repeat."))
	 (or ~search-loc (princ "Loose Search..."))
	 (and ~search-loc (setq it (car ~search-loc) ~stack (cdr ~search-loc)))
	 (~search ~search-arg))
	((= cmd 120)			;p command - print it.
	 (terpri)
	 (~print nil nil)
	 (terpri))
	((= cmd 121)			;q command - depart.
	 (do () ((~up)))			;go all the way up.
	 (return it))			;and return the result of editing to caller.
	((= cmd 122)			;r commands - 2 letters:
	 (setq cmd (tyi))
	 (and (> cmd 140) (setq cmd (- cmd 40)))	;monocase
	 (cond ((= cmd 101)			;ra command - rplaca
	        (and (atom it) (error "RA: atom?"))
	        (rplaca it (~read)))
	       ((= cmd 104)			;rd command - rplacd
	        (setq ~search-loc nil)		;prob. clobbering
	        (and (atom it) (error "RD: atom?"))
	        (rplacd it (~read)))
	       ((= cmd 105)			;re command - replace it.
	        (or ~stack (error "Can't replace!"))	;LIE.
	        (setq x (~read))
	        (setq ~search-loc nil)	;clobbering.
	        (and (eq it (caaar ~stack))
		   (rplaca (caar ~stack) x))
	        (and (eq it (cdaar ~stack))
		   (rplacd (caar ~stack) x))
	        (setq it x))
	       ((= cmd 111)			;ri command - remember it.
	        (set (~read) it))
	       ((= cmd 120)			;rp command - remember place.
	        (setq places (cons (cons (~read) (cons it ~stack))
			        places)))
	       ((error (maknam (list 122 cmd '/: '/  '/?))))))
	((= cmd 123)			;s command -search
	 (~search (setq ~search-arg (~read))))
	((= cmd 124)			;t command - type out with limitation.
	 (terpri)
	 (~print (~arg 2 ~prinlevel) (~arg 1 ~prinlength))
	 (terpri))
	((= cmd 127)			;w command - tell where we are.
	 (terpri)
	 (~where (reverse ~stack))
	 (terpri))
	((= cmd 130)			;x command - evaluate cruft
					;also treats colon differently.
	 (setq x (eval (read)))
	 (and ~colon (progn (print x)(terpri))))
	((error (maknam (list cmd '/: '/  '/?)))))	;unknown command.  This also ends the errset.
    nil))	;make the cond not be for value and end the progn and the errset.
        ;come here when an error occurred.
        (~clear)				;flush already-typed commands.
        (setq it save-it ~stack save-stack))	;error - restore values
    (setq ~arg nil ~colon nil)			;finished with command, clear arguments for next command.
numeric-value					;go here if numeric-value to be kept.
    ))	;keep on looping in the do.

;;;Routine to Reveal Context - W command.

(defun ~where (st)
    (cond ((null st)			;here we are
	 (~print (~arg 2 ~prinlevel) (~arg 1 ~prinlength)))
	(t		;go down into a list
	  (princ "(")
	  (do ((x nil t) (y nil x) (this (caar st)))
	      ((or (null st) (eq (cdar st) 'car))
	       (and x	;this is not first, put first atom in car, dot dot dot
		  (progn
		    (~atomic-car (car this))
		    (cond (y (princ "..."))	;put three dots if item is not 1st or second
		          ((princ " ")) )))
		;display the item
	       (cond (st		;item is list element.
		    (~where (cdr st))
		    (and (cdaar st) (princ "...")))	;not last in list
		   ((atom it)	;dotted pair - item is cdr of.
		    (princ " . ")
		    (prin1 it))
		   (t		;partial list.
		    (~print2)))
	      (princ ")"))
	   (setq st (cdr st))))))

(defun ~atomic-car (this)
    (cond ((atom this)
	 (prin1 this))
	(t
	 (princ "(")
	 (~atomic-car (car this))
	 (and (cdr this) (princ "..."))
	 (princ ")"))))
