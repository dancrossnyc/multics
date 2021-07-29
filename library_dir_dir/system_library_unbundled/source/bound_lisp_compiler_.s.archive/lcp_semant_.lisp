;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************

;;; HISTORY COMMENTS:
;;;  1) change(2018-07-30,Swenson), approve(2018-07-30,MCR10051),
;;;     audit(2018-08-01,GDixon), install(2018-08-17,MR12.6g-0014):
;;;     Updated (cv-date-) to handle new 4-digit year return value from
;;;     (status date).
;;;                                                      END HISTORY COMMENTS

;;;(c) Copyright 1973, Massachusetts Institute of Technology.
;;;    All rights reserved.
;
; ******************************************************
; ******************************************************
; *****					 *****
; ****          LISP COMPILER - SEMANTICS	 *****
; *****					 *****
; *****    VERSION OF 13 JUNE 1974 - D. A. MOON    *****
; *****   OBSCURELY DERIVED FROM complr.264        *****
; *****					 *****
; ******************************************************
; ******************************************************
;
; Modified April 1981 by Bernard S. Greenberg to use backquote.
; Modified September 1982 by William M. York to fix a bug in compilation of
;	(array ...) forms.
; Modified 29 September 1982 by WMY to fix compilation of all forms of (break ...)
; Modified 5 October 1982 by WMY to fix nested "(progn 'compile ...)" forms
;	to compile code in the correct order (inmost first), and to
;	remove the old three-argument case for (break ...) forms.
; Modified 5 October 1982 by Richard Lamson to add (includef ...)
; Modified 9 October 1982 by Richard Lamson to optimize (nth <constant> ...) 
;				    and (nthcdr <constant> ...) forms.
; Modified 13 October 1982 by Richard Lamson to fix error messages from 
;				     command interface.
;

(cond ((not (boundp 'compiler-revision))
       (princ "lisp_compiler:  Undefined compiler version.")(terpri)
       (quit)))


(global cf cl pause genprefix nfunvars special fixnum flonum fixsw flosw notype arith array* closed muzzled
	unspecial reducible irreducible noargs mapex symbols lisp
	put-in-tree	;request of H. Lieberman
	expr-hash
	system-file
	compile-top-level-forms		;for GSB & BSG 5/4/80
	sobarray cobarray eoc-eval compiler-state compile maklap top-level coutput gofoo ;jonl's crocks for owl
	nocompile
	-db -debug -eval -tm -time -times -ps -pause -pause_at -mc -macros -gp -gnp
	-genprefix -nw -nowarn -tt -total -total_time -list -ls -long -lg
	-all_special -pathname -pn -p -no_compile -ncp
	-ck -check -ioc -messioc -mioc -hd -hold -pedigree -pdg -brief -bf arith
	*expr *fexpr *lexpr **array messioc check debug macros dataerrp barfp
	defpl1 update return ignore fixed bin binary float packed-pointer packed-ptr
	pointer ptr bit aligned unaligned character varying char lisp array
	l le g ge n e)			; special tags used by signp!

(%include backquote)			;BSG 4/22/81

(declare 
        (genprefix !pass1-genfun)
        (special 
	defdat				;used by pass 2
	codelist				;..
	pc				;..
	constant-list			;..
	vgol				;..
	exit				;..
	code-list				;..
	constants-list			;..
	functions-called			;..
	functions-defined			;..
	labels-to-define			;..

	obarray				;declare the 3 obarrays for the function use.
	compiler-obarray			;which allows the user to switch obarrays easily
	initial-readin-obarray		;..
	sobarray
	cobarray
	eoc-eval
	compiler-state
	expr-hash
	system-file

	errset				;need to shut this interrupt off sometimes
	seg-name				;name of object segment as string
	ecrsloss				;assq list of local delarations.
	first-eof				;used to detect missing ")" - see cf and cmp1
	gl				;list of dotted pairs of prog tags and their renamings
	bvars				;bound variables
	locvars				;local variables, dotted pairs with cnt of last reference
	codelist				;list of code put out by pass 2
	condp				;t if in a cond, and, or or
	lmbp				;t if in a lambda
	progp				;t if in a prog
	data				;--random--
	prssl				;sections of prog are seperated by go's and tag's
	effs				;t = for effect, nil = for value
	p1csq				;list of vars setq'ed in cond, and, or or
	p1lsq				;list of vars setq'ed in lambda
	p1psq				;list of vars setq'ed in prog
	mapf				;t if thi s function is the object of a map, causes go and
					;   return to use a farther-out prog according to p1mapcnt
	barfp				;if should break on Compiler Error
	nowarn				;option switch. If t supresses warning messages
	cnt				;basic-block number currently compiling
	pvrl				;prog variables
	p1ll				;lambda list
	lsubrf				;i.o.
	rnl				;dotted pairs of duplicated variables and their gensym renamings
	gofoo				;special mark for prog
	gone2				;list of tags that appear as object of go in a prog
	ffvl				;list of symbols used as "free functional variables."
	pause				;controls breaks between passes
	pause-at				;list of functions to stop during compiling of
	check				;if t, do only pass 1
	dev				;KLUDGE: shared by luz, lsub
	genprefix				;explodec'ed prefix for generated function names
	gfyc				;numeric suffix for generated function names, incremented each time
	map-funarg			;special mark for fcn which is object of map
	map-prog				;special mark for prog generated by map
	map-do				;special mark for do generated by map,  will turn into map-prog
	p1mapcnt				;count of number of map-prog's before first real prog, used to
					;   to know where to return or go to in a map-funarg
	being-compiled			;name of current function being compiled, changed to nil
					;   after it has been typed out so it won't be typed out more
					;   than once.
	current-function			;copy of being-compiled, not set to nil.
	source-map			;list of pathnames, reverse order
	compiler-revision			;mod level of compiler
	compiler-version			;string of compiler version
	time-option			;t if user wants to se e compilation times
	total-time			;t if user wants to see total time & paging used by a compilation
	errflag				;t if errors have occurred in this function - used to suppress pass 2
	args				;set to output of pass 1 when pause option is used
	errsetp				; I don't know.
	errlist				;used to make complr self-starting
	macros				;t if macros option was used, output macro defs to defsubr file
	nocompile				; don't interpret the defun's and defprop's in the file...put it all out.
	special				;setq'ed by loser to t if all variables are to be special
	closed				;t => no in-line arithmetic for plus,...
	noargs				;I don't know
	undfuns				;list of functions referenced but not yet defined
	dataerrp				;break if Error
	barfp				;break if severe Compiler Error
	indiclist				;dotted pairs of expr and subr forms of P-list indicators
	p1cof				;call out flag, t if function with side-effects has been called
	messioc				;(apply 'ioc messioc) is done before printing an error message
	fixsw				;t means take all arithmetic functions as having fixnum arguments.
	flosw				;t means take all arithmetic functions as having flonum arguments.
	fixfns				;assoc list of arithmetic functions and their fixnum forms
	flofns				;assoc list of arithmetic functions and their flonum forms
	nfunvars				;special variable which may be declare setq'ed to t to cause
					;compiler not to allow special variables as free functional vars.
        )
        (*fexpr **array special *reducible)
        (*fexpr fixnum flonum notype array*)
        (*expr finish-code historian init-code-generator pass2 nargs))

;;; Initialize crocks

(setq sobarray (get 'obarray 'array) cobarray (get 'compiler-obarray 'array))
(setq eoc-eval nil compiler-state 'top-level)
(defprop coutput put-in-tree expr)

;;;declarations for compiling number compiler with number compiler.

(declare
   (fixnum (nargs notype) cnt gfyc p1mapcnt i))

(declare (eval (read)))
    (sstatus macro /: (function (lambda nil nil)) splicing)

; listing package

(declare (defpl1 make_lisp_listing "" (char(*)) (char(*)) ) ;args are object seg, option
         (defpl1 absolute_pathname_ "" (char (*)) (return char (168.)) (return fixed bin (35.)))
         (defpl1 absolute_pathname_$add_suffix "" (char (*)) (char (*)) (return char (168.)) (return fixed bin (35)))
         (defpl1 com_err_$one-arg "com_err_" (fixed bin (35.)) (char (*)) (char (*)) (char (*)))
         (defpl1 hcs_$status_minf "" (char (*)) (char (*)) (fixed bin (1))
			       (return fixed bin (2)) (return fixed bin (24.))
			       (return fixed bin (21.))))


;;;get macros used by both parts of compiler

(%include compiler-macros)

(defun logor macro (x) (displace x (cons 'boole (cons 7 (cdr x)))))

;;;get table of functions that have unpredictable side effects

(%include compiler-badfns)

    (defun p1e: macro (x)
	(displace x (list (list 'lambda '(effs) (cons 'p1 (cdr x)))
		        t)))

    (defun p1v: macro (x)
	(displace x (list (list 'lambda '(effs) (cons 'p1 (cdr x)))
		        nil)))

(defun initialize: nil

; stuff having to do with pure and args properties removed.

    (setq errlist '((command-interface)(init1)))
	(sstatus interrupt 0 (function (lambda (args) (iog vt (prin1 current-function) (terpri)))))

    (mapc '(lambda (x) (set x (maknam (exploden x))))
	    '(gofoo map-funarg map-prog map-do))
    (putprop 'do 'doexpander '*macro)
    (putprop 'let 'let-expander '*macro)
    (putprop 'prog1 'prog1-expander '*macro)
    (putprop map-do 'doexpander '*macro)
    (putprop map-prog t '*macro)
    (putprop map-funarg t '*macro)
    (mapc '(lambda (x)(putprop x t '*defpl1-internal))
	'(*pl1call *unmkd-push *unmkd-pop *pack-ptrs *unpack-ptrs *cons-string
		 *rcv-char-* *pl1call-nopop))
    (setq *nopoint nil)
    (setq gfyc 0 messioc '(vt) seg-name nil pause nil pause-at nil infile nil instack nil outfiles nil ^q nil
          genprefix '(/! g) rnl nil barfp nil check nil mapf nil special nil macros nil 
          ffvl nil ecrsloss nil closed nil source-map nil nfunvars nil time-option nil noargs nil total-time nil nocompile nil
	expr-hash nil
	system-file nil
          undfuns nil dataerrp nil nowarn nil
          base 8. ;ibase ibase  
	fixsw nil flosw nil
          indiclist '((expr.subr) (fexpr.fsubr) (lexpr.lsubr)))
     (sstatus feature compiler)
     (sstatus feature fastarith)
     (alloc '(list (60000. nil 0.3)))	;set gc parameters by guess
     (sstatus charmode nil)
     (**array obarray readtable)

	; declare standard reducible functions

     (*reducible
	*	*$	+	+$	-	-$	1+	1+$
	//	//$	\	remainder	gcd	*dif	*quo	quotient
	1-	1-$	<	=	>	CtoI	ItoC	abs
	add1	and	ascii	assoc	assq	atom	boole	caaaar
	caaadr	caaar	caadar	caaddr	caadr	caar	cadaar	cadadr
	cadar	caddar	cadddr	caddr	cadr	car	cdaaar	cdaadr
	cdaar	cdadar	cdaddr	cdadr	cdar	cddaar	cddadr	cddar
	cdddar	cddddr	cdddr	cddr	cdr	catenate	comment	difference
	eq	equal	explodec	exploden	expt	fix	fixp	flatc
	float	floatp	get_pname	greaterp	index	last	length	lessp
	lsh	max	min	minus	minusp	nconc	not	nreverse
	null	numberp	or	plus	plusp	prog2	progn	rot
	signp	stringlength	stringp	sub1	subrp	substr	substr2
	times	typep	zerop	member	memq	ifix	fsc
	first	second	third	fourth	rest1	rest2	rest3	rest4
	nth	nthcdr
     )

	;establish declarations of built-in special variables.

     (mapcar
       '(lambda (x)
	(mapcar
	  '(lambda (y)
	     (and	(memq 'value (status system y))	;if a special variable built in to LISP,
		(putprop y t 'special)))		;then declare it to be special
	  x))
       (makoblist nil))

     t)
 (defun init1: nil (ioc stev) (terpri) (princ "Multics lisp compiler ") nil)

					
(defun compile-fcn: (name flag exp rnl)		;renamed from compile
					;BSG 10/13/80 to keep off user obarray
					;in light of progn 'compile
     (prog (p1mapcnt p1cof locvars cnt lsubrf bvars effs p1ll mapf vgol exit start-runtime
            condp lmbp p1csq p1lsq progp p1psq gone2 defdat fl spfl errflag
            pvrl gl ecrsloss compiler-state)
	      (setq compiler-state 'compile)
                (setq cnt 1 p1mapcnt -1000000000 start-runtime (cond (time-option (runtime)) (t 0)))	;set p1mapcnt funny to avoid hanging go|return's
                (and (setq fl (getl name '(subr fsubr lsubr)))
	           (sysp name)
		(not system-file)
                     (warn name "is a system-defined function - 
        please check over your code for bugs!"))
                (cond ((null (eq (car exp) 'lambda)) (barf exp "is not a function" data))
                      ((and (cadr exp) (atom (cadr exp)))		; atomic lambda list --> lexpr
		    (cond	((not (eq flag 'expr))
			 (warn name "lexpr must be defined as expr property."))
			((setq spfl (getl name '(*expr *fexpr)))
			 (wrntyp name '*lexpr spfl)))
                        (putprop name t '*lexpr)
                        (setq lsubrf t flag 'lexpr)
                        (setq exp (cons (car exp) (cons (list (cadr exp)) (cddr exp))))))
					; ***** makes lsubr have (lambda (nargs) body)
					; ***** make sure that pass2 knows about this
                (cond (lsubrf  ;this is for lexpr's, which have already been done
		     (setq defdat '('lsubr . 777000)))	;777000=any#args allowed.
                      ((greaterp (setq fl (length (cadr exp))) 510.) 
                        (barf name "too many lambda variables" data))
                      ((cond ((eq flag 'expr)
                                (ckargs name fl t)
			   (setq defdat (cons ''subr fl))
                                (setq fl '*expr) 
                                t)
                             ((eq flag 'fexpr) 
			  (setq defdat '('fsubr . 1))
                                (setq fl '*fexpr) 
                                t))
                      (and (setq spfl (getl name '(*expr *lexpr *fexpr)))
                           (not (eq fl (car spfl)))
		       (wrntyp name fl spfl))
                      (putprop name t fl)))
                (setq exp (p1glm (setq p1ll (p1lmbify (cadr exp) name))
			   (p1localdcl p1ll (cadr exp) (cddr exp))
			   name))
                (uuvp p1ll 'p1ll 'lambda)

	      (progn (cond (time-option
		(princ "
Pass 1 time for ")
		(princ name)
		(princ " = ")
		(prin1 (//$ (float (difference (runtime) start-runtime))
		        1e6))
		(terpri)
	       )))

	     ((lambda (args)	;bind special var args to output of pass 1 so user can look at it
	      (and (or pause (memq name pause-at))
		 (apply 'break (list (list'end-of-pass-1-for name) t))))
	     exp)

	    (cond (errflag (err 'nonfatal))	;suppress pass2 if errors occurred
		(check)			;or -check option was specified
		(t
		 (setq codelist
		      (pass2 (cadr exp)	;ll
			   (caddr exp)	;body
			   flag		;type
			   name))		;entry name

	      (and (or pause (memq name pause-at))
		 (apply 'break (list (list 'end-of-pass-2-for name) t)))
		))

                (return name)))

(defun p1: (x)		;x is lisp, return value is semantically-translated version of x.
  (prog (y z tem)

   a
    (cond	((memq (setq z (typep x)) '(fixnum flonum bignum string))	;literal constant - just quote it and return
	 (return `',x))
	((eq z 'random)
	 (warn x "random piece of data - nil substituted")
	 (return ''nil))

	((eq z 'symbol)			;atomic symbol
	 (cond ((memq x '(t nil))		;literal constant - quote it.
	        (return `',x))
	       ((setq z (assq x rnl))		;variable - if renamed, substitute its renaming
	        (setq x (cdr z))
	        (go a))
	       ((p1special x))		;if special, leave it.
	       ((setq z (assq x locvars))	;if local, update cnt of last usage of it.
	        (rplacd z (1+ cnt))) )
	 (setq cnt (1+ cnt))		;update cnt since variable was seen
	 (return x))			;and leave as (possibly renamed) variable.

	;; x is a list - compile it as a function call.

	((eq (setq z (typep (car x))) 'list)	;check the type of the functional
	 (cond				;computed function.
	   ((eq (caar x) 'lambda)
	    (and (cadar x)
	         (atom (cadar x))
	         (barf x "lexpr not allowed here" data))
	    (return (p1lam (car x) (cdr x))) )	;process direct lambda-application.

	   ((eq (caar x) 'label)
	    (putprop (setq y (cadar x)) t 'special)
	    (putprop (setq tem (gen-fcn-name)) t '*expr)
	    (compile-fcn tem 'expr (caddar x) (list (cons y tem)))
	    (return (p1lam (list 'lambda
			     (list y)
			     (cons tem (cdr x)))
		        (list (list 'function tem)) )))

	   ((or (eq (caar x) 'function) (eq (caar x) 'quote))
	    (rplaca x (cadar x))
	    (go a))
	   (t (return (mapcar (function p1v-fcn) x))) ))	;really a computed function, just eval fcn & args

	((not (eq z 'symbol))		;if functional not list, better be atom.
	 (barf x "bad functional form" data))
	)
   b

;;;form with atomic function

    (and (get (car x) '*defpl1-internal)
         (return x))			;leave internal frobbies alone
    (setq z (getl (car x) '(fsubr subr lsubr *fexpr *expr *lexpr *array macro *macro)))
    (cond ((or (null z)		;not yet seen - variable function or implicit *expr dcl
               (and (memq (car z) '(fsubr lsubr subr)) (not (sysp (car x)))))
	 (and (setq z (assq (car x) rnl))
	      (prog2 (setq x (cons (cdr z) (cdr x)))
	             (go b)))
	 (cond				;check for special or bound.
	   ((or (and (not nfunvars)		;if special variables not disallowed,
		   (specialp (car x))	;free functional variable or bound functional variable.
		   (progn
		     (or (memq (car x) ffvl)	;free fcnl varbl - add to list of such
		         (push (car x) ffvl))))
	        (memq (car x) bvars))	;bound fncl varbl.
	    (setq x (cons ((lambda (fn)
			    (or (atom fn)
			        (barf fn "illegal function" data))	;if not checked here, would get compiler error in pass 2
			    fn)
			(p1v (car x)))
		        (cdr x))))
	   (t
	    (ckargs (car x) (length (cdr x)) nil)	;implicit declaration of user function
	    (remprop (car x) '*expr)		;reorder property list so pass 2 doesn't blow out
	    (putprop (car (setq undfuns (cons (car x) undfuns))) t '*expr)))
	 (setq p1cof t)				;calling some random function that may have side-effects.
	 (setq x (cons (car x) (mapcar (function p1v-fcn) (cdr x))))	;compile the arguments
	 )

	((memq (car z) '(macro *macro))		;expand macro and re-process
	 (cond ((not (eq (cadr z) t))			;if really a macro...
	        ((lambda (f)
		       (and (symbolp f)(getl f '(macro *macro))
			  (setq f (p1-chase-linked-macros f)))
		       (setq x (macro-expand x f)))	;expand,
	         (cadr z))
	        (go a))				;and re-process
	       ((eq (car x) map-prog)			;special kludges...
	        (setq x (p1prog (cdr x) (1+ p1mapcnt))))	;(falls through)
	       ((eq (car x) map-funarg)
	        (return ((lambda (mapf)		;mapped function gets compiled with mapf on
			(p1 (cadr x)))
		        t)))
	       ((barf x "bad *macro" barf)) ))

	((memq (car z) '(*expr *lexpr *fexpr *array))	;declared user function
	 (setq p1cof t)				;may have side effects
	 (and (eq (cadr z) 'dcl)
	      (not (eq (car z) '*array))	;if declared but not defined, remember not defined
	      (prog2
		(rplaca (cdr z) 'dcl2)
;		(push (car x) undfuns) ;flushed 4/27/80 -BSG
		0))
	 (cond
	   ((eq (car z) '*fexpr))		;no arg munging for *fexpr's
	   ((eq (car z) '*lexpr)		;*lexpr - just compile args but don't check count
	    (setq x (cons (car x) (mapcar (function p1v-fcn) (cdr x)))))
	   (t				;*expr or *array - check number of args and compile the arguments
	    (and (setq y (nargs (car x)))
	         (not (= y (length (cdr x))))
	         (prog2 (barf x "wrong number of arguments" nonfatal)
	                (return ''nil)))
	    (setq x (cons (car x) (mapcar (function p1v-fcn) (cdr x)))) )))	;compile the arguments.

	((prog2				;system function.
	   (and (memq (car x) (badfns)) (setq p1cof t))	;check for those system functions which can cause random side effects
	   (eq (car z) 'subr))
	 (setq x (p1subr x)))
	((eq (car z) 'fsubr)
	 (setq x (p1fsubr x)))
	((eq (car z) 'lsubr)
	 (setq x (p1lsubr x)))

	((barf x "lost function in p1" barf)))		;should never get here.

;;;end of giant cond for all those cases of forms with atomic functions
;;;now check for reducible functions

    (cond ((null (setq y (get (car x) 'reducible))))
	((eq y 'system)				;if a system reducible function...
	 (or (getl (car x) '(*expr *fexpr *lexpr *array))	;and not redefined...
	       (setq x (p1sysred x (car x)))))		;then let p1sysred transform its constant arguments
	((setq x (p1red x (car x)))))		;or if a user reducible function, p1red does it all.

;;;check for arithmetic functions for which we may to substitute a special fixnum or
;;;flonum function for a more general function, according to its arguments and declarations.


     (and (setq y (get (car x) 'arith))	;if is to always have another function substituted.
	(setq x (cons y (cdr x))))		;then do it.  never have to make any number of args, etc. checks

     (and (not closed)
          (setq z (get (car x) 'arith-subst))	;if general function for which function of particular type might be substitued...
	(cond
	   (fixsw				;if always assume fixnums don't look at args...
	    (and (car z) (setq x (p1arithsubst (car z) (cdr x)))))
	   (flosw				;ditto for case where always assuming flonums
	    (and (cadr z) (setq x (p1arithsubst (cadr z) (cdr x)))))
	   ((not (null (cdr x)))			;make sure no substitute if no arguments.
	    (do ((allfix t) (allflo t) (tem)		;inspect arguments to see if all of same type
		(undcl? nil) (argl (cdr x) (cdr argl)))
	        ((null argl)
	         (cond ((and allfix (car z))	;if all args fixnums and can substitute,
		      (setq x (p1arithsubst (car z) (cdr x))))
	               ((and allflo (cadr z))	;if all args flonums and can substitute
	                (setq x (p1arithsubst (cadr z) (cdr x))))
		     ((not undcl?)			;if mixed types, but all were number-declared...
		      (setq x (p1convert-mixed-to-float x z)))	;so put in conversions to float
		     (t) ))			;otherwise, leave it alone.
	    (cond	((eq 'fixnum (setq tem (p1type (car argl))))
		 (setq allflo nil))
		((eq 'flonum tem)
		 (setq allfix nil))
		((setq allfix nil allflo nil undcl? t)))
		))))	;end all nested stuff up to the and...arith-subst

;;;end of pass1 processing

	(return x) ))


(defun p1arithsubst: (f x)			;performs as directed by arith-subst property.
    (cond	((atom f) (cons f x))		;usual case just substitutes new function name.
	((apply (car f) (list x)))))		;this hack is for zerop - process as a macro.


(defun p1convert-mixed-to-float: (x z)		;put in float on all args, since pass 2 can sort out the
					;unnecessary ones, and substitute in the flonum version of the fcn.
    (cond ((cadr z)				;if can substitute flonum version
	 (p1arithsubst (cadr z) (mapcar '(lambda (y) (list 'float y)) (cdr x))))
	(x)))				;otherwise leave it alone.


;;;function for determining the type of a piece of output from pass 1
;;; returns fixnum, flonum, or nil

(defun p1type: (x)
    (cond (closed nil)		;foo.  could maybe allow some through, but this is OK.
	((atom x)
	 (get x 'number))		;in pass 1 output, all atoms are variables
	((eq (car x) 'quote)	;constant - check its type
	 (cond ((floatp (cadr x)) 'flonum)
	       ((smallnump (cadr x)) 'fixnum)
	       (nil) ))
	((eq (car x) 'progn)
	 (p1type (car (last x))))
	((eq (car x) 'prog2)
	 (p1type (caddr x)))
	((atom (setq x (car x)))	;function call - check name
	 (and (setq x (get x 'numfun)) (car x)))
	((eq 'lambda (car x))	;lambda-expression, type is type of last form in body.
	 (p1type (car (last x)))
		)))	;anything else, return nil

;compile an fsubr

(defun p1fsubr: (x)
	(prog (tem y z fun)
	      (setq fun (car x))
                   (cond ((eq fun 'quote) (return x))
                     ((eq fun 'function)	;functional constant gets compiled
                          (return `',(p1gfy (cadr x))))
		 ((eq fun 'setq) 
		      (setq tem nil)
                          (do zz (cdr x) (cddr zz) (null zz)
                              (and (null (cdr zz)) (return (setq tem nil)))
                              (p1special (setq y (cond  ((setq z (assq (car zz) rnl))
                                                         (cdr z)) 
                                                        ((car zz)))))
                              (setq z (p1v (cadr zz)))
                              (setq cnt (+ 2 cnt))
                              (p1sqv y)
                              (setq tem (cons z (cons y tem))))
                          (and (null tem) (go wna))
                          (return (cons fun (nreverse tem))))
		     ((eq fun 'prog) (return (p1prog (cdr x) 0)))	;non-mapcar prog.
                         ((eq fun 'cond) (return (p1cond (cdr x))))
		     ((memq fun '(catch throw))
		      (setq z (list fun (p1v (cadr x))))
		      (and (cddr x) (rplacd (cdr z) `(',(caddr x))))	;if tag is given, quote it
		      (return z))
		     ((eq fun 'errset)
		      (setq z (list fun (p1v `(list ,(cadr x)))))
		      (and (cddr x)
			 (rplacd (cdr z) (list (p1v (caddr x)))))	;process 2nd arg if given
		      (return z))
                         ((setq tem (assq fun '((function . quote) (*function . *function))))
                          (return (list (cdr tem) (p1gfy (cadr x)))))
                         ((or (setq tem (eq fun 'or)) (eq fun 'and))
                          (cond ((null (cdr x))  (warn x "= vacuous and / or") (return  `',(not tem)))
			  ((null (cddr x)) (return (p1 (cadr x))))
			  ((return (p1and fun (cdr x))))))
                         ((eq fun 'unwind-protect)
		      (return (cons 'unwind-protect (cons (p1 (cadr x))(mapcar 'p1e (cddr x))))))
		     ((eq fun 'go) 
		      (or (setq y (p1sqg))
			(return ''nil))

                          (return (cond ((atom (cadr x)) 
                                         (setq gone2 (cons (cadr x) gone2))
				 (or (setq z (assq (cadr x) gl))
				     (barf (cadr x) "is an undefined go tag" nonfatal)
				     (return ''nil))		;;;gets here since barf returns nil
				 (and (get (setq z (cdr z)) 'defined)		;if tag already defined,...
				      (putprop z t 'back-reference))		;is a backward reference.
				 (list 'go z y))		;subst (gensym) for tag
                                        (t (setq gone2 (cons gofoo gone2))
                                           (list 'go (p1v (cadr x)) y)))))
                         ((eq fun 'signp) 
                          (return (list fun (cadr x) (p1v (caddr x)))))
		     ((eq fun 'store)		; nstore removed, DAM
                          (setq z (p1v (caddr x)))
		      (or (getl (caar x) '(fsubr subr lsubr *fexpr *expr *lexpr *array))
			(putprop (caar x) 'dcl '*array))	;implicit declaration
                          (return (list fun (p1v (cadr x)) z)))
                         ((eq fun 'array)
		      (return (p1 `(*array ',(cadr x) ',(caddr x) .,(cdddr x)))))
		     ((eq fun 'iog) (return (list 'iog (cadr x) (p1 (cons 'progn (cddr x))) )))
		     ((eq fun 'ioc) (return (p1ioc x)))
                         ((eq fun 'err)
                          (return (cond ((null (cdr x)) '(err 'nil))
				((cddr x) (barf x "err with 2 args cannot be compiled -- you lose" data))
				((list 'err (p1v (cadr x)))) )))

		     ((memq fun '(status sstatus)) (return (p1sts x)))

		     ((eq fun 'break)	; this code -WMY 9/29/82
		      (and (null (cddr x))	; handle (break foo) case
			 (return x))
		      (and (not (null (cdddr x)))
			 (barf x "wrong number of arguments." nonfatal))
		      ; now (break foo <form>) case
		      (return (p1 `(and ,(caddr x)
				    (break ,(cadr x))))))

		     ((memq fun '(subrcall lsubrcall arraycall))
		      (return (cons fun		;call fcn
				(cons (cadr x)	;type
				      ((lambda (effs)
					(mapcar 'p1 (cddr x)))
				       nil) ))))
		     ((eq fun 'declare)
			(barf x "local declaration not at beginning of body - ignored" nonfatal)
			(return x))		;let pass 2 discard it. (?)
		     ((eq fun 'eval-when)
		      (barf x "eval-when not allowed in function bodies - forms ignored" nonfatal)
		      (return ''nil))
		     (t (return x)))		;random fsubrs simply return their form

wna	(barf x "wrong number of arguments." nonfatal)
	(return ''nil)))


;compile an lsubr

(defun p1lsubr: (x)
       (prog (tem z dummy fun)
	   (setq fun (car x))
	   (cond ((eq fun 'list) 
		(or (cdr x) (return ''nil))	; list with no args
		(or (cddr x) (return (list 'ncons (p1v (cadr x)))))    ; list with 1 arg
							)	; list with more args leave alone.
								; turning into bunch of conses is
								; a loser on a machine with only
								; one accumulator.
	         ((eq fun 'list*)
		(or (cdr x)(go wna))				;0 args not legal
		(or (cddr x)(return (p1v (cadr x))))			;(list* x) => x
		(or (cdddr x)(return (p1 (cons 'cons (cdr x)))))
		(setq z (mapcar 'p1v-fcn (cdr x)))			;Compile up all the args.
		(setq x (car (last z)))				;Look at last compiled arg...
		(cond ((atom x))					;Variable, fall thru.
		      ((and (eq (setq tem (car x)) 'quote)(eq (cadr x) 'nil))    ;list* nil stupid turkey or macro
		       (return (cons 'list (nreverse (cdr (nreverse z))))))
		      ((getl tem '(*lexpr *fexpr *expr)))		;Fall thru on redefines.
		      ((memq tem '(ncons list))
		       (return (cons 'list (nconc (nreverse (cdr (nreverse z)))(cdr x)))))
		      ((memq tem '(list* cons))
		       (return (cons 'list* (nconc (nreverse (cdr (nreverse z)))(cdr x))))))
		(return (cons 'list* z)))
	         ((eq fun 'boole) (setq x (p1boole x))))		;transform to just and, or, xor
	   (setq fun (car x))		;new ball game now.
	   (cond ((eq fun 'prog2)
		       (and (or (null (cdr x)) (null (cddr x)))
			  (go wna))
                           (setq z (cons (p1e (cadr x))
                                         (cons (p1 (caddr x))
                                               (mapcar (function p1e-fcn) (cdddr x)))))
                           (return (cons fun z)))
		      ((eq fun 'progn)
		       (return (cons 'progn
			       (maplist '(lambda (x)
					(cond ((null (cdr x))	;if last clause.
					       (p1 (car x)))
					      ((p1e (car x))) ))	;if not last, don't need value
				(cdr x)) )))
;;;                         special hack for 
;;;                             (eval (cons 'fsubr list)) or for 
;;;                            (eval (list 'fsubr foo))
;;;             both =>         (apply 'fsubr list)
                         ((eq fun 'eval)
		      (cond ((cddr x)
			   (warn x "may not work")
			   (return (list 'eval (p1v (cadr x)) (p1v (caddr x)) ))))
                            (setq z (p1v (cadr x)))
                            (cond ((and (not (atom z)) 
                                        (eq (car z) 'cons)
                                        (setq tem (p1f (cadr z) (caddr z))))
                                   (return tem))
			    ((and (not (atom z))
			          (eq (car z) 'list)
				;;This kludge upon a kludge accounts
				;;for the fact that all list/cons coalescing has already been
				;; done, and a list of 0 or 1 element output by p1
				;;just isnt supposed to occur.
				(let ((zl (length (cddr z))))
				     (let ((p1farg (cond ((= zl 0) nil)
						     ((= zl 1)(cons 'ncons (cddr z)))
						     (t (cons 'list (cddr z))))))
					(setq tem (p1f (cadr z) p1farg)))))
			     (return tem))
			    (t (return (list 'eval z)))))
		     ((eq fun 'apply) 
		      (cond ((cdddr x)
			   (warn x "may not work")
			   (return (list 'apply (p1fcnarg (cadr x)) (p1v (caddr x)) (p1v (cadddr x)) ))))
		      (setq tem (p1v (cadr x)))		;compile and investigate first arg

		      ;; now we check for the special case of applying a function
		      ;; which is a subr of known number of arguments or a lambda
		      ;; with a tell-tale argument list.

		      (cond ((and (not (atom tem))
			        (eq (car tem) 'quote)
			        (or (and (not (atom (setq z (cadr tem))))	; check for lambda form, non-lexpr
				       (eq (car z) 'lambda)
				       (or (null (setq z (cadr z)))	; arglist must not be atom then!
					 (not (atom z)))
				       (setq z (length z)))	; get number of arguments
				  (and (atom z)			; check for subr
				       (getl z '(subr expr *expr))
				       (setq z (nargs z)))))		; see if nargs known

			      (return (p1 (cond ((= z 0)	;special case for no arguments
					     (list 'progn
						 (caddr x)	;must eval arglist, surely will be nil.
						 (cdr tem)))	;then call fcn with no args.
					    (t		;regular case, eval arg list + spread args + call
					     (list (list 'lambda
						       (ncons (setq dummy (gensym)))
						       (cons (cadr tem) (p1spread dummy z)))
						 (caddr x))) )))  ))
		      (return (list 'apply (p1fcnarg0 tem) (p1v (caddr x)))))

		     ((setq z (assq fun '((mapcan mapcon car)
					(mapcon mapcon list)
					(map map list)
					(mapc map car)
					(mapcar maplist car)
					(maplist maplist list))))

		      (return (p1 (p1map z (cadr x) (cddr x)))))


		     ((eq fun 'mapatoms)
			(setq tem (cond ((cddr x) (caddr x)) (t 'obarray)))
			(setq dummy (gensym))
			(setq x (cadr x)) ; get function
			(cond ((and (not (atom x)) (memq (car x) '(quote function)))
				(return (p1 (subst (gensym) 'obary (subst dummy 'p (subst (cadr x) 'f
					(list '(lambda (obary)
						(do p 0 (1+ p) (= p 511.)
						 (declare (fixnum p))
						 (mapc (function f) (arraycall t obary p)))
						(do p 511. (1+ p) (= p (+ 511. 128.))
						 (declare (fixnum p))
						 ((lambda (obary) (and obary (f obary))) (arraycall t obary p)))
						t)
					     tem)))))))
				(t (return (p1 (subst (gensym) 'obary (subst dummy 'p (subst (gensym) 'f
					(list '(lambda (obary f)
						(do p 0 (1+ p) (= p 511.) (declare (fixnum p))
						 (mapc f (arraycall t obary p)))
						(do p 511. (1+ p) (= p (+ 511. 128.)) (declare (fixnum p))
						 ((lambda (obary) (and obary (funcall f obary))) (arraycall t obary p))))
					      tem x)))))))))
		     ((and (eq fun 'eoffn) (cddr x))	; eoffn of two args.
			(return (list 'eoffn (p1v (cadr x)) (p1fcnarg (caddr x)))))

			; random lsubr
                         (t (and (setq z (args fun))
                                 (numberp (car z))
                                 (setq tem (length (cdr x)))
			   (or (< tem (car z)) (> tem (cdr z)))
                                 (go wna))
		        (return (cons fun (mapcar 'p1v-fcn (cdr x))))))	; do p1 to args of lsubr

wna	(barf x "wrong number of arguments." nonfatal)
	(return ''nil)))


;compile a subr

(defun p1subr: (x)
	(prog (y z)
                   (and (setq y (nargs (car x))) 
                        (not (= y (length (cdr x))))
		    (go wna))
	         (cond ((setq z (assq (car x) '((sassoc . assoc)(sassq . assq))))
		      (cond ((memq (car (cadddr x)) '(quote function))	;check for (sassoc x y 'f)
			   (return (p1 (list 'cond		;change to:
				       (list (list (cdr z) (cadr x) (caddr x)))  ; (cond ((assoc x y))
				       (list (list 'apply (cadddr x) nil)))	;((apply 'f nil)))
				)))
			  (t
			   (setq x (list (list 'lambda (list (setq y (gensym)))	;temp to eval 3rd arg
					(list 'cond
					   (list (list (cdr z)
						     (cadr x)
						     (list 'prog2 nil
							        (caddr x)
							        (list 'setq y (cadddr x)) )))
					   (list (list 'apply y nil)) ))
					nil))
			   (return (p1 x)))) ))
                   (cond ((eq (car x) 'not) (setq x (cons 'null (cdr x))))
			;; change 1+ and 1- to forms more suitable for optimization
		     ((eq (car x) '1+) (return (p1 (cons '+ (cons '1 (cdr x))))))
		     ((eq (car x) '1-) (return (p1 (list '- (cadr x) 1))))
                         ((memq (car x) '(member assoc  delete equal memq  assq))
                          (setq z (mapcar (function p1v-fcn) (cdr x)))
                          (cond ((eq (car x) 'equal)
			   (cond ((or (p1type (car z)) (p1type (cadr z)))	; if either arg has a numeric type...

				(return (cons 'eq z)))
			         ((return (cons 'equal z))) )))
                          (and  (memq (car x) '(member assoc delete))
                                (or (p1eqqte (car z))
                                    (and (not (atom (cadr z)))
				 (eq (caadr z) 'quote)
                                         (not (eq (car x) 'delete))
                                         (not (do y (cadadr z) (cdr y) (null y)
                                                  (and (not (pnamep (cond ((eq (car x) 'member)
                                                                         (car y))
                                                                        ((caar y)))))
                                                        (return t))))))
                                (setq x (cons (cdr (assq (car x) '((member . memq)
                                                                   (assoc . assq)
                                                                   (delete . delq))))
                                              (cdr x))))
                          (and  effs
                                (eq (car x) 'memq)
			  (not (atom (cadr z)))
			  (eq (car (cadr z)) 'quote)
			  (setq y (cadr z))
                                (< (length (cadr y)) (memq-max))	; it is a heuristic that memq of > memq-max things
							; isn't worth changing to eq's.
                                (atom (setq data (car z)))
                                (setq z (cons 'or 
                                              (mapcar 
                                                (function 
                                                  (lambda (x) 
                                                        (list 'eq data (list 'quote x))))
                                                (cadr y))))
                                (setq cnt (1- cnt))
                                (return (p1 z)))
		      (and (eq (car x) 'assq)
			 (not (atom (cadr z)))
			 (eq (car (cadr z)) 'quote)
			 (setq y (cadr z))
			 (< (length (cadr y))(assq-max))
			 (atom (setq data (car z)))
			 (setq z (cons 'cond
				     (mapcar
					(function (lambda (x)
						   (list (list 'eq data (list 'quote (car x)))  
							(list 'quote x))))
					(cadr y))))
			 (setq cnt (1- cnt))
			 (return (p1 z)))
		      (return (cons (car x) z)))


		     ((memq (car x) '(sort sortcar))		; sorts take function arg.
			(return (list (car x) (p1v (cadr x)) (p1fcnarg (caddr x)))))
		     ((and (eq (car x) 'princ)
			 (stringp (cadr x))
			 (= (stringlength (cadr x)) 1)	;optimize princ of short string into a tyo
			 (return (list 'tyo
				     (list 'quote (CtoI (cadr x)))
				)) ))

                         ((or (and  (eq (car x) 'set) 		; this is a pretty useless optimization.
                                (not (atom (cadr x))) 		
                                (eq (caadr x) 'quote)
                                (setq x (cons 'setq (cons (cadadr x) (cddr x))))
			 (return (p1 x)))
			(setq p1cof t))	; don't know what will be setq'ed...
                         ))
                   (cond ((eq (car x) 'null) 
                          (cond ((and (null effs) 
                                      (bool1able (cadr x)) 
			        (not (prog (y)			; don't allow memq as bool1able here
					(setq y (cadr x))
				     p    (and (eq (car y) 'memq) (return t))
					(and (eq (car y) 'progn) (setq y (car (last y))) (go p))
					(and (eq (car y) 'prog2) (setq y (cadr y)) (go p)))))
			   (setq z (function p1e-fcn)))
			  (t (setq z (function p1v-fcn))))
		     (setq z (list (apply z (list (cadr x)))))
		     (return (cons (car x) z)))
                        ((eq (car x) 'expt)
                         (return (xcons (list (setq y (p1v (cadr x)))
                                              (p1v (caddr x)))
                                        (cond ((eq (p1type y) 'flonum)
                                               'expt$)
                                              (t 'expt)))))
                        ((eq (car x) 'return) 
		     (or (setq y (p1sqg))
		         (return ''nil))
		     (return (list 'return (p1v (cadr x)) y))))
	         (cond ((eq (car x) 'cons)			;Try nifty cons condensations
		      (let ((opd1 (p1v (cadr x)))		;Compile and condense first.
			  (opd2 (p1v (caddr x))))		;Second operand is good one...
			 (cond ((atom opd2))		;Variables, fall thru
			       ((and (eq (setq x (car opd2)) 'quote)(eq (cadr opd2) 'nil))
			        (return (list 'ncons opd1)))	;cons (1 arg) => ncons
			       ((getl x '(*expr *fexpr *lexpr)))     ;Fall thru for *foo..
			       ((memq x '(cons list*))
			        (return (cons 'list* (cons opd1 (cdr opd2)))))
			       ((memq x '(list ncons))
			        (return (cons 'list (cons opd1 (cdr opd2))))))
			 (return (list 'cons opd1 opd2)))))
	         (cond ((memq (car x) '(< = >))
		      (return (p1generic x (mapcar 'p1v-fcn (cdr x))))))
	       (return (cons (car x) (mapcar 'p1v-fcn (cdr x))))		;random subr - just eval all args

wna	(barf x "wrong number of arguments." nonfatal)
	(return ''nil)))

;;; Generic hack   BSG 8/6/80

(defun p1generic (form cmpargs)		;args already p1'ed
       (do ((known-type nil)
	  (fn (car form))
	  (fargs cmpargs (cdr fargs)))
	 ((null fargs)(cons fn cmpargs))

	 (let ((arg (car fargs)))
	      (let ((type (cond  ((and (not (atom arg))(eq (car arg) 'quote))
			      (typep (cadr arg)))
			     (t (p1type arg)))))
		 (cond ((not (memq type '(fixnum flonum nil)))
		        (barf (cond ((and (not (atom arg))(eq (car arg) 'quote))
				 (or (cadr arg) (maknam '(n i l))))
				(t arg))
			    (catenate "Invalid operand for " fn)
			    data))
		       ((eq known-type type))
		       ((null type))
		       ((null known-type)(setq known-type type))
		       (t 
		         (barf form "Inconsistent numeric argument types" nonfatal)
		         (return (cons (cdr (assoc fn '((> . greaterp)(< . lessp)(= . equal)))) cmpargs))))))))


(defun p1and: (f x)			;f is and  or  or
				;x is list of args, at least 2
				;generates e.g. (and (setq-list) call-out-flag transformed-body)
     ((lambda (p1vars p1cnt)
        ((lambda (condp p1csq lmbp p1lsq p1cof)
	(setq x (mapcar (function p1aoc) x))	; transform the body
	(setq x (nconc (list f p1csq p1cof) x)) ; make return value
	)
        t nil nil nil nil)
     (p1bug p1cnt p1vars)
     (p1setqex (cdr x))
     x)
   locvars cnt))

(defun p1bug: (p1cnt p1vars)		;See if pass 2 really needs this.
        (setq cnt (1+ cnt))
        (do x p1vars (cdr x) (null x)
            (cond ((greaterp (cdar x) p1cnt) (rplacd (car x) cnt))))
        (setq cnt (1+ cnt)))

(defun p1aoc: (j)
;$   (comment compile a piece in an and-or clause, or the first part of a cond clause)
    (cond ((bool1able j) (p1 j))
          ((p1v j))))

(defun p1cond: (x)		;x is list of cond clauses
				;produces
				;   (cond (setq-list) call-out-flag ((a b) c) (d))
				;from
				;   (cond ((a b) c) (t d))
   (cond ((null x)
          (warn nil "vacuous cond - nil generated")
	''nil)
         (((lambda (p1vars p1cnt)
	  ((lambda (condp p1csq lmbp p1lsq p1cof z y)

		; make reversed list of processed cond clauses in z.

	     (do x x (cdr x) (null x)
		(setq y (p1aoc (caar x)))		;compile the antecedent
		(cond ((and (not (atom y))		;if the antecedent is a constant,
		 	  (eq (car y) 'quote))
		       (and (equal y ''nil) (go next-clause))	;if nil, delete this clause
		       (cond ((cdar x)
			    (setq y (cons 'progn (cdar x)))	;new improved antecedent
			    (setq y (list (p1aoc y))) ) ;compile the antecedent,listify it.
			   ((setq y (list y))))
						;if (t (x) (y)) change to((progn (x) (y)))
						;but leave a clause (t) the way it is.
		       (setq x '(fin)))		;and this is the last clause of this cond
						;that can be reached, so delete the rest.
		      (t (setq y (cons y (cdar x)))))

		;at this point y is the cond clause, with compiled antecedent

		(push (cond ((null (cdr y)) y)
			  (t			;if consequents, compile them
			   (list   (car y)
				 (p1 (cond ((cddr y)	;make sure only one consequent
					  (cons 'progn (cdr y)))
					 ((cadr y)) ))) ))
		      z)
	next-clause
		)
	     (setq x (nconc (list 'cond p1csq p1cof)
			(nreverse (or z '(('nil)))))))	;make return value
						; ensuring there is at least one clause!
	   t nil nil nil nil nil nil)
	(p1bug p1cnt p1vars)
	(p1setqex (cdr x))
	x)
       locvars cnt))
))





(defun p1boole: (x)			;boole transformer.  x is a form whose car is boole.
				;puts everything in terms of and, ior, xor.  I.e. boole 1, 6, or
				;  7, or else progn or prog2 for the trivial cases.
    (prog (y)			;*** only works if boole has 3 or more args.
	(cond ((and (cdr x) (cddr x) (cdddr x)))
	      (t (barf x "boole with less than 3 args." nonfatal)  (go lose)) )
	(or (numberp (setq y (cadr x)))
	    (return x))	;variable boole - leave it alone
	(setq y (assq y '(	;get characteristics of this boole from first arg, which must be fixnum.
			(0 con 0)
			(1)
			(2 ca 1)
			(3 spm)
			(4 cm 1)
			(5 spa)
			(6)
			(7)
			(10 cr 7)
			(11 cr 6)
			(12 ca 3)
			(13 ca 7)
			(14 cr 5)
			(15 cm 7)
			(16 cr 1)
			(17 con -1) )))
	(and (null y) (barf x "first arg to boole must be a fixnum between 0 and 17" nonfatal)  (go lose))
	(or (setq y (cdr y))
	    (return x))		;if an elementary boole (1, 6, or 7), just leave it.
	(cond ((eq (car y) 'cr)	;complement result:
	       (return (list 'boole
			 6	; by xoring with -1
			 (p1boole (cons 'boole
				       (cons (cadr y)
					   (cddr x) )))
			 -1)))
	      ((eq (car y) 'con)	;result is a constant, so make progn to eval args then return constant value
	       (return (cons 'progn (append (cddr x) (list (cadr y))) )))
	      ((eq (car y) 'spm)	;result is last arg
	       (return (cons 'progn (cddr x))))
	      ((eq (car y) 'spa)	;result is 2nd arg
	       (return (cons 'prog2
			 (cons nil
			       (cddr x)))))
	      ((eq (car y) 'ca)	;complement "accumulator"
	       (return (prog (z zz)
		(setq z (caddr x)	;2nd arg
		      zz (cdddr x))	;rest of args

	loop	(setq z (list 'boole	;complement result so far (z)
			    (cadr y)	;and apply boole to next arg
			    (list 'boole 6 z -1)
			    (car zz) ))
		(cond ((setq zz (cdr zz))
		       (go loop))
		      ((return z)) ) )))
	      ((eq (car y) 'cm)	;complement "memory": all opnds except 2nd arg
	       (return (cons 'boole
			 (cons (cadr y)
			       (cons (caddr x)
				   (mapcar '(lambda (z)
					     (list 'boole 6 z -1))
					(cdddr x)  ))))) ))
lose	(return ''nil)))

;;;"number compiler" stuff.

;;;set up properties giving relations
;;;of the various arithmetic functions.

(mapcar '(lambda (x) (putprop (car x) (cdr x) 'arith-subst) (car x))
    '(
	(abs absfix/! abs$)			; special abs substitution
	(times * *$)
	(plus + +$)
	(difference - -$)
	(minus - -$)		;works since number of args has been checked
	(quotient // //$)
	(add1 1+ 1+$)
	(sub1 1- 1-$)
	(lessp (p1lesspcheck) (p1lesspcheck))
	(greaterp (p1greaterpcheck) (p1greaterpcheck))
;	(zerop (p1zeropfix) (p1zeropflo))	; - removed because pass 2 wins on this now
	(gcd \\ nil)
	(remainder \ nil)))

;;;functions for macroly expanding zerop, lessp, greaterp into =, <, or >

(defun p1lesspcheck: (x)
    (cond	((null (cddr x))		;only two args, can substitute.
	 (list '< (car x) (cadr x)))
	((cons 'lessp x)) ))	;more than two.  Oh, well...

(defun p1greaterpcheck: (x)
    (cond	((null (cddr x))		;only two args, can substitute.
	 (list '> (car x) (cadr x)))
	((cons 'greaterp x)) ))	;more than two.  Oh, well...

;;;The properties on atoms derived from declarations are as follows:
;;;	args	as in the args function, (m.n) or (nil.n)
;;;	number	declares numeric variable.  property is
;;;		passed through renaming.  value is fixnum, flonum,
;;;		or nil.
;;;	numfun	declares numeric function.  value is list whose car
;;;		is type of result returned by function (fixnum, flonum, or nil),
;;;		and whose cdr is list of argument types which is not
;;;		used right now but I suppose might be someday.
;;;	arith	arith declaration has been used.  value of property
;;;		is function which should be substituted for this one.
;;;	arith-subst  property placed on system functions which can get special functions
;;;		substituted when arguments are all fixnums or all flonums - value of property is
;;;		list of fixnum function and flonum function, with nil where there is not one of the choices

;;;Note:  array* arrays have both a *array property and a numfun property.

(defun p1red: (x f)		;reduce expression x with reducible function f

  (prog nil
    (mapc '(lambda (y)			;make sure all arguments are constants
					;Note:  this isn't quite right for reducible *fexpr's, but who cares.
		(and (or (atom y)
		         (not (eq (car y) 'quote)))
		     (return (cons f (cdr x)))))	;non-constant arg seen - leave form alone.
	(cdr x))
    (return (p1ctevq x (cons f (cdr x)))) ))	;all args constant - evaluate at compile time.


(defun p1sysred: (x f)	;reduce expression x with reducible system function f.

    (prog (y const nonconst firstconst boolectl)

	(cond ((memq f '(+ +$ plus * *$ times))		;commutative.
	       (return (p1redcomm f (cdr x) x)))

	      ((memq f '(- -$ difference *dif *quo quotient // //$))
						;commutative except for first arg.
						;and other funnies
	       (and (null (cdr x))			; if no args, generate constant result.
		  (return (p1ctevq x x)))
	       (and (not (atom (cadr x)))
		  (eq (car (cadr x)) 'quote)
		  (setq firstconst (cadr x)))
	       (cond ((null (cddr x))			;if only one arg, is a funny.
		    (cond	((null firstconst)		;if arg not constant, can't reduce
			 (return x))
			((and (zerop (cadr firstconst)) ;be sure not to divide by zero at compile time
			      (memq f '(// //$ quotient)))
			 (return x))		;but do it at run time.
			((return (p1ctevq x (list f firstconst))))) ))
	       (setq y (p1redcomm (cdr (assq f '((- . +)		;reduce 2nd - nth args
					 (-$ . +$)
					 (difference . plus)
					 (*dif . plus)
					 (quotient . times)
					 (*quo . times)
					 (// . *)	;you might not think this one works, but in fact it does.
					 (//$ . *$)) )) 
			      (cddr x)
			      x))
	       (setq boolectl (and (not (atom y)) (eq 'quote (car y))))	;t if y is a constant
	       (cond ((and (null boolectl) (null firstconst))
		    (return (cons (car x) (cons (cadr x) (cdr y)))))	;can't do anything.
		   ((and boolectl firstconst)		;if completely constant
		    (and	(zerop (cadr y))		;special check for division by zero.
			(memq f '(// //$ quotient *quo))
			(return (list (car x) firstconst y)))	;not a complete constant
		    (return (p1ctevq x (list f firstconst y))))
		   (boolectl			;if constant except for 1st, e.g. (- x '4)
		    (return (list (car x) (cadr x) y)))
		   ((and (cdr y)			;1st const. some others const?
		         (not (atom (cadr y)))
		         (eq (caadr y) 'quote)	;yes
		         (not (and  (zerop (cadadr y))	;be sure not to divide by zero at compile time.
				(memq f '(// //$ quotient *quo)) )))
		    (return (cons (car x)
			        (cons (p1ctevq x (list f firstconst (cadr y)))
				    (cddr y)))))	;combine the constants.

		   (t				;otherwise, have only first constant so leave it.
		     (return (cons (car x)
			         (cons firstconst
				     (cdr y))))) ))

	      ((eq f 'equal)	;if one arg is nonbignum atomic constant, change to eq
	       (and (not (atom (cadr x))) (eq (car (cadr x)) 'quote) (setq firstconst (cadr x)))
	       (and (not (atom (caddr x))) (eq (car (caddr x)) 'quote) (setq const (caddr x)))
	       (and
		firstconst
		(atom (cadr firstconst))
		(not (bigp (cadr firstconst)))
		(not (stringp (cadr firstconst)))	;strings are not really atomic
		(return (p1red x 'eq)) )
	       (and
		const
		(atom (cadr const))
		(not (bigp (cadr const)))
		(not (stringp (cadr const)))		;strings are not really atomic
		(return (p1red x 'eq)) )
	       (return x))			;can't optimize, leave the way it is.

	      ((and (eq f 'ascii)
		  (not (atom (setq y (cadr x))))
		  (eq (car (cadr x)) 'quote))
	       (return (p1ctevq x (list f y)) ))

	      ((eq f 'boole)		;Note:  boole has already been transformed to just and, or, and xor
	       (or (and (not (atom (cadr x)))		;make sure type of boole is constant fixnum.
		      (eq (car (cadr x)) 'quote)
		      (smallnump (cadr (setq boolectl (cadr x)))))
		 (return x))		;not constant type of boole, abandon attempt to optimize
	       (do z (cddr x) (cdr z) (null z)	;look at 2nd and following args
		(cond ((and (not (atom (car z)))
			  (eq (car (car z)) 'quote))
		       (push (car z) const))
		      ((push (car z) nonconst)) ))
	       (and const			;coalesce the constant arguments
		(cdr const)		;if more than 1
		       (setq const (list (p1ctevq
					x
					(nconc (list 'boole boolectl)
					       const)))))
	       (cond (nonconst			;if not all args were constant,
			(return (nconc (list 'boole boolectl)
				     const
				     (nreverse nonconst))) )
		   ((return (car const))))	;if all args constant, return quoted value
		)

	      ((memq f '(nth nthcdr))
	       (and (not (atom (cadr x)))	; look at count
		  (eq (car (cadr x)) 'quote)   ; it's a constant.
		  (smallnump (cadr (cadr x)))  ; it's a fixnum.
		  (< (cadr (cadr x)) 20.)     ; it's small enough.
		  (return (p1nthred f (cadr (cadr x)) (caddr x) x)))
	       (return x))		; can't do anything with it.

	       ((memq f '(\ remainder))		;watch out for division by zero
	        (and (not (atom (caddr x)))		;look at divisor
		   (eq (car (setq const (caddr x))) 'quote))
	        (and const (zerop (cadr const)) (return x))  ;don't ctev if divisor is 0.
	        (return (p1red x f)) )

	       ((return (p1red x f))) )  ;not a special case, just do it.

	(return x) ))		;if fall through from a clause of the above cond, couldn't reduce, so return argument



;;;reduce a commutative function
;;;args == function, arglist, form for p1ctev to use in error message
;;;if all args constant, returns quoted value
;;;otherwise, returns expression with constants combined and moved up to the front.

(defun p1redcomm: (f argl errform) 
       (prog (const nonconst) 
	   (mapc '(lambda (x) 
		        (cond ((atom x) (push x nonconst))	 ;atom must be var
			    ((eq (car x) 'quote)
			     (push x const))		 ;definite constant
			    ((and (eq (car x) f)		 ;internal call same f
				(not (atom (cdr x)))
				(not (atom (cadr x)))
				(eq (caadr x) 'quote)	 ;internal constant can be cumulated
				(push (cadr x) const)
				(push (cons f (cddr x))
				      nonconst)))
			    (t (push x nonconst))))
	         argl)
	   ;; evaluate cumulated constant
	   (and const
	        (setq const (p1ctevq errform (cons f const)) 
		    argl (cadr const)))
	   (and const					 ;get rid of identities
	        (cond ((or (and (memq f '(+ +$))
			    (zerop argl))
		         (and (eq f 'plus) (eq argl 0)) ; only fixnum zero is identity
		         (and (eq f '*) (= argl 1.))
		         (and (eq f '*$) (= argl 1.0)))
		     (setq const nil))))
	   (return (cond ((null nonconst) const)
			 ((null const) (cons f (nreverse nonconst)))
		         	(t (cons f (cons const (nreverse nonconst))))))))

(defun p1nthred: (fun count form errform)
       (cond ((eq (car form) 'quote)
	    (p1ctevq `(,fun ,count ,form) errform))
	   ((= count 0)
	    (cond ((eq fun 'nth) `(car ,form))
		(t form)))
	   ((eq fun 'nth)
	    (nth-cdr-ing count 'car form))
	   (t
	    (nth-cdr-ing (1- count) 'cdr form))))

(defun nth-cdr-ing (n last-op operand)
       (do ((i n (1- i))
	  (result operand `(cdr ,result)))
	 ((< i 1) `(,last-op ,result))))
	    

(defun p1ctevq: (x y)
    (list 'quote (p1ctev x y)))	;same as p1ctev but value is constant (since has already been evaluated)
				;use this entry for making results to return from p1red, p1sysred, p1redcomm.

(defun p1ctev: (x y)		;compile-time evaluator, x = for err msg, y = exp
	(cond ((null (errset (setq y (eval y))))
	       (barf x "lisp error during compile time evaluation." nonfatal)
	       ''nil)
	      (y) ))

(defun p1-chase-linked-macros: (f)		;seek out (defprop a b macro)'s
       (do ((prop))(nil)
	 (setq prop (getl f '(macro *macro)))
	 (or prop (return f))
	 (or (symbolp (cadr prop))(return (cadr prop)))
	 (setq f (cadr prop))))

(defun macro-expand: (x f)	;returns expanded macro - x is form, f is functional (macro property)
    (cond	((errset (setq x (funcall f x)))
	 x)		;win, return expanded result
	((eq f 'doexpander)
	 (barf x "incorrect do format" nonfatal)
	 ''nil)		;lose, make result be nil
	((eq f 'prog1-expander)
	 (barf x "incorrect prog1 format" nonfatal)
	 ''nil)
	((eq f 'let-expander)
	 (barf x "incorrect let format" nonfatal)
	 ''nil)		;ditto.	    
	(t (barf x "lisp error during macro expansion" nonfatal)
	 ''nil) ))	;again, lose, make result nil

(defun p1eqqte: (z)
        (and (not (atom z))
             (eq (car z) 'quote)
             (pnamep (cadr z))))

(defun p1e-fcn: (x) ((lambda (effs) (p1 x)) t))

(defun p1e1: (x)
;    comment called only from p1prog  
;               tries to factor out a setq from a cond - for example,  
;               (cond ((and (setq x (foo)) alpha) (return nil))) 
;             goes into 
;               (prog2 (setq x (foo)) (cond ((and x alpha) (return nil))))   
    (cond ((or prssl (not (memq (car x) '(cond and or)))) (p1 x))
          (((lambda (data tem f) 
                    (and (setq data (p1hunoz (setq tem (cond (f (cadr x)) 
                                                                ((cdr x)))))) 
                         (or (memq (cadr data) bvars)
                             (assq (cadr data) rnl))
                         (p1 (prog2 (setq tem (p1hunoz tem))
                                     (list 'prog2 
                                           data 
                                           (cons (car x)
                                                 (cond (f (cons tem (cddr x)))
                                                       (tem))))))))
                 nil nil (eq (car x) 'cond)))
          ((p1 x))))

(defun p1hunoz: (y)  (cond ((or (atom (car y)) 
                                (null (cdar y))
                                (not (atom (caar y))))
                            (and data y))
                          ((eq (caar y) 'setq) (cond (data (cons (p1fv (cdar y)) 
                                                                 (cdr y))) 
                                                     (t (car y))))
                          ((getl (caar y) '(fexpr fsubr *fexpr macro *macro)) (and data y))
                          (data (cons (cons (caar y) (p1hunoz (cdar y))) (cdr y)))
                          ((p1hunoz (cdar y)))))

(defun p1f: (f l) 
       ;;      patch up for forms of (eval (cons 'fsubr list))
       (cond ((and (not (atom f))
	         (or (eq (car f) 'quote) (eq (car f) 'function))
	         (atom (setq f (cadr f)))
	         (getl f '(fexpr fsubr *fexpr)))
	    (list 'apply (list 'quote f) l) )))

(defun p1fv: (x) (cond  ((and (cdr x) (cddr x)) (p1fv (cddr x)))
                        ((car x))))


(defun p1fcnarg: (x)			; compile an argument to a functional.

    (p1fcnarg0 (p1v x)))			;compile argument and process the compiled value

(defun p1fcnarg0: (x)			;like p1fcnarg but x is already compiled

    (cond	((and (not (atom x))		;see if it turned out to be (quote (lambda ...
	      (eq (car x) 'quote))
	 (list 'quote (p1gfy (cadr x))))	;if it did, compile the functional form.
	(t x)))				;otherwise, return the ordinary value.

(defun p1gfy: (x) 
    (cond ((atom x) x)
          (t (setq x (compile-fcn (gen-fcn-name)
			    'expr 
			    x 
			    nil 
			    ))
	   x)))

(defun gen-fcn-name: nil		;routine to generate a name for compiler-produced function.
  ((lambda (x) (putprop x t 'dont-intern) x)
    (maknam (append genprefix (explodec (setq gfyc (1+ gfyc)))))))

(defun p1glm: (ll body fl)	; convert (lambda ll body) to (lambda 
			;		 ll transformed-body)
    (cond ((null (cdr body)) (setq body  (p1 (car body))))
	(t (setq fl nil)
	   (do body body (cdr body) (null body)
		(push (cond ((null (cdr body))
			   (p1 (car body)))		;last is for value
			  ((p1e (car body))))	;others are for effect
		   fl))
	   (setq body (cons 'progn (nreverse fl))) ))
    (list 'lambda ll body ))


(defun p1ioc: (x)				;compile ioc into setq's when possible
					;in certain peculiar cases, will not give same
					; result as interpreted ioc. E.g. (ioc gr)
					; and (ioc rg) are compiled the same but
					; interpreted differently.
    (prog (z setqs others)

	(do y (exploden (cadr x)) (cdr y) (null y)
	 (setq z (car y))
	 (cond
	   ((= z (CtoI "d"))
	    (defprop ^d t special)
	    (push '(^d t) setqs))
	   ((= z (CtoI "c"))
	    (defprop ^d t special)
	    (push '(^d nil) setqs))
	   ((= z (CtoI "r"))
	    (defprop ^r t special)
	    (push '(^r t) setqs))
	   ((= z (CtoI "t"))
	    (defprop ^r t special)
	    (push '(^r nil) setqs))
	   ((= z (CtoI "v"))
	    (defprop ^w t special)
	    (push '(^w nil) setqs))
	   ((= z (CtoI "w"))
	    (defprop ^w t special)
	    (push '(^w t) setqs))
	   ((push z others)))	;can't be done as a setq, have to call the interpreter's ioc.
	 )
	(return (cons 'progn
		    (nconc
			(and setqs (list (p1 (cons 'setq (apply 'append (nreverse setqs))))))
			(and others (list (list 'ioc (maknam (nreverse others)))))
			))) ))


(defun p1lam: (f argl)
            ((lambda (p1ll rnl bvars condp lmbp p1lsq body ecrsloss)
                     (setq argl (mapcar (function p1v-fcn) argl))
                     (setq p1ll (p1lmbify (cadr f) (cadr f)))
		 (setq body (p1localdcl p1ll (cadr f) (cddr f)))		;process local declarations, if any
                     (and (not (= (length argl) (length p1ll))) 
		      (barf (cons (list 'lambda p1ll '---)
                                      argl) 
                                "wrong number of args" 
                                data))
                     (setq cnt (1+ cnt))
                     (setq argl (cons (p1glm p1ll body nil) argl))
                     (uuvp p1ll 'p1ll 'lambda)
		 (setq f (lsub p1lsq p1ll)))
                nil rnl bvars nil t nil nil ecrsloss)
            (p1setqex (list f p1cof))
            argl)



(defun p1lmbify: (z data)
    (do ((w) (x z (cdr x)) (y))
        ((null x) (nreverse w))
		(setq y x)		;simulating maplist here
					;make copy since will setq y
                    (cond ((null (car y)) 
                           (barf data "- nil not permissible in bound var list" data))
                          ((memq (car y) (cdr y)) 
                           (warn (list (car y) 'from data)
                                 "- repeated in bound var list")))
                    (cond ((specialp (setq y (car y))))
		      (special (putprop y t 'special))	;all variables are special in (declare (setq special t)) mode
                          (t (cond ((assq y locvars)
			      (setq y (p1rename y))
			      (push (cons y 0) locvars))
			     ((push (cons y 0) locvars))) ))
                    (push y bvars)
	(setq w (cons y w)) ))	;cons up result for 'maplist'



(defun p1localdcl: (lambda-list orignames body)	;pulls local declarations off front of body.
    (prog ()
next-body-elem
	(and (null body) (return body))	;if reached end of body, return.
	(or (and (not (atom (car body)))	;else check for declaration
	         (eq (car (car body)) 'declare))
	    (return body))			;if not a declaration, return with body proper.

	;(car body) is a local declaration.

	(mapc '(lambda (dcl)		;so scan the declarations
		(or (memq (car dcl) '(fixnum flonum notype))
		    (barf dcl "bad local declaration" data))
		(mapc '(lambda (item)	;scan the clauses of a declaration
			(cond
			  ((atom item)	;declaring a local variable
			   (cond  ((specialp item)
				 (barf (list (car dcl) item) "implementation restriction: no local declaration of special variables" nonfatal))
				(t	;if OK dcl, push onto ecrsloss list
				 (push (cons item
					   (cond ((eq (car dcl) 'notype)
						 nil)
					         ((car dcl)) ))
				       ecrsloss)) ))

			  (t		;declaring a local functional variable. (ecch)
			   (cond	((specialp (car item))
				 (barf (list (car dcl) item)
				       "implementation restriction:  no local declaration of special variables"
				       nonfatal))
				(t	;OK dcl, push onto ecrsloss ist
				 (push (cons (car item)
					   (cons (cond ((eq (car dcl) 'notype)
						      nil)
						     ((car dcl)) )
					         (cdr item)))
				       ecrsloss)) )) ))
		  (cdr dcl)))
	     (cdar body))

	(setq body (cdr body))		;this clause has been processed, discard it and go on the next.
	(go next-body-elem))

;;;now perform declarations according to ecrsloss

    (mapc '(lambda (var rnvar)		;map over var list
	    ((lambda (dcl)			;dcl:=ecrsloss entry for var
	      (and dcl
		 (and (eq var rnvar)	;if not already renamed, rename it.
		      (progn
			(setq rnvar (p1rename var))
			(rplaca (memq var lambda-list) rnvar)
			(rplaca (memq var bvars) rnvar)
			(rplaca (assq var locvars) rnvar) ))
		 (putprop rnvar
			(cdr dcl)
			(cond ((atom (cdr dcl)) 'number)
			      ( 'numfun ) ))))
	     (assq var ecrsloss)))
	orignames lambda-list)

    body)	;return the body with local dcl pulled off the front



(defun p1rename: (x)			;gets gensym'ed name for a variable to avoid property-list conflicts
    ((lambda (gs)
	(putprop gs x 'rename)		;remember original name for error messages
	(push (cons x gs) rnl)		;also put entry on rename list so future references will get renamed.
	(and (setq x (get x 'number))		;carry over number-variable declaration
	     (putprop gs x 'number))
	gs)				;and return the new name
      (gensym)))


;;;extended p1rename which patches bvars and locvars - used by p1localdcl (KLUDGE)

(defun p1renamex: (x)
  ((lambda (gs)
     (rplaca (or (memq x bvars)
	       (barf x "not in bvars! - p1renamex" barf))
	   gs)
     (rplaca (or (assq x locvars)
	       (barf x "not in locvars! - p1renamex" barf))
	   gs)
     gs)
   (p1rename x)))

(defun p1map: (mapx mapfcn maplists)		;to translate the map fcns
    (prog (mapgensyms mapvalue mapc-for-value z y mapargs maptem1 maptem2)
	(or effs (progn (setq mapvalue (gensym))	;if must return a value
	          (and (eq (cadr mapx) 'map)
		     (setq mapc-for-value (gensym) mapvalue nil))))
					;if map or mapc for value, value will be first of maplists
	(setq y (eq (caddr mapx) 'car))	;t for mapc,mapcar,mapcan. nil for map,maplist,mapcon
	(and
	  (or (and (not effs)(eq (cadr mapx) 'maplist))
	      (memq (cadr mapx) '(mapcan mapcon)))
	  (setq maptem1 (gensym) maptem2 (gensym)))


	;construct the do - variables and the arg list for the function being mapped.

	(do i (length maplists) (1- i) (zerop i)
		(setq mapgensyms (cons (gensym) mapgensyms))
		(setq mapargs (cons (cond (y `(car ,(car mapgensyms)))
				      ((car mapgensyms)))
				mapargs))
	  )

	(setq y nil)

	;see if mapfcn needs to be evaluated

	(cond ((and (not (atom mapfcn))
		  (memq (car mapfcn) '(quote function))
		  (or (atom (cadr mapfcn))
		      (eq (caadr mapfcn) 'lambda)))
	       (setq mapfcn (cadr mapfcn))		;mapfcn need not be evaled.
	       (and (atom mapfcn)			;but check for fsubrs.
		  (getl mapfcn '(fsubr *fexpr))
		  (setq y mapfcn mapfcn nil)))  		;special hack - will become (apply 'f (list g1 g2...
	      ((setq y mapfcn mapfcn (gensym))))	;will have to eval mapfcn, assign to gensym

	;generate a do

	(setq z `(,map-do		;so go and return will get special treatment
	           ,(nconc			;make var list
		    (mapcar (function (lambda (val gs)
					`(,gs
					   ,(cond ((and mapc-for-value (eq gs (car mapgensyms)))
						 mapc-for-value)	;get first maplist from outer
								; lambda which has already evaled it.
						(t val))
					   (cdr ,gs))))
			  maplists mapgensyms)
		    (and mapvalue `((, mapvalue)))
		    (and maptem1 `((,maptem1) (,maptem2))))
		 (				;make list of endtest, retval
		  (or . ,(mapcar (function (lambda (gs)
					     `(null ,gs)))
			       mapgensyms))
		  ,mapvalue
		  )
		 ;the body of the do is the application of the function

		 ,(cond ((prog2 (setq z (list map-funarg (cond (mapfcn (cons mapfcn mapargs))  ;normal case
						          (`(apply ',y
							         (list .,mapargs))))));fsubr
			      (null mapvalue))
		         z)	;if value not needed, just call the mapfcn
		        ((eq (cadr mapx) 'maplist)	;value is list of results
		         `(progn
			  (setq ,maptem2 (ncons ,z))
			  (cond ((null ,mapvalue)	;first time...
			         (setq ,maptem1 (setq ,mapvalue ,maptem2)))
			        (t (rplacd ,maptem1 ,maptem2)
				 (setq ,maptem1 ,maptem2)))))
		        (`(progn		;mapcan or mapcon
			  (setq ,maptem2 ,z)  ;compute it
			  (and ,maptem1 (rplacd ,maptem1 ,maptem2))
			  (or ,mapvalue (setq ,mapvalue ,maptem2))
			  (setq ,maptem1 (or (last ,maptem2) ,maptem1)))))
		 ))

	; now put a lambda around it if the fcn needs to be evaled

	(and y
	     mapfcn
	     (setq z `((lambda (,mapfcn) ,z)
		     ,y)))

	;if it is a map or mapc for value, enclose the code in a lambda to evaluate the first list
	;and do the map and then return the first list as the value

	(and mapc-for-value
	     (setq z `((lambda (,mapc-for-value)
			   ,z
			   ,mapc-for-value)	; <- a gensym
		     ,(car maplists) )))

	(return z)))

(defun p1prog: (x p1mapcnt)	; change (prog (vars) body) to (prog (setq-list) call-out-flag (tags-gone-to) (vars) transformed-body)
    (prog2  nil 
            ((lambda (rnl bvars progp gone2 p1psq effs p1cof mapf ecrsloss)
                        (prog (condp p1csq lmbp p1lsq pvrl p1vars gl p1cnt body prssl)
                                (setq pvrl (p1lmbify (car x) (car x)))
			  (setq body (p1localdcl pvrl (car x) (cdr x)))	;process local declarations, if any.
			  (mapc (function (lambda (x)		;put special prog vars on setq list
					(and (specialp x)
					     (setq p1psq (add x p1psq)))))
			        pvrl)
                                (setq p1vars locvars)
                                (setq p1cnt (setq cnt (1+ cnt)))

			  ;do the tags

			  (mapc (function (lambda (x) (and (atom x)
						     (setq gl (cons (cons x (gensym)) gl)))))
			        body)

			  ;do the body

                                (setq body 
                                      (mapcar 
                                         (function 
                                           (lambda (y)
                                                   (setq cnt (1+ cnt))
                                                   (cond ((atom y)
                                                          (setq prssl t)
					        (setq y (cdr (assq y gl)))	;subst (gensym) for tag
					        (putprop y t 'defined)	;tag is defined now.
					        y)
                                                        (t (p1e1 y)))))
                                          body))
                                (p1bug p1cnt p1vars)
                                (uuvp pvrl 'pvrl 'prog)
			  (setq x nil)
                                (cond ((memq gofoo gone2)
					(mapc (function (lambda (x) (putprop (cdr x) t 'back-reference)))
						gl))	;if a computed goto, any tag could
							;get back referenced.
;                                       gofoo on gone2 says there is a computed go
			         (t (mapc (function (lambda (tag)
					(or (memq (car tag) gone2)
					    (prog2 (push (car tag) x)  ;remember this unused tag for err msg
					           (putprop (cdr tag) t 'back-reference)
							;since there are no go's to this tag, tell
							;pass 2 that it has no slotlist.
							;but can't delete it from gl.
						     )) ))
					gl)))
			  (and x (warn x "- unused go tags"))
                                (setq x (list (lsub p1psq pvrl) p1cof))
;		here is return value.
                                (return (nconc (list 'prog p1psq p1cof gl pvrl) body))))
                        rnl bvars t nil nil t nil nil ecrsloss)
            (p1setqex x)
	  ))

(defun p1special: (x)
    (cond ((specialp x))
	(special (putprop x t 'special))	;in (declare (setq special t)) mode, everything is special
          ((not (memq x bvars)) 
           (putprop x t 'special)       ;if free and not yet declared, then do so and inform user
	 (or (memq 'value (status system x))	;if a system global variable, do not barf.
               (warn x "undeclared - henceforward assumed to be special")) 
           t)))


(defun p1spread:	(var arglen)		; generate arglen arguments by decomposing a list-valued atom.
 (cond ((= arglen 0) nil)
       ((= arglen 1) (list (list 'car var)))
       (t (do ((arglst (list (list 'cadr var))
		   (xcons arglst (subst var 'foo '(car (setq foo (cdr foo))))))
	     (argcnt 2 (1+ argcnt)))
	    ((= argcnt arglen) (cons (list 'car var) arglst))))))
;;;function to extend setq's from inner prog, cond, or lambda
;;;to outer prog, cond, or lambda.  Also extends p1cof.
;;;(replaces old p1sqe)
;;;argument is list of:
;;;	setq'ed vars minus those bound at level being exited
;;;	p1cof value
;;;p1setqex should be called after the various p1ll, p1psq,
;;;etc. have been unbound so it can mung previous values.

(defun p1setqex: (x)
    (cond	((car x)	;vars to be setq'ed-propagated.
	   (and condp (setq p1csq (ladd (car x) p1csq)))
	   (and lmbp (setq p1lsq (ladd (car x) p1lsq)))
	   (and progp (setq p1psq (ladd (car x) p1psq))) ))
    (setq p1cof (or p1cof (cadr x)))
    nil)

(defun p1sqg: nil 		;called for go, return.
    (setq prssl t)
    (cond ((not progp) (barf nil "go or return not in prog" nonfatal))	;and p1sqg returns nil, which causes the
	((not mapf) 0)						;go or return to get changed to 'nil
	((plusp p1mapcnt) p1mapcnt)		;inside map
	(t 				;inside map but not inside prog
	 (barf nil "go or return not in prog" nonfatal)) ))

(defun p1sqv: (y) 
          (cond (condp (setq p1csq (add y p1csq))))
          (cond (lmbp (setq p1lsq (add y p1lsq))))
          (cond (progp (setq p1psq (add y p1psq))))
	(and (setq y (assq y locvars))	;if a local variable, update its cnt
	     (rplacd y (cond ((plusp (cdr y)) cnt)	;I.E.: setq is considered to be a reference
			 ((minus cnt))) )))		;but if never yet referenced, make minus for uuvp

(defun p1sts: (x)		;compile a status or sstatus
      (prog (y z)
	(setq z (substr (setq z (get_pname (cadr x)))  		;get control argument
		      1
		      (min 4 (stringlength z)) ))
	(and (eq (car x) 'sstatus) (go sstat))			;if sstatus, go to other routine
	(setq y (assoc z '(					;status - get description of what to do
			("chtr" 1 char)
			("ioc" 2)
			("macr" 3 char)
			("synt" 4 char)
			("topl" 5)
			("urea" 6)
			("uwri" 7)
			("+"    8.)
			("date" 9.)
			("dayt" 10.)
			("runt" fcn runtime)
			("time" 12.)
			("inte" 13. eval)
			("spcn"  14.)		
			("crun" 15.)
;			("*nop" var *nopoint)	;flushed
;			("*rse" 17.)	;flushed
			("gcti" 18.)
			("spcs" 19. eval)
			("pdls" 20. eval)
			("pdlr" 21. eval)
			("pdlm" 22. eval)
			("lisp" 23.)
			("pagi" 24.)
			("unam" 25.)
			("jcl"  26.)
			("arg" 27. eval)
			("terp" 28.)
			("_"    29.)
			("syst" 31. eval)
			("char" 32. eval)
			("tabs" constant)
			("crfi" 34.)
			("ttyr" 35.)
			("udir" 36.)
			("feat" 37.)
;			("()()" 38.)		        ;flushed
			("uuol" 39.)
			("divo" 40.)
			("abbr" 41.)
			("dow" 42.)
			("stat" constant)
			("ssta" constant)
			("newl" constant)
			("nofe" 46.)
			("linm" 47.)
			("clea" 48.)
			("eval" 49.)
			("mulq" 50.)
			("mulp" 51.)
			  )))
	(cond ((null y) (go barf))
	      ((null (cddr y)) (return x))		;can leave as fsubr
	      ((eq (cadr y) 'constant)		;can do at compile time
	       (return (list 'quote (eval x))))
	      ((eq (cadr y) 'var)			;can use value of a special variable
	       (putprop (caddr y) t 'special)
	       (return (p1 (caddr y))))
	      ((eq (cadr y) 'fcn)			;other function exists
	       (return (p1 (cons (caddr y) (cddr x)))))
	      ((eq (caddr y) 'eval) (go ieval))		;evaluate all arguments
	      ((eq (caddr y) 'char) 			;2nd arg is a char - number or pname
	       (and (pnamep (caddr x))
	            (rplaca (cddr x) (CtoI (get_pname (caddr x)))))
	       (go ieval))
	      )

barf	(and (cddr x) (warn x "unrecognized status function - left exactly as written."))	;don't barf if no args
	(return x)

	;;;come here to change to lsubr version

ieval	(return (p1 (cons (cdr (assq (car x) '((status . *status) (sstatus . *sstatus)) ))
		        (cons (cadr y)		;this number is control argument, goes to tv in lisp_status_fns_
			    (cddr x)))))

	;;;come here to do sstatus function

sstat	(setq y (assoc z '(				;get description of what to do
			("chtr" 1. char)
			("ioc" fcn ioc)
			("macr" 3 char fcn)
			("synt" 4 char)
			("topl" 5 eval)
			("urea" fcn uread)
			("uwri" fcn uwrite)
			("+"    8. eval)
			("inte" 13. eval fcn)
			("crun" fcn crunit)
;			("*nop" var *nopoint)	;flushed
;			("*rse" fcn *rset)	;flushed
			("gcti" 18. eval)
			("spcs" 19. eval)
			("pdls" 20. eval)
			("pdlr" 21. eval)
			("pdlm" 22. eval)
			("terp" 28. eval)
			("_"    29. eval)
			("char" 32. eval)
			("crfi" 34.)
			("ttyr" 35. eval)
			("feat" 37.)
			("uuol" 39. eval)
			("divo" 40. eval)
			("abbr" 41. eval)
			("nofe" 46.)
			("clea" 48. eval)
			("eval" 49. eval)
			("mulq" 50. eval)
			("mulp" 51. eval)
			)))
	(cond ((null y) (go barf))
	      ((null (cddr y)) (return x))		;can leave as fsubr.
	      ((eq (cadr y) 'var)			;change to setq to special variable
	       (putprop (caddr y) t 'special)
	       (return (p1 (list 'setq (caddr y) (caddr x)))))
	      ((eq (cadr y) 'fcn)			;change to some other fcn
	       (return (p1 (cons (caddr y)(cddr x)))))
	      ((eq (caddr y) 'char)
	       (and (pnamep (caddr x))
		  (rplaca (cddr x) (CtoI (get_pname (caddr x)))))
	       (go ieval1))
	      ((eq (caddr y) 'eval) (go ieval1))
	      ((go barf)))

ieval1	(and (eq (cadddr y) 'fcn)			;third argument is functional - use p1gfy on it
	     (not (atom (setq z (cadddr x))))
	     (memq (car z) '(quote function))
	     (rplaca (cdddr x) (list 'quote (p1gfy (cadr z)))))
	(and (= (cadr y) 3)				;sstatus macro has 4th argument, s or nil, default nil.
	     (cond ((null (cddddr x))
		  (setq x (append x '(nil))))	;no 4th arg, put in default of nil
		 ((equal "s" (substr (get_pname (car (cddddr x))) 1 1))
		  (setq x (append x '(t))))		;4th arg = splicing, put t as 4th arg
		 ((setq x (append x '(nil))))))	;other 4th arg, change to nil.
	(go ieval)))

(defun p1v-fcn: (x) ((lambda (effs) (p1 x)) nil))


(defun uuvp: (l ll f)
     (prog (unused set-not-ref)		
	(mapc (function (lambda (x)		
			(and (setq x (assq x locvars))
			     (cond ((zerop (cdr x)))
				 ((minusp (cdr x))
				  (rplacd x (minus (cdr x)))	;if setq'ed only, make cnt + for pass 2
				  t)
				 (nil))
			     (setq x (cond ((do y rnl (cdr y) (null y)
					   (and (eq x (cdar y)) (return (caar y))) ))
						;get variable of which this is the renaming,if there is one.
				         ((car x))))
			     (cond ((memq x (cond ((eq f 'prog) p1psq)
					       (t p1lsq)))
				  (push x set-not-ref))	;setq'ed but never referenced
				 (t (push x unused)))	;not referenced at all
			    )))
		l)
	(cond ((eq f 'prog)
	       (and unused (warn unused "- unused prog variables"))
	       (and set-not-ref (warn set-not-ref "- prog variables setq'ed but never referenced")))
	      (t
	       (and unused (progn (warn unused "- unused lambda variables")
			      (mapc '(lambda (x) (rplacd (assq x locvars) cnt)) unused)))
						;put dummy references on unused lambda variables
						;to keep pass 2 from barfing.
	       (and set-not-ref (warn set-not-ref "- lambda variables setq'ed but never referenced"))))
	(set ll (lsub (eval ll) unused))	;remove unused vars but not setq'ed ^ref vars.
      ))

(defun doexpander: (x)
    (prog (indxl endtst endval tg1 tg2 vars stepdvars vals prog-fcn declarations body prog-with-initial-values-p)
	(setq prog-fcn (cond ((eq (car x) map-do) map-prog)
			  (t 'prog)))
          (cond ((and (car (pop x)) (atom (car x)))	;pop gets rid of "do"
					; and checks for indxl of nil!!!
                 (setq  indxl (list (list (car x) (cadr x) (caddr x)))
                        endtst (car (setq x (cdddr x))) 
                        endval nil))
                (t  (setq indxl (car x) 
                          endtst (car (pop x)) )		;list of endtst,endval
		(cond (endtst			;normal 'do'?
		       (setq endval (and (cdr endtst)	;yes, pick up end test and return values
				     (cons 'progn (cdr endtst)))
			   endtst (car endtst)))
		      (t				;no, this is non-iterative do - prog with init values.
		       (setq prog-with-initial-values-p t))) ))
          (setq stepdvars (mapcan '(lambda (x) (and (cdr x) (cddr x) (list x))) 
                                  indxl))
	(and stepdvars
	     prog-with-initial-values-p
	     (progn
		(warn stepdvars "will not be incremented because this is a non-iterative do")
		(setq stepdvars nil)))
          (pop x)
	(setq body
	   (do ((bd x (cdr bd))) ((or (null bd) (atom (car bd)) (not (eq (caar bd) 'declare))) bd)
	      (setq declarations (append declarations (cdar bd)))))	;make list of all clauses of all declares
	(cond ((not prog-with-initial-values-p)
	       (setq tg1 (gentag))
	       (cond ((null endtst)
		    (and endval (warn endval "will not be returned because of nil end test")))
		   (t (setq tg2 (gentag))))))
          (mapc '(lambda (x) (push (car x) vars) (push (cadr x) vals)) indxl)
	(return
            `((lambda
	      ,(nreverse vars)
	      (declare . ,declarations)		;move user's declarations up to here
	      (,prog-fcn
	        ()			;prog's lambdalist
	        ,@(and tg2 `((go ,tg2)))	;put test at end
	        ,@(and tg1 `(,tg1))
	        ,@ body
	        ,@(and stepdvars (list (dostepr stepdvars)))
	        ,@(and tg2 (list tg2))
	        ,@(and endtst
		     `((cond (,endtst (return ,endval)))))
	        ,@(and tg1 `((go ,tg1)))))
	    . ,(nreverse vals)))))

	;generate code to step all variables in parallel.
(defun dostepr: (stepdvars)
       `(setq ,(caar stepdvars)
	    ,(cond ((null (cdr stepdvars)) (caddar stepdvars))
		 (t `(prog1 ,(caddar stepdvars)
			  ,(dostepr (cdr stepdvars)))))))
(defun gentag: nil
     ((lambda (z)
	(putprop z t 'compiler-generated)
	z)
      (gensym) ))

(defun let-expander (x)
	(nconc (list (append (list 'lambda (mapcar 'car (cadr x)))(cddr x)))
	       (mapcar 'cadr (cadr x)))))
(defun prog1-expander (x)
       (or (cdr x)(error "prog1 must have at least 1 argument" 'fail-act))
       (list* 'prog2 ''0 (cdr x)))




(defun cmp1: (supplied-forms)
; translate a file compiling those S-expressions which try to define functions.

(catch  (prog (x dectb fl form irasloss being-compiled compiler-state yet-to-compile fun informs)

        (setq compiler-state 'maklap informs supplied-forms)
        (setq irasloss (setq dectb '((*fexpr . fexpr) (*expr . expr) (*lexpr .expr))))
    a   (or (errset (setq form
		      (cond (yet-to-compile
			    (prog2 nil (car yet-to-compile)
				 (setq yet-to-compile
				       (cdr yet-to-compile))))
			  (supplied-forms
			    (cond (informs (prog2 0 (car informs)
					      (setq informs (cdr informs))))
				(t (return nil))))	;all done
			  ((read)))))
	  first-eof
	  (progn	  (printmes nil "There is probably a missing "")""." nil)
		  (or (null current-function) (equal current-function '(nil))
		      (printmes current-function "was the last thing compiled." nil))
		  (return nil)))

        (setq current-function '(nil) being-compiled nil)
    b   (cond ((or nocompile			;dont look for anything
	         (atom form)		;dont compile tl atoms
	         (not (eq (car form) 'defun))	;"Random" or macro form
	         ;;MUST be defun at this point
	         (getl 'defun '(macro *macro))	;for Bawden.. let it fall thru to macro stuff
	         ))
              ((memq (caddr form) '(fexpr expr macro))
                (setq form (list 'defprop (cadr form)
                        (cons 'lambda (cdddr form)) (caddr form))))
	    ((memq (cadr form) '(fexpr expr macro))	; for alternate form of defun...
	      (setq form (list 'defprop (caddr form)
		    (cons 'lambda (cdddr form)) (cadr form))))
              ((setq form (list 'defprop (cadr form) (cons 'lambda (cddr form)) 'expr))))
       (cond ((atom form))		;ignore atoms since no side effects to evaluation
	   ((memq (setq fun (car form)) '(include %include includef))   ;interpreter (include %include) statement is changed to compiler include dcl.
	    (cond (supplied-forms
		  (barf form " - illegal during program-invoked compilation" nonfatal))
		((errset (let ((errset nil)) (eval form)))
		 (push (namestring (names infile)) source-map)
		 (eoffn infile		;succeeded - set up eoffn
		   (function
		     (lambda (a1 a2)	;check for eof-in-object, pop back to prev file
			(cond (first-eof
				(setq first-eof nil) a1 a2;hack for no msg
				t)	;go back & check for eof in the middle of an object.
			      (t
				(setq first-eof t)	;really done
				nil) )))) )	;cause (inpush -1) and continue
		((printmes form "include file not found." 'data)) ))

	   ((eq fun 'declare)
	    (let ((current-function '(declare)))	 ;special
	         (or (errset (mapc (function eval) (cdr form)))
		   		;unless declarations lose, do them 
		   (go c)))          ;and go to next expression in file
	    (go a))

	   (nocompile (or (eq fun 'comment) (put-in-tree form)))	; if not compiling, just shove it through.
	   ((and (eq fun 'defprop)
                   (assq (setq fl (cadddr form)) indiclist))
	    (cond ((atom (setq current-function (cadr form))))	; atom is ok.
		((null (cdr current-function)) (setq current-function (car current-function)))
		((null (cddr current-function)) (put-in-tree (cons 'defprop
				(cons (car current-function)
				      (xcons (ncons (cadr current-function))
					(setq current-function (gen-fcn-name)))))))
		(t (put-in-tree (cons 'defprop (cons (car current-function)
				(xcons (ncons (caddr current-function))
					(setq current-function (gen-fcn-name))))))))
              (setq undfuns (delq current-function  undfuns))
	    (and (memq current-function ffvl) (go f))
              (cond ((not (atom (caddr form))) 
		  (and expr-hash (setq yet-to-compile (cons (list 'defprop (cadr form)
						(sxhash (caddr form)) 'expr-hash) yet-to-compile)))
                     (compile-fcn (setq being-compiled current-function) fl (caddr form) nil)
                     (go a))
                    (t  (and (setq x (getl current-function '(*expr *fexpr *lexpr)))
			(not (eq fl (cdr (assq (car x) dectb))))
			(wrntyp current-function fl x))
		    (putprop current-function t (do x dectb (cdr x) (null x)
				  (and (eq fl (cdar x)) (return (caar x))) ))
		    (put-in-tree form))))
             ((and (eq fun 'defprop)
                   (eq (cadddr form) 'macro))
              (and macros (put-in-tree form))
              (cond ((getl (cadr form) '(expr fexpr subr fsubr lsubr *fexpr *expr *lexpr))
		 (putprop (cadr form) (caddr form) '*macro))
                    (t (eval form))))
             ((cond ((eq fun 'array) (setq fl (cadr form)) t)
                    ((and (eq fun '*array) (p1eqqte (cadr form)))
                        (setq fl (cadadr form))
                        t))
	    (putprop fl t '*array)		;give *array property since code to set up array at run time was seen
	    (and (memq (setq current-function fl) ffvl) (go f))
	    (put-in-tree form))
             ((and form (atom fun) (setq fl (cadr (getl fun '(macro *macro)))))
	    (cond ((and (symbolp fl)(cadr (getl fl '(macro *macro))))
		 (setq fl (p1-chase-linked-macros fl))))
	    (cond ((or (null (errset (setq irasloss (funcall fl form))))
		     (eq irasloss dectb))
		 (go c))
		(irasloss (setq form irasloss) (go b))))          ;apply macro property and try again
	   ((eq fun 'comment))		;no need to keep comments around
	   ((and (eq fun 'progn) (equal (cadr form) ''compile))
	    ;; tack code on front of list so it will be compiled first
	    (setq yet-to-compile (append (cddr form) yet-to-compile)))
	   ((and (eq fun 'eval-when)(cdr form))
	    (cond ((memq 'load (cadr form))
		 ;; tack code on front of list so it will be compiled first
		 (setq yet-to-compile (append (cddr form) yet-to-compile))))
	    (cond ((memq 'compile (cadr form))
		 (or (let ((current-function '(eval-when)))
			(errset (mapc 'eval (cddr form))))
		     (go c)))))
	   (t (put-in-tree form)))
       (go a)

;;;
;;;   Top level evaluation has lost
;;;

c     (let ((^r nil)(^w nil))
	 (apply 'ioc messioc)
	 (princ "
lisp_compiler: lisp error during declaration or macro expansion at top 
	level; the losing form is ")
	 (prinb form 5. 20.)			;display the losing form but limit the amount of typeout
	 (terpri)
	 (cond (dataerrp (princ "Please correct and type $p") (break dataerrp t) ))
	 (go a))

    f   (barf current-function "has previously been compiled as a
        free functional variable - you will lose!"  data))
     e-o-f)	;end of catch way back there

  (or check (finish-code))
      ;;;moved to before eoc-eval 5/4/80
  (mapc '(lambda (x) (or (errset (eval x)) (barf x " - losing form in eoc-eval." nonfatal)))
        eoc-eval)
t)




(defun cl: fexpr (l)
;	compile a list of functions given by atom name.
       (let ((data nil)
	   (barfp t)
	   (eoc-eval nil)
	   (constant-list nil)
	   (pc)(seg-name "[pd]>!lcp-scratch!")(codelist)
	   (functions-called)(functions-defined)(labels-to-define)(being-compiled))
	  (or check (init-code-generator))
	  (mapcar (function (lambda (j)
			        (cond ((setq data
					 (getl j '(expr fexpr)))
                                          (compile-fcn (setq being-compiled j)
					     (car data) (cadr data) nil)))))
		l)
	  (or check (finish-code))))

(defun compile-top-level-forms (forms segnam)	;5/4/80
       (let ((data nil)
	   (barfp nil)
	   (eoc-eval nil)
	   (constant-list nil)
	   (pc)(seg-name "[pd]>!lcp-scratch!")(codelist)(check check)
	   (functions-called)(functions-defined)(labels-to-define)(being-compiled))
	  (cond (segnam (setq seg-name segnam))(t (setq check t)))
	  (or check (init-code-generator))
	  (cmp1 forms)))
       


(defun cf:  (x)
        (prog (start-time start-runtime start-paging line tem ^w ^q ^r current-function eoc-eval
	      pc codelist constant-list functions-called functions-defined labels-to-define) ;for pass 2
	(or (errset (setq infile (openi x))) (go nofile))
	(setq source-map (list (namestring (names infile))))
	(setq first-eof t)
	(eoffn infile (function (lambda (a1 a2)
				(cond (first-eof (setq first-eof nil) a1 a2 t)	;retry in case eof in obj
				      ((throw nil e-o-f)) ))))

	(setq seg-name			; strip ".lisp" suffix
	      (let ((names-infile (names infile)))
		 (cond ((eq (car (last names-infile)) 'lisp)
		        (do ((split (cdr (reverse (cdr names-infile)))
				(cdr split))
			   (answer nil (list* "." (car split) answer)))
			  ((null (cdr split))
			   (apply 'catenate (cons (car split) answer)))))
		       (t (get_pname (cadr names-infile))))))
	(setq start-time (status daytime))
	(setq start-runtime (cond (total-time (runtime)) (t 0)) start-paging (status paging))
	(ioc rq)

	(or check (init-code-generator))
c	(cond ((atom (setq tem (errset (cmp1 nil))))	;compile some function definitions
	       (setq ^q t ^w nil ^r t line
				(cons (cond ((symbolp current-function) current-function)
					  (t '-????-))
				      line))
	       (cond ((null tem)
		    ((lambda (^r ^w)
			(apply 'ioc messioc)
			(princ "
*** LISP ERROR WHILE COMPILING ")
			(princ current-function)
			(princ "
    The error message from Lisp appears above.
")
			(break barfp barfp)		;in debug mode ,  let user fiddle.
			(go c))
		      nil nil) ))
	       (go c)))				;keep on compiling the file
	(ioc svt)					;switch all i/o to tty
	(and line (printmes (sort line 'alphalessp) "- failed to compile." nil))
	(and undfuns (printmes (sort undfuns 'alphalessp) "- functions referenced but not defined." nil))
	(close infile)
	(and total-time
	     (let ((base 10.)(*nopoint t))  ;print compiling statistics
		(princ "
Compilation finished.  Elapsed time = ")
		(pr-time
		  (let ((a (mapcar 'difference (status daytime) start-time)))
		       (let ((c (caddr a))(b (cadr a))(a (car a)))
			  (and (minusp c) (setq c (+ c 60.) b (1- b)))
			  (and (minusp b) (setq b (+ b 60.) a (1- a)))
			  (and (minusp a) (setq a (+ a 24.)))	;if we crossed a midnight, patch it up.
			  ;;3-day compilations will still lose.
			  (list a b c))))
		(princ ", runtime = ")
		(prin1 (//$ (float (setq start-runtime (difference (runtime) start-runtime))) 1000000.0))
		(princ ",
	paging = ")
		(prin1 (car (setq tem (mapcar (function difference) (status paging) start-paging))))
		(princ " + ")
		(prin1 (cadr tem))
		(princ " ")
		(prin1 (list (// (* (cadr tem) 1000000.) start-runtime)))	;paging rate in parentheses
		(princ ", gc time = ")
		(prin1 (//$ (float (status gctime)) 1000000.0))
		(princ " (")
		(prin1 (// (* 100. (status gctime)) start-runtime))
		(princ "%)")
		(terpri)))
	(return nil)
nofile	(barf  x " - file not found." nonfatal)
        ))



(defun pr-time: (3list)		;routine to print out a time
				;called with base = 10., (status *nopoint) = t
	(pr-tim1 (car 3list))
	(tyo 72)			; ":"
	(pr-tim1 (cadr 3list))
	(tyo 72)
	(pr-tim1 (caddr 3list))  )

(defun pr-tim1: (x)			;print 2 digit number with leading zero 
	(and (lessp x 10.) (tyo 60))	;put leading zero if needed
	(prin1 x))

(defun command-interface: ()			; interpret the arguments of the 'lisp_compiler xxx -opt' command
       (setq errlist '((init1)))		; we only want to get called once
       (terpri)
       (prog (i arg file hold listing-desired seg-name long-listing)
	   (setq long-listing "-brief")	; default is short listing
	   (setq i 1)
nextarg 	   (or (setq arg (status arg i)) (go last-arg))	     ; go if no more arguments to do
	   (cond ((equal (substr (get_pname arg) 1 1) "-")     ; process an option
		(cond   
		  ((memq arg '(-pathname -pn -p))
		   (setq file (absolute_pathname_ (status arg (setq i (1+ i)))))
		   (cond ((= 0 (cadr file))
			(setq file (car file)))
		         (t (com_err_$one-arg (cadr file) "lisp_compiler" "^a" (status arg i))
			  (quit))))
		  ((memq arg '(-db -debug)) (debug t))
		  ((eq arg '-eval)
		   (eval (readlist (exploden (status arg (setq i (1+ i)))))))
		  ((memq arg '(-tm -time -times)) (setq time-option t total-time t ^d t))
		  ((memq arg '(-tt -total -total_time))
		   (setq total-time t))
		  ((memq arg '(-ps -pause)) (setq pause t))
		  ((eq arg '-pause_at)
		   (setq pause-at (readlist (exploden (status arg (setq i (1+ i))))))
		   (and (atom pause-at) (setq pause-at (ncons pause-at))))
		  ((memq arg '(-nw -nowarn)) (setq nowarn t))
		  ((memq arg '(-mc -macros)) (setq macros t))
		  ((eq arg '-all_special) (setq special t))
		  ((memq arg '(-gp -gnp -genprefix))
		   (eval (list 'genprefix (status arg (setq i (1+ i))))))
		  ((memq arg '(-ck -check)) (setq check t))
		  ((eq arg '-ioc) (eval (list 'ioc (status arg (setq i (1+ i))))))
		  ((memq arg '(-list -ls))
		   (setq listing-desired t))
		  ((memq arg '(-long -lg))
		   (setq long-listing ""))
		  ((memq arg '(-messioc -mioc))
		   (setq messioc (list (status arg (setq i (1+ i))))))
		  ((memq arg '(-hd -hold)) (setq hold t))	;remain in lisp after compiling
		  ((memq arg '(-no_compile -ncp)) (setq nocompile t))	; don't interpret defun's and defprop's in file
		  ((memq arg '(-pedigree -pdg))
		   (let ((obarray cobarray))	;for autoload
		        (historian)(quit))) ; print history and exit
		  (t (princ "lisp_compiler: Unrecognized control argument ")
		     (princ arg)
		     (princ " has been ignored.")
		     (terpri))
		  ))
	         ((null file)
		(setq file (absolute_pathname_$add_suffix arg "lisp"))
		(cond ((= 0 (cadr file))
		       (setq file (car file)))
		      (t (com_err_$one-arg (cadr file) "lisp_compiler" "^a" arg)
		         (quit))))
	         (t (princ "lisp_compiler: extra argument has been ignored: ")
		  (princ arg)
		  (terpri) ))
	   (setq i (1+ i))
	   (go nextarg)

last-arg 	   (and (null file) (return nil))	; if no file specified, enter lisp so he can use cf
	   (let ((minf-result (hcs_$status_minf file "" 1)))   ; check that file is OK
	        (cond ((not (= 0 (caddr minf-result)))	; not found?
		     (com_err_$one-arg (caddr minf-result) "lisp_compiler" "^a" file)
		     (quit))
		    ((not (= 1 (car minf-result))) ; a link or directory
		     (com_err_$one-arg 0 "lisp_compiler" "^a is not a file." file)
		     (quit))
		    ((= 0 (cadr minf-result)) ; bit count zero
		     (com_err_$one-arg 0 "lisp_compiler" "Zero-length file: ^a" file)
		     (quit))))
	   (princ (catenate "Lisp Compiler " compiler-revision))(terpri) ; announce ourselves
	   (cf file)			; compile file
	   (and listing-desired		; if -list option used, call make_lisp_listing
	        (make_lisp_listing seg-name long-listing))
	   (or hold (quit))			; quit unless -hd option was given
	   ))

(defun printmes: (w msg warn)
  (or (and nowarn				;suppress warning s if called with the -nowarn option
	 (or (null warn) (eq warn 'warn)))
      ((lambda (^r ^w)
	  (apply 'ioc messioc)
	  (or warn (setq ^r nil))		;suppress output of random msgs to the defsubr file
	  (and warn being-compiled (progn
		(terpri)
		(princ "*** DIAGNOSTICS FOR ")
		(princ (cond ((boundp 'being-compiled) being-compiled) (t '(???))))
		(terpri)
		(setq being-compiled nil)))	;so this header is only printed once per function in error.

	  (or (zerop (charpos t)) (terpri))	;get to left margin
	  (princ (cdr (assq warn '(		;put message prefix
			(warn . "Warning: ")
			(nonfatal . "Error: ")
			(data . "Severe Error: ")
			(barf . "Compiler Error: ")
			(nil . "lisp_compiler: ") ))))
	  (cond (w (cond (warn (prinb w 5. 20.)) ((prin1 w))) (tyo 40)))	;if there is a datum, print it
					;but limit the length of the output.
            (prinst msg)                   	;print out the message
	  (terpri)
            (cond ((and warn (not (eq warn 'warn)))
                    (cond ((eq warn 'data) 
                           (and dataerrp (princ "; data error - to proceed type $p
 ")		         (break data t))
                           (err 'data))		; ???????
		      ((eq warn 'nonfatal)
		       (setq errflag t)	;so pass2 will be suppressed
		       (and dataerrp (princ ";data error - to proceed type $p
")				(break data t)))
                          (t (princ "
%%%%%%%% compiler error - contact the compiler maintenance persons %%%%%%%%")
                             (break barf barfp)
                             (err 'barf)))))
           nil  ;no value in particular
                )
      nil nil)))



(defun prinst: (x)		;print string with line breaking between words
    (cond	((< (stringlength x) (chrct t))	;if room on line,
	 (princ x))			;print it
	(t				;otherwise, find place to break
	 (do ((a (chrct t) (1- a)))		;which is last space before chrct
	     ((or (signp le a) (= (getcharn x a) 40))
	      (and (signp g a) (princ (substr x 1 (1- a))))	;print part on this line
	      (terpri)			;and on next line,
	      (prinst (substr x (1+ a))) )))))	;the rest

(defun prinb: (x nlevels atom-cnt)		;print with limited output - for printmes
	(cond ((atom x) (prin-atom x) (setq atom-cnt (1- atom-cnt)))
	      ((zerop nlevels) (princ "(...)")	;suppress if too deep in nesting
	       (setq atom-cnt (1- atom-cnt)))	;count as atom since takes up space on printout
	      (t (princ "(")		;output a list...
		(catch (map '(lambda (x) (cond ((zerop atom-cnt) (princ "...")	;if end of output,
					  (throw nil))			;tell user & leave
					 (t (setq atom-cnt (prinb (car x) (1- nlevels) atom-cnt))
					    (and (cdr x) (tyo 40)		;if more, space
					         (atom (cdr x))
						(progn			;dotted pair
						   (princ ". ")
						   (cond ((zerop atom-cnt) (princ "..."))
						         (t (prin-atom (cdr x)) 
							  (setq atom-cnt (1- atom-cnt))))
						   (throw nil) )))))
			x))
		(princ ")") ))
	atom-cnt)	;must return this to caller, so he can update his copy.



(defun prin-atom: (x)		;routine to print an atom for printmes - knows about renaming.
    ((lambda (y)
	(and y (progn		;x is renamed version of y
		(and barfp	;in debug mode,...
		     (princ x)	;explain what's going on.
		     (princ '=))
		(setq x y))))	;and change atom to print to user's name for it
       (get x 'rename))
     (prin1 x))

(defun put-in-tree: (x)
    (push (cons nil x) functions-defined))

(defun nargs: (name) 
    ((lambda (n) (and n (numberp (cdr n)) (cdr n)))
	(cond ((get name 'args))	; if we put an args property on, get it now.
	      ((getl name '(*fexpr *lexpr *expr *array)) nil)	; if user declared the function, don't look at system args 	\prop.
	      ((sysp name) (args name))) ))		; if system function, get its args property.

(defun ckargs (name fl force)			;check out args prop
       (let ((n (nargs name)))
	  (cond ((null n) (putprop name (cons nil fl) 'args))
	        ((= n fl))
	        (force (warn name "has been previously used with the wrong number of arguments")
		     (putprop name (cons nil fl) 'args))
	        (t (barf name "wrong number of args" data)))))


(defun wrntyp: (name def-prop prev-prop-cons) 
    (cond ((not (eq t (cadr prev-prop-cons)))	;if it was explicitly declared
	 (warn (list (car prev-prop-cons) name) "declaration does not agree with definition."))
	((warn (list def-prop name) "declaration required because this function is referenced before it is defined.")))
    (lremprop name '(*expr *fexpr *lexpr args) ))

(defun lremprop: (name l) (mapc '(lambda (x) (remprop name x)) l))



	; functions for declaring variables and specifying compiler options

;;;function to cause the compiler to stop at various places:
;;;	nil	stop nowhere
;;;	t	stop everywhere
;;;	f00	stop while compiling f00
;;;	(a b c)	stop while compiling one of a, b, c.

(defun pause: (x)
    (cond	((null x) (setq pause nil pause-at nil))
	((eq x t) (setq pause t))
	((atom x) (setq pause-at (list x)))
	(t (setq pause-at x))))

(defun macros: (x) (setq macros x))		;copy macro definitions into defsubr file
(defun noargs: (x) x)			;dummy, for compat.
(defun mapex: (x)
    (cond (x t)					;mapex t is ok.
          ((warn nil "(mapex nil) is not supported."))))	;but not mapex nil
(defun system-file: (x) (setq system-file x))
(defun expr-hash: (x) (setq expr-hash x))
(defun nocompile: (x) (setq nocompile x))
(defun symbols: fexpr (x) x)			;dummy, for compat.
(defun genprefix: fexpr (x) (setq genprefix (exploden (car x))))
					;set prefix for generated fcn names, initially "!g"
(defun *declare: (x y)
       (do x x (cdr x) (null x)
	 (and (memq (car x) undfuns)(setq undfuns (delq (car x) undfuns)))
	 (putprop (car x) 'dcl y)))

(defun special: fexpr (x) (*declare x 'special))
(defun unspecial: fexpr (x)   (do x x (cdr x) (null x)  (remprop (car x) 'special)))
(defun reducible: fexpr (x) (*declare x 'reducible))
(defun *reducible: fexpr (x) (mapc '(lambda (x) (putprop x 'system 'reducible)) x) x)	;for default reducible fcns
(defun irreducible: fexpr (x) (mapc '(lambda (x) (remprop x 'reducible)) x) x)
(defun *expr: fexpr (x) (*declare x '*expr)
	(*unarith x))
(defun *fexpr: fexpr (x) (*declare x '*fexpr)
	(*unarith x))
(defun *lexpr: fexpr (x) (*declare x '*lexpr)
	(*unarith x))
(defun **array: fexpr (x) (*declare x '*array)
	(*unarith x))

(defun *unarith: (x)
    (mapc '(lambda (x)
	   (remprop x 'numfun))
	x)
    x)


;;;declarations for the "number compiler."

(defun fixnum: fexpr (x) (*arith x 'fixnum))
(defun flonum: fexpr (x) (*arith x 'flonum))
(defun notype: fexpr (x) (*arith x nil))

(defun *arith: (list prop)	;makes number dcls
    (mapc '(lambda (item)
	    (cond	((atom item)
		 (putprop item prop 'number))
		((and (eq prop 'notype)	;check for undeclaration
		      (null (cdr item)))
		 (remprop (car item) 'numfun))
		(t	;declaration of function
		  (putprop (car item)
			 (cons prop
			       (subst nil 'notype (cdr item)))
			 'numfun) )))
	list)
    t)


(defun messioc: fexpr (f) (setq messioc (list (car f))))
(defun check: (x) (setq check x))
(defun debug: (x)			;set flags for debugging compiler
	(setq dataerrp x barfp x pause x)
	(setq errset (and x '(lambda (args) (break errset t))))
	(*rset x)
	(nouuo x)
	(and x (sstatus uuolinks))	;debug t => want to be able to baktrace
	  )


(defun fixsw: (x) (and (setq fixsw x) (setq flosw nil)) x)		;can't both be on
(defun flosw: (x) (and (setq flosw x) (setq fixsw nil)) x)		;can't both be on

(defun eoc-eval (x) (setq eoc-eval (cons x eoc-eval)))

(defun array*: fexpr (x)	;for declaring "number arrays"
    (mapc '(lambda (x)
	   ((lambda (type dcls)
		(or (memq type '(fixnum flonum notype))
		    (barf x "bad array declaration" data))
		(do ((dcl dcls (cdr dcl)) (ndims) (array))
		    ((null dcl))
		    (cond ((atom (car dcl))		;old style
			 (setq array (car dcl) dcl (cdr dcl) ndims (car dcl))
			 (and (or (not (numberp ndims)) (not (pnamep array)))
			      (barf x "bad array* declaration" data))
			 )
			(t			;new style
			 (setq array (caar dcl) ndims (length (cdar dcl)))
			 (putprop array
				(mapcar '(lambda (x)
					(and (numberp x) x))
				        (cdar dcl))
				'array*)))
		    (putprop array 'dcl '*array)
		    (putprop array (cons nil ndims) 'args)
		    (apply type (list (cons array (n-fixnums ndims))))	;do fixnum, flonum, or notype declaration (hack)
		    )) ;end do, lambda
		(car x) (cdr x))) ;end lambda
	x)	;end mapc
    t) ;end defun

(defun n-fixnums: (n)	;make list of n repetitions of 'fixnum
    (do ((i n (1- i)) (x nil (cons 'fixnum x)))
        ((zerop i) x) ))

(defun arith: fexpr (x)	;declares intrinsic functions of general arithmetic kind to be of specific kind
			;e.g. declares add1 to be like 1+
    (mapc '(lambda (x)
	   (or (memq (car x) '(fixnum flonum notype))
	       (barf (car x) "bad type in arith declaration" data))
	   (do ((item (cdr x) (cdr item)) (tem))
	       ((null item))
	    (cond ((eq (car x) 'notype)
		 (remprop (car item) 'arith))
		((setq tem (assq (car item) fixfns))
		 (putprop (car item) (cdr tem) 'arith))
		((setq tem (assq (car item) flofns))
		 (putprop (car item) (cdr tem) 'arith))
		((warn (car item) "non-arithmetic function in arith declaration")) )))
	x)
    t)

;;;more silly number declarations

(defun closed: (x) (setq closed x))
(defun muzzled: (x) (warn nil "muzzled declaration not implemented") x)

(defun defpl1: fexpr (x)	;the defpl1 declaration compiles a "pl1 subr"
  (do ((args nil)		;list of gensyms for input/update args
       (temps nil)		;list of gensyms for return args
       (values nil)		;initial-values list for temps
       (*unmkd-push 0)	;number of words used on unmkd pdl
       (*unpack-ptrs nil)	;list of unmkd cell, arg gensym to unpack into it
       (*pack-ptrs nil)	;list of unmkd cell, gensym to be packed out of it
       (*c*ret nil)		;cons of gensym and ret char (*) cell addr
       (fn (car x))		;lisp name of the function
       (extname (cadr x))	;PL/I name of the function
       (setqs nil)		;list of temp gensym, special var to setq to it
       (results nil)	;list of temp gensyms to be returned
       (argdescs nil)	;list of 4-lists to be passed on to pass 2
			;describing each arg
       (ermsg)
          )()
    (setq undfuns (delq (setq current-function fn) undfuns))
    (and (equal extname "") (setq extname (get_pname fn)))
    (or (stringp extname) (barf extname "is not a valid external name - defpl1" data))
    (and
     (setq ermsg (catch		;catch barfage
     (do ((x (cddr x) (cdr x))	;map down the arg dcl's
	(z (gensym) (gensym))	;name for arg/temp of this argument
	(argtype nil nil)		;how passed
	(datatype nil nil)		;lisp datatype
	(datalength nil nil)	;precision or length
	(arraytype nil nil)		;nil if scalar, ndims if array
	(descrip))			;PL/I descriptor image
         ((null x) nil)
	  (do ((xl (car x) (cdr xl))	;map down arg attributes
	       (last? (cdr x))
	       (x))		;x is an attribute
	      ((null xl))
	    (setq x (car xl))
	    (cond	((or (eq x 'update) (eq x 'return))
		 (and (or argtype datatype) (throw "update or return attribute must come first - defpl1"))
		 (setq argtype x x (cadr xl))	;check for options on how to return
		 (cond ((eq x 'ignore) (setq xl (cdr xl)))
		       ((and (not (atom x)) (eq (car x) 'setq))
			(setq xl (cdr xl))
			(push (cadr x) setqs)
			(push z setqs))
		       ((push z results))))
		((memq x '(fixed float))
		 (and (or (eq (cadr xl) 'binary) (eq (cadr xl) 'bin))
		      (setq xl (cdr xl)))	;gobble up bin after fixed
		 (cond ((atom (cadr xl))	;no more stuff?
		        (setq datalength (cond ((eq x 'fixed) 17.)
					 (t 27.))))
		       (t			;accept precision attribute
		         (setq datalength (caadr xl) xl (cdr xl))
			      (and (or (not (smallnump datalength))
				     (< datalength 1)
				     (> datalength (cond ((eq x 'fixed) 35.)
						     (t 27.) )))
				 (throw "incorrect arithmetic precision specified - defpl1" barf))))
		 (setq datatype (cond ((eq x 'fixed) 'fixnum)
				  (t 'flonum)))
		 (setq descrip (logor datalength
				  (lsh (cond ((eq x 'fixed) 1) (3))
				       29.))))
		((memq x '(packed-pointer packed-ptr))
		 (setq datatype 'fixnum descrip (lsh 27. 28.)))
		((memq x '(pointer ptr))
		 (setq datatype 'pointer descrip (lsh 26. 28.))
		 (setq *unmkd-push (+ 2 *unmkd-push))	;expansion space
		 (or (eq argtype 'return)	;if input, unpack
		     (progn (push z *unpack-ptrs)
			  (push (- *unmkd-push) *unpack-ptrs)))
		 (and argtype		;if output, pack
		     (progn (push z *pack-ptrs)
			  (push (- *unmkd-push) *pack-ptrs))))
		((eq x 'bit)	;bit string = fixnum
		 (and (or (atom (cadr xl))	;pick up length
			(not (smallnump (setq datalength (caadr xl))))
			(> datalength 36.))
		      (throw "incorrect bit string length specified - defpl1" barf))
		 (setq datatype 'fixnum xl (cdr xl)
		       descrip (logor datalength (lsh 19. 29.))))
		((eq x 'aligned) (setq descrip (boole 4 descrip (lsh 1 28.))))	;unpacked
		((eq x 'unaligned) (setq descrip (logor descrip (lsh 1 28.))))	;packed
		((memq x '(character char))
		 (setq xl (cdr xl) x (caar xl))	;fetch length
		 (setq descrip (logor (lsh 21. 29.)
				  (cond ((eq x '*) 77777777) (x))))
		 (cond ((and (eq x '*) (eq argtype 'return))
		        (and last?
			   (throw "defpl1- returns char (*) must be last parameter" barf))
		        (setq datatype 'ret-char-*)
		        (setq *unmkd-push (+ 4 *unmkd-push))
		        (setq *c*ret (cons z (- *unmkd-push))))
		       ((eq datatype 'varying-string)
		        (or (numberp x)
                                (throw "for char varying a length must be specified" barf))
                            (setq datalength x))
                           (t (setq datatype 'string datalength x)) ))
                    ((eq x 'varying)
                     (cond ((eq datatype 'string)
                            (setq datatype 'varying-string)
                            (or (numberp datalength)
                                (throw "for char varying a length must be specified" barf)))
                           (t (setq datatype 'varying-string))))
		((eq x 'lisp)		;raw lisp object
		 (setq datatype 'lisp descrip 010000000107))	;looks like fixed bin(71)
		((eq x 'array)		;array of other frobs
		 (and argtype (throw "arrays are passed by reference and so may not be update or return - defpl1" barf))
		 (setq xl (cdr xl))		;fetch bounds
		 (setq arraytype (length (car xl))))	;but all we want is ndims
		((throw "unrecognized attribute - defpl1" barf)))		;error - bad word
	    )	;close do

	(setq descrip (logor descrip (lsh 1 35.)))	;turn on high order bit in descriptor
	(cond ((eq argtype 'return)		;put z on proper var list
	       (push z temps)
	       (push (cond  ((eq datatype 'string) (list '*cons-string datalength))
			((eq datatype 'ret-char-*) "")
                              ((eq datatype 'varying-string) "")
			((eq datatype 'fixnum) 0)
			((eq datatype 'flonum) 0.0)
			((eq datatype 'pointer) 0)
			(t nil))
		   values))
	      (t (push z args)))

	; Form an argdesc 4-list out of datatype, datalength, arraytype, and descrip

	(push
	    (cond	((null arraytype)		;scalar
		 (and (memq datatype '(string varying-string))	;a string needs a descriptor cell
		      (setq *unmkd-push (+ 2 *unmkd-push)))
		 (list			;first: addressing type
		    (cond	((eq datatype 'pointer) 'unmkd)	;unpacked ptr on pdl
			((eq datatype 'string) 'string)	;string - make descr
                              ((eq datatype 'varying-string) 'varying-string)  ;varying string make it
			((eq datatype 'lisp) nil)		;lisp object - addr
			((eq datatype 'ret-char-*) 'ret-char-*)
			(t '1+))				;number - addr + 1
		    z			;second: the local var involved
                        (cond ((eq datatype 'varying-string)
                                datalength)
                              (t descrip))	;third: the descriptor image
		    			;fourth: unmkd pdl cell
		    (cond ((eq datatype 'pointer)	;pdl cell involved
			 (do x (append *unpack-ptrs *pack-ptrs)
			       (cddr x) (null x)
			     (and (eq (cadr x) z) (return (car x)))))
			((eq datatype 'ret-char-*)(cdr *c*ret))
			(t (- *unmkd-push)))))
		(t			;array
		 (setq *unmkd-push (+ *unmkd-push	;alloc space for array descriptor
				  (* 2 (// (+ 2 (* 3 arraytype))
					 2))))
		 (list 'array		;first: addressing type
		       z			;second: local var
		       (logor		;third - typeword image
			(lsh (cdr (or (assq datatype '((fixnum . 2) (flonum . 3) (lisp . 0)))
			              (throw "non-arrayable type - defpl1" barf)))
			     18.)
			arraytype)
		       (- *unmkd-push) )))	;fourth: the pdl cell
	    argdescs)	;end of moby push
	)		;end of mobyier do
	barf))		;end of catch
     (barf x ermsg data))

    ; cons up the expression for the interface function and feed it to the compiler

    (setq *unmkd-push (+ *unmkd-push (+ 2 (* 4 (length argdescs)))))	;alloc space for argument list
    (compile-fcn
             (setq being-compiled fn)	;name
	   'expr			;type
	   (list 'lambda (reverse args)  ; the form
	     (cons (append
	        (list 'lambda (reverse temps)	;lambda-bind the result temps
						;without generating any code
		(list '*unmkd-push *unmkd-push)	;allocate unmkd pdl space
		(and *unpack-ptrs 		;prepare any unpacked pointer arguments
		     (cons '*unpack-ptrs (nreverse *unpack-ptrs)))
		(list* (cond (*c*ret '*pl1call-nopop)(t '*pl1call))
				;make the call
		       (- *unmkd-push)	;(where to put arg list)
		       extname
		       (nreverse argdescs))
		(and *pack-ptrs (cons '*pack-ptrs (nreverse *pack-ptrs)))	;prepare any unpacked pointer results
		(and *c*ret (list '*rcv-char-* (car *c*ret)(cdr *c*ret)))
		(list '*unmkd-pop *unmkd-push)	;clear unmarked pdl
		(and setqs (cons 'setq (nreverse setqs))))	;setq special-var results
		args				;fake out compiler
		temps				;unused variable message
		(cond ((= (length results) 1) results)	;return result, nil, or list of results
		      (t (ncons (cons 'list (nreverse results))))))
	        (nreverse values)))			;rest of lambda-binding of temps
	   nil)		;last arg to compile is initial rnl
    (setq being-compiled '(declare))
  )		;end of do to bind variables
  ) ;end of defun defpl1

; functions stolen from pass 2

(defun lsub: (l ll) 
    (cond ((null ll) l)
          (((lambda (dev) (luz l)) ll))))

(defun luz: (x)		;list difference x - dev
    (cond ((null x) nil)
	((memq (car x) dev) (luz (cdr x)))
	((cons (car x) (luz (cdr x))))))

(defun bool1able: (x) 		;Note:  numberp, fixp, floatp should be bool1able when they return t. <, >?
    (and (not (atom x))
         (or (memq (car x) '(and or null not eq = zerop cond memq 
				signp plusp minusp atom stringp subrp))
	   (and (eq (car x) 'progn)
	        (bool1able (car (last x))))
             (and (eq (car x) 'prog2)
                  (null (cdddr x))
                  (bool1able (caddr x))))))

(defun add: (x y) (cond ((memq x y) y) (t (cons x y))))

(defun ladd: (l ll)
    (cond ((null l) ll)
          ((ladd (cdr l) (add (car l) ll)))))


;;;declare numeric-result properties of system functions.

(fixnum (*) (+) (-) (//) (1+) (1-)  (\) (\\))
(fixnum (CtoI))
(fixnum (absfix/!))
(fixnum (boole))
(fixnum (chrct) (linel) (pagel) (linenum) (pagenum) (charpos))
(fixnum (filepos))
(fixnum (flatc) (flatsize))
(fixnum (index))
(fixnum (isqrt))
(fixnum (length))
(fixnum (lsh))
(flonum (fsc))
(fixnum (ifix flonum))
(fixnum (random))
(fixnum (rot))
(fixnum (stringlength))
(fixnum (sxhash))
(fixnum (runtime))
(fixnum (tyipeek) (listen))	;can't put in tyi due to end of file kludge
(flonum (*$) (+$) (-$) (//$) (1+$) (1-$))
(fixnum (^ fixnum fixnum))
(flonum (^$ flonum flonum))
(flonum (abs$))
(flonum (atan))
(flonum (cos) (sin))
(flonum (exp))
(flonum (expt$))
(flonum (float))
(flonum (log))
(flonum (time))
(flonum (sqrt))
(fixnum (getcharn))

;;;declare types of system variables

(fixnum base ibase)



(declare (eval (read)))
    (sstatus macro /: nil)

;at this point in loading of .defs file, initialize global variables

(initialize)
(defun cv-date- (x)
    (do ((ml '(January February March April May June July August September
	     October November December)(cdr ml))
;;         (yr (+ 1900. (car x)))
         (yr (car x))
         (mx 1 (1+ mx))
         (dy (caddr x)))
        ((= (cadr x) mx)
         ((lambda (base *nopoint)
                  (catenate (car ml) " " (maknam (exploden dy)) ", "
			(maknam (exploden yr))))
	10. t))))
(setq compiler-version (catenate "Multics LISP Compiler, Version " compiler-revision ", " (cv-date- (status date))))


;;; Hack by BSG 1/20/80 inspired by JONL's in COMPLR, to keep
;;; compiler bootstrap history; but mine will live and grow for eternity.

(declare (defun get-compiler-history-variable (x)
	      (let ((obarray obarray))
		 (use c)
		 (let ((var (intern (copysymbol x nil))))
		      (cond ((boundp var)(symeval var))
			  (t nil)))))
         (read))
 ;;interpreter only
(defun get-compiler-history-variable (x) 'Interpreter)


(defun semant-signature-history-macro macro (x)	;backquoted 4/24/81
  `(setq semant-compile-date
         '(,(status date) ,(status daytime))
         semant-compiler-history
         ',(or (get-compiler-history-variable 'compiler-history)
	     (get-compiler-history-variable 'compiler-version))))

(semant-signature-history-macro)

(declare (special compiler-history compiler-version
	        semant-compile-date semant-compiler-history
	        cg-compile-date cg-compiler-history))

(defun record-compiler-history ()
       (mapc '(lambda (x)(or (boundp x)(set x '??)))
	   '(semant-compile-date semant-compiler-history cg-compile-date cg-compiler-history))
       (setq compiler-history
	   `(,compiler-version		;This save date
	     (lcp_semant_
	       ,(history-date-encode semant-compile-date)
	       ,semant-compiler-history)
	     (lcp_cg_
	       ,(history-date-encode cg-compile-date)
	       ,cg-compiler-history))))

(defun history-date-encode (triplet-list)
       (let ((l (mapcar '(lambda (triplet) (+ (* 64. 64. (car triplet))
				      (* 64. (cadr triplet))
				      (caddr triplet)))
		    triplet-list)))
	  (cons (car l)(cadr l))))
