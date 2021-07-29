;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;;  ************************************************************
;;  **** MACLISP **** LISP FUNCTION TRACING PACKAGE (TRACE) ****
;;  ************************************************************
;;  * (C) COPYRIGHT 1974 MASSACHUSETTS INSTITUTE OF TECHNOLOGY *
;;  ***** THIS IS A READ-ONLY FILE!  (ALL WRITES RESERVED) *****
;;  ************************************************************

;; Trace package now works in both Multics and PDP-10 lisp.


;; REVISIONS:
;;  45		(Rick Grossman, 12/74)
;;	Replace the trac1 template with compilable code.
;;	Flush trprint in favor of new trace-printer.
;;	Make trace, remtrace, untrace compilable.
;;	Improve trace-edsub so that this will work:
;;	 (trace y (x wherein y)), and similarly untrace.
;;	 Note that (trace (y wherein z) (x wherein y))
;;	 still partially loses.
;;	Have untrace return only the list of actually
;;	 previously traced functions.
;;  46		(Rick Grossman, 1/75)
;;	Add trace-indenter as default print function.
;;	Fix bug:  (.. value ..) also printed the arg.
;;	Put "break" condition within scope of the "cond" one.
;;	Fix bug:  (trace (foo cond bar1 value)) lost
;;	 because trace*g4 was referenced in "value"
;;	 but never set.
;;	Fix bug:  If FEXPR or MACRO is an atom, loses.
;;	Clean up some of the duplicate trace-1 code.
;;	Add TRACE-OK-FLAG to prevent tracing calls by trace.
;;	Flush definition of PLIST.
;;	Change ADD1 to 1+.
;;	Replace MIN with open-compilable COND.
;;	Flush excess consing in trace-indenter call.
;;  50		(JONL, 1/75)
;;	Try to merge Moons hackery with Grossman's latest stuff
;; 	Add function BREAK-IN
;;	Fix bug in TRACE-INDENTER s.t. if TRACE-INDENTATION
;;	 ever goes to zero, then simply skip indentation.
;;  51		(JONL, 2/75)
;;	Use the PRIN1 variable in TRACE-INDENTER.
;;  52		(GROSS, 2/75)
;;	Lambda-bind TRACE-INDENTATION (and use a gensym name).
;;  53		(MOON Feb. 25, 1975)
;;	Take break out from control of cond, dammit!!
;;	This is the only way to break on condition without
;;	printing a lot of garbage; also it's a documented feature.

;; Note:  When adding new functions to this file,
;;	  be sure to put their names in the list in REMTRACE.


(%include backquote)


(declare 
 (macros nil) 
 (mapex t)
 (setq nfunvars t)
 (special trace-olduuo traced-stuff prin1
  trace*g1 trace*g2 trace*g4 trace*g5
  trace*copies trace*subr-args trace-printer trace-ok-flag
  trace-break-fun trace-indent-incr trace-indent-max)
 (fixnum ng)
 (*fexpr trace untrace))

(defun macex macro (x) 
       (list 'defun (cadr x) 'macro (caddr x) 
	   (eval (cadddr x))))

(macex newlineseq (x)
       (cond
         ((status feature Multics) ''(list (ascii 10.))) 
         (t ''(list (ascii 13.) (ascii 10.)))))
 

(macex version (x) 
       (subst
         (maknam (nconc (newlineseq) 
		    (explodec '/;loading/ trace/ ) 
		    (explodec
		      (cond
		        ((status feature newio) (caddr (names infile))) 
		        ((cadr (status uread)))))
		    (newlineseq)))
         'version
         ''(or (status feature noldmsg)
	     (iog vt (princ 'version) (ascii 0.)))))


(version)


(and (getl 'remtrace '(fsubr fexpr)) (remtrace)) 

(and (not (boundp 'trace-printer))
 (setq trace-printer 'trace-indenter) ) 
(and (not (boundp 'trace-break-fun))
     (setq trace-break-fun 'break))

(setq trace-olduuo nouuo traced-stuff nil trace-ok-flag t) 
;; The flag  trace-ok-flag  is bound NIL inside all trace fns.

(sstatus feature trace) 

(setq
  trace*subr-args
  (list (gensym) (gensym) (gensym) (gensym) (gensym))
  trace*g1 (gensym) trace*g2 (gensym)
  trace*g4 (gensym) trace*g5 (gensym))

;; Initial indentation.
(set trace*g5 0)



;; Define remtrace first in case the loading does not finish.

(defun remtrace ()
       (prog (trace-ok-flag y) 
	   (errset (untrace)) 
	   (mapc '(lambda (x) 
		        (do nil
			  ((null (setq y (getl x '(expr fexpr subr fsubr)))))
			  (remprop x (car y)) ) ) 
	         '(trace untrace remtrace untrace-1 trace-edsub
		       trace-indenter break-in ) ) 
	   (nouuo trace-olduuo) 
	   (sstatus nofeature trace) 
	   (gctwa)))


(defun untrace fexpr (l) 
       (prog (trace-ok-flag) 
	   (cond
	     (l (setq l (mapcan '(lambda (x) (and (untrace-1 x) (list x))) l)))  
	     ((setq l (mapcar 'untrace-1 (trace))) 
	      (and traced-stuff (progn (print 'lossage) (print (trace))))))
	   (and (null traced-stuff) (nouuo trace-olduuo))
	   (return l)))   


(defun untrace-1 (x) 
  (prog (y ret) 
    a 	(cond ((null (setq y (assoc x traced-stuff))) (return ret)) 
	      ((atom (car y)) 
		(and (eq (get (car y) (caddr y)) (cadddr y)) 
		     (remprop (car y) (caddr y)))) 
	      (t (trace-edsub (cons (caddr y) (caar y)) 
			      (caddar y) 
			      (cadr y)))) 
	(setq traced-stuff (delq y traced-stuff)) 
	(setq ret x)
	(go a))) 


(defun trace-edsub (pair sym ind)
       (prog (y z) 
	   ;; Return NIL if lose.
	   (and (setq y (assq sym traced-stuff)) 
	        (eq ind (caddr y)) 
	        (setq z (getl sym (list ind))) 
	        (eq (cadddr y) (cadr z)) 
	        ;; We want to munge the original definition,
	        ;; not the trace kludgery.
	        ;; Note that this partially loses for traced macros,
	        ;; since we munge the macro property, not the
	        ;; trace-generated fexpr one.
	        (setq sym (cdr z)) ) 
	   (return
	     (cond 
	       ((setq y (get sym ind)) 
	        (putprop sym (sublis (list pair) y) ind) ) ) ) )) 




;; Define the code to produce the trace stuff.
 


(defun trace-1 macro (dummy) 
       '(let ((t1 nil)
	    (in-vals nil))
	   (sublis trace*copies
		 `(lambda ,(cond (c) (gg) (g (car g)) (trace*g1))
			((lambda
			   (,trace*g2
			     ,trace*g1
			     ,@(cond ((null q) `(,y)))
			     ,@(cond (f `(,trace*g4)))
			     ,@(cond (p `(,p)))
			     ,@(cond ((eq print 'trace-indenter)`(,trace*g5))))
			   ,@(and f `((setq ,trace*g4 ,(car f))))
			   ,@(cond 
			       ((or ne (memq (car m) '(arg both))) 
			        (setq t1 (cond
				         ((eq print 'trace-indenter) 
					`(,print ,y 'enter ',x 
					         ,(cond
						  ((memq (car m) '(arg both)) trace*g2) 
						  (t `',trace*g2))
					         ,(and (or n ne)
						     `(list ,@ne ,@n))
					         ,trace*g5))
				         (t `(,print 
					     (list ,y
						 'enter
						 ',x
						 ,@(cond
						     ((memq (car m) '(arg both)) 
						      `(,trace*g2)))
						 ,@ne
						 ,@n)))))
			        (cond
				((or f fe) 
				 ;; There is a COND or ENTRYCOND
				 `((and
				     ,@(and f `(,trace*g4))
				     ,@(and fe `(,(car fe)))
				     ,t1)))
				(t `(,t1))))) 
			   ,@(and break
				`(,(cond ((eq trace-break-fun 'break)
					`(break ,y ,break))
				         (t `(and ,break (,trace-break-fun ',x))))))
			   ,(cond (q `(apply ',y ,trace*g2))
				(t `(setq ,trace*g1 (apply ',y ,trace*g2))))
			   ,@(cond ((and (null q)
				       (or nx (memq (car m) '(value both)))) 
				  (setq t1
				        (cond
					((eq print 'trace-indenter) 
					 `(,print ,y 'exit ',x 
						,(cond
						   ((memq (car m) '(value both)) trace*g1) 
						   (t `',trace*g2))
						,(and (or n nx)
						      `(list ,@nx ,@n))
						,trace*g5)) 
					(t `(,print
					      (list ,y
						  'exit
						  ',x
						  ,@(cond
						      ((memq (car m) '(value both)) 
						       `(,trace*g1)))
						  ,@nx
						  ,@n)))))
				  (cond
				    ((or f fx) 
				     ;; There is a COND or EXITCOND
				     `((and
				         ,@(and f `(,trace*g4))
				         ,@(and fx `(,(car fx)))
				         ,t1)))
				    (t `(,t1)))))
			   ,@(cond ((null q) `(,trace*g1))))
			 ;; lambda args
			 ,(setq in-vals
			        (cond
				(c (car c)) 
				(gg `(listify ,gg)) 
				(g `(list ,@(car g))) 
				(t `(listify ,trace*g1))))
			 nil
			 ,@(cond ((null q) `((1+ ,y))))
			 ,@(cond (f `(nil)))
			 ,@(cond
			     ;;ARGPDL stuff
			     (p `((cons (list ,@(cond ((null q)
						 `((1+ ,y))))
					  ',y
					  ,in-vals)
				      ,p))))
			 ,@(cond ((eq print 'trace-indenter)
				`((+ ,trace*g5 trace-indent-incr)))))))))



;;	c is non-nil for f-type, holds lambda list 
;;	 cm = (MACRO (LAMBDA ...) ...) if macro.
;;	g is non-nil for expr type, (car g) is lambda list ;
;;	not c or g => l-form
;;	 gg = lexpr variable (if nil, is lsubr).
;;	q if non-nil means the function is go, throw, etc.,
;;	 so no return values (etc.) will be hacked.

;;	n holds list of extra quantities for typeout

;;	traced-stuff =
;;		list of currently traced stuff, typically
;;		((a 'trace 'expr newexpr) ...)
;;		(((a 'wherein b) 'expr g0003) ...)

;;	x = tracee
;;	y = new symbol for tracee
;;	m = (BOTH/ARGS/VALUE/NIL . stuff-to-print)
;;	Keyword values:
;;	 f:	COND
;;	 fe:	ENTRYCOND
;;	 fx:	EXITCOND
;;	 p:	ARGPDL
;;	 break:	BREAK
;;	 b:	(foo WHEREIN bar)
;;	 ne:	ENTRY
;;	 nx:	EXIT

;; Obscure functions:
;;	qu*	Expand a quoted list, hacking:
;;		(EV frob)	eval the frob, & use result;
;;		(EV* frob)	eval, & splice the result in.
;;
;;	trace-edsub	(pair atom ind):  Do sublis on the
;;					atom's property.
;;		This is used for WHEREIN substitution.


(defun break-in fexpr (l)
  (apply 'trace 
	 (mapcan '(lambda (x) (list (cons x '(break t))))
		 l)))

(defun trace fexpr (l) 
 (cond
  ((null l) (mapcar 'car traced-stuff)) 
  (t
   (prog2 nil
    (mapcan 
     '(lambda (c)
       (prog 
        (x y g gg n ne nx m break f fe fx b p q cm sube
         print getl trace-ok-flag ) 
        (setq print trace-printer) 
        (cond
         ((atom c) (setq x c c nil)) 
         (t
          (setq x (car c)) 
          (setq c (cdr c)) 
          (or (atom x)
	   ;; hack list of functions
           (return (mapcar (function (lambda (x) 
               (car (apply 'trace (list (cons x c)))) )) 
             x )) ) ) )  
        (or
         (setq getl
           (getl x '(fexpr fsubr expr subr lsubr macro)) ) 
         (return (ncons (list '? x 'not 'function))) ) 
        (or (atom (cadr getl)) (eq (caadr getl) 'lambda) 
         (return
          (ncons (list '? x 'bad (car getl) 'definition)) ) )  
        (go y) 
        l
        (setq c (cdr c)) 
        l1
        (setq c (cdr c)) 
        y
        (cond
         ((null c) (setq m '(both)) (go x)) 
         ((eq (car c) 'grind) 
          (setq print 'sprinter) (go l1) ) 
         ((eq (car c) 'break) 
          (setq break (cadr c)) 
          (go l) ) 
         ((eq (car c) 'cond) 
          (setq f (cdr c)) 
          (go l) ) 
         ((eq (car c) 'entrycond) 
          (setq fe (cdr c)) 
          (go l) ) 
         ((eq (car c) 'exitcond) 
          (setq fx (cdr c)) 
          (go l) ) 
         ((memq (car c) '(arg value both nil)) 
          (setq m c)
	  (go x) ) 
         ((eq (car c) 'wherein) 
          (cond
           ((or (not (atom (cadr c))) 
             (null
              (setq y
               (getl (cadr c) '(expr fexpr macro)) ) ) ) 
            (go wherein-loss) ) )  
          (untrace-1 (setq g (list x 'wherein (cadr c)))) 
          (setq traced-stuff
           (cons
            (list g
             (car y) 
             (setq n (copysymbol x nil)) ) 
            traced-stuff ) ) 
	  (setplist n (plist x))
	  (or
           (trace-edsub (cons x n)
	    (cadr c)
	    (car y))
	   ;; This can lose if the EXPR, FEXPR, or MACRO found
	   ;; above is really a tracing frob!  Hence:
	   (go wherein-loss) )
          (setq b g) 
          (setq x n) 
          (go l) ) 
         ((eq (car c) 'argpdl) 
          (cond
           ((and (setq p (cadr c)) (eq (typep p) 'symbol)) 
            (set p nil) 
            (go l) ) 
           ((return (ncons (list '? 'argpdl p)))) ) ) 
         ((eq (car c) 'entry) 
          (setq ne (cons ''/|/| (cadr c))) 
          (go l) ) 
         ((eq (car c) 'exit) 
          (setq nx (cons ''/|/| (cadr c))) 
          (go l) ) 
         ((return (ncons (list '? (car c))))) ) 
	wherein-loss
	(return (ncons (list '? 'wherein (cadr c))))
        x
	(untrace-1 x) 
        (cond
         ((setq q (memq x '(go return err throw))) 
          (cond
           ((eq (car m) 'value) 
            (setq m (cons nil (cdr m))) ) 
           ((eq (car m) 'both) 
            (setq m (cons 'arg (cdr m))) ) ) ) ) 
        ;; copy atom in way that works in any lisp.
        (set (setplist (setq y (copysymbol x nil)) nil) 0) 
        ;; transfer property list to new trace atom
        (setplist y (nconc (plist y) (plist x))) 
        ;;
        (setq c
         (cond
          ((memq (car getl) '(fexpr macro)) 
           (cond
            ((atom (cadr getl)) (list trace*g1)) 
            ((cadr (cadr getl))	) ) )  
          ((eq (car getl) 'fsubr) (list trace*g1)) ) ) 
        (setq cm (cond ((eq (car getl) 'macro) getl))) 
        (setq g
         (cond
          ((eq (car getl) 'expr) 
           (cond
            ((atom (setq g (cadr getl))) nil) 
            ((null (cadr g)) (cdr g)) 
            ((atom (cadr g)) 
             (setq gg (cadr g)) 
             nil ) 
            (t (cdr g)) ) ) 
          ((eq (car getl) 'subr) 
           (cond
            ((setq g (args x)) 
	     (setq g (cond ((> (cdr g) 5)
			    (do ((ng (- (cdr g) 5) (1- ng)) 
			         (l trace*subr-args (cons (gensym) l)))
			         ((zerop ng) l)))
	                   ((do ((ng (- 5 (cdr g)) (1- ng)) 
				 (l trace*subr-args (cdr l))) 
	                        ((zerop ng) l)))))
	     (list g)))))) 
	(and
	 ;; For fns called by TRACE itself, suppress tracing.
	 (or (memq x
           '(*append *delq *nconc args assoc assq boundp cons
             copysymbol fixp gctwa get getl last memq apply
             ncons nreverse plist princ print putprop remprop
             setplist sstatus status sublis terpri typep xcons
             trace-indenter sprinter delq error gensym nouuo
	     prin1 ) ) 
          (eq x prin1) ) 
	 (setq f (list
           (cond
            (f (list 'and 'trace-ok-flag (car f))) 
            ('trace-ok-flag)))))
        (setq sube
         (list (cons 'recurlev y) (cons 'arglist trace*g2))) 
        (setq n
         (cond
          ((cdr m) 
           (cons ''// (sublis sube (cdr m))) ) ) ) 
        (setq ne (sublis sube (list ne f fe break))) 
        (setq nx 
         (sublis 
          (cons (cons 'fnvalue trace*g1) sube) 
          (list nx  fx) ) ) 
        (setq 
         f (cadr ne) fe (caddr ne) 
         break (cadddr ne) ne (car ne) ) 
        (setq fx (cadr nx) nx (car nx)) 
        (setplist
         x
         (cons
          (cond
           (cm
            (setplist y 
             (cons 'fexpr (cons (cadr cm) (plist y))) ) 
            'macro ) 
           (c 'fexpr) 
           (t 'expr) ) 
          (cons (trace-1) (plist x)) ) )  
        (return
         (ncons (cond (b) 
	              (t (setq traced-stuff (cons (list x 'trace (car (plist x)) (cadr (plist x))) 
				                  traced-stuff)) 
	                 x)))))) 
     l) 
    (and traced-stuff (nouuo t) (sstatus uuolinks)))))) 



(declare
 (unspecial n) 
 (fixnum indentation trace-indent-incr trace-indent-max
  n recurlev ) ) 


(defun trace-indenter (recurlev type fn arg stuff indentation) 
 (prog (trace-ok-flag) 
   (setq indentation (- indentation trace-indent-incr))
  (terpri) 
  (do n 
   (cond
    ((< indentation 0) 0) 
    ((< indentation trace-indent-max) indentation) 
    (trace-indent-max) ) 
   (1- n) 
   (zerop n) 
   (princ '/ ) ) 
  (princ '/() (prin1 recurlev) (princ '/ ) (prin1 type) 
  (princ '/ ) (prin1 fn) 
  (cond ((not (eq arg trace*g2)) 
    (princ '/ ) 
    (cond ((and (boundp 'prin1) prin1)
           (funcall prin1 arg))
          ((prin1 arg))) )) 
  (do l stuff (cdr l) (null l) 
   (princ '/ ) 
    (cond ((and (boundp 'prin1) prin1)
           (funcall prin1 (car l)))
          ((prin1 (car l))))
 ) 
  (princ '/)/ ) ) )    


(setq 	trace-indent-incr 2. 
	trace-indent-max 16. 
	trace*copies (mapcar '(lambda (x) (cons x (copysymbol x t))) 
			     '(trace-indenter print quote cond list 
				and setq break apply listify))) 

