;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************

;;;   **************************************************************
;;;   ***** Maclisp ****** S-expression formatter (grindef) ********
;;;   **************************************************************
;;;   ** (c) Copyright 1974 Massachusetts Institute of Technology **
;;;   ****** this is a read-only file! (all writes reserved) *******
;;;   **************************************************************

;;;This version of Grind works in both ITS Maclisp and Multics Maclisp
;;; copied from (mc gfn 386).

;;gfn - fns for pretty-printing functions and S-expressions in core.
;;when compiled, uses about  2300.instructions,  950. list cells, 
;;    320. fixnum cells, and 160. symbols.  remgrind applied therein 
;;    will reclaim about 300. list cells, the array space of 
;;    grindreadtble and gtab/|, and very little else.

(declare (array* (notype (gtab/| 128.)))
         (noargs t)
         (special merge readtable grindreadtable remsemi
                  grindpredict grindproperties grindef predict
                  grindfn grindmacro programspace topwidth
                  grindlinct global-lincnt /; /;/; user-paging 
                  arg linel pagewidth gap comspace fill nomerge comnt
                  /;/;? ^d macro unbnd-vrbl cnvrgrindflag form 
		  prog? n m l h grind-standard-quote sgploses)
         (*expr form topwidth programspace pagewidth comspace
                nomerge remsemi prin50com rem/; rem/;/;)
         (*fexpr trace slashify unslashify grindfn grindmacro
                 unreadmacro readmacro grindef)
         (*lexpr merge predict user-paging fill testl)
         (mapex t)
         (genprefix /|gr)
         (or (get 'maknum 'subr) (defun macro maknum (x) (cons '(lambda (x) (abs (sxhash x))) (cdr x))))	;temporary for Multics
         (fixnum nn mm (prog-predict notype fixnum fixnum)
                 (block-predict notype fixnum fixnum) (setq-predict notype fixnum fixnum)
                 (panmax notype fixnum fixnum) (maxpan notype fixnum) (gflatsize)))

(defun macex macro (x) (list 'defun (cadr x) 'macro (caddr x) (eval (cadddr x))))

(defun ifoio macro (x) 
       (cond ((not (status feature newio)) (cadr x)) ('(comment ifoio not taken))))

(defun ifnio macro (x)
       (cond ((status feature newio) (cadr x)) ('(comment ifnio not taken))))

(macex newlineseq (x)
    (cond ((status feature Multics)
	 ''(list (ascii 12)))
	(t ''(list (ascii 15)(ascii 12)))))

(macex version (x) 
        (subst (maknam 
                  (nconc (newlineseq)
                         (explodec '/;loading/ grindef/ )
                         (explodec (cond ((status feature newio) (caddr (names infile)))
                                         ((cadr (status uread)))))
		     (newlineseq)))
                 'version 
                 ''(iog nil (princ 'version) (ascii 0))))

(ifnio (defun newlinel macro (x) (subst (cadr x) 'nn '(setq linel nn))))

(ifoio (defun newlinel (nn)
              (setq chrct (+ chrct (- nn linel)))
              (setq linel nn)))

(ifoio (defun grchrct macro (x) 'chrct))
(ifnio (defun macro set-linel (x) '(setq linel (linel (and outfiles (car outfiles))))))
(ifoio (defun macro set-linel (x) '(comment linel)))


(version)

;;*user-paging

(prog nil 		;some initializations

(and (not (boundp 'grind-use-original-readtable)) 
     (setq grind-use-original-readtable t))
(and (or (not (boundp 'grindreadtable)) (null grindreadtable))
     ((lambda (readtable) (setsyntax 12. 'single nil)                  ;^l made noticeable.
                          (sstatus terpri t)                           ;the grindreadtable is tailored for
                          (setsyntax '/;                               ;grind. no cr
                                     'splicing
                                     'semi-comment))                   ;are inserted by lisp when
      (setq grindreadtable 
            (*array nil 'readtable grind-use-original-readtable))))    ;print exceeds linel.
(and (not (boundp 'grind-standard-quote))                              ;standard readmacroinveser for quote
     (setq sgploses (setq grind-standard-quote t)))
(setq remsemi nil m 0. grindlinct 8. grindef nil global-lincnt 59.)
(setq grindproperties '(expr fexpr value macro datum cexpr))
(and (status sstatus feature) (sstatus feature grindef))
(array gtab/| t 128.)) 


;;debugging break for grind.

(declare (read) (read))                                                ;gbreak restricted to interpretive
                                                                       ;version.

(defun gbreak fexpr (x) 
       (and gbreak                                                     ;break transparent to chrct
            (prog (chrct* ^r) 
                  (setq chrct* (grchrct))
                  (apply 'break
                         (cond ((null x) '(grind t))
                               ((list x t))))
                  (terpri)
             a    (cond ((eq chrct* (grchrct)))
                        ((princ '/ ) (go a)))
                  (return t)))) 

(setq gbreak t) 



;;rem function - note: to be complete, remgrind should remprop all grindfn, grindmacro and grindpredict
;;properties from any atom on the obarray.
;;*grindfn (expr fexpr lexpr macro value grindpredict) comment-form


(defun remsubr (x) (remprop x 'subr)) 

(defun remfsubr (x) (remprop x 'fsubr)) 

(defun remlsubr (x) (remprop x 'lsubr)) 

(defun remgrind fexpr nil 
       (lispgrind)
       (cond ((status sstatus nofeature) (sstatus nofeature grind) (sstatus nofeature grindef)))
       (cond ((null (get 'conniver 'array))
              (remsubr 'grindexmac)
              (remsubr 'grindatmac)
              (remsubr 'grindcolmac)
              (remsubr 'grindcommac)
              (remsubr 'grindseparator)
              (remsubr 'grindnxtchr)))
       (remfsubr 'grind)
       (remfsubr 'grind0)
       (remfsubr 'grindef)
       (remsubr 'turpri)
       (remlsubr 'fill)
       (remlsubr 'user-paging)
       (remlsubr 'merge)
       (remlsubr 'testl)
       (remlsubr 'predict)
       (remfsubr 'slashify)
       (remfsubr 'unslashify)
       (remfsubr 'unformat)
       (remfsubr 'grindmacro)
       (remfsubr 'grindfn)
       (remfsubr 'readmacro)
       (remfsubr 'unreadmacro)
       (remfsubr 'readmacroinverse)
       (remsubr 'slashify1)
       (remsubr 'unslashify1)
       (remsubr 'programspace)
       (remsubr 'grindmacrocheck)
       (remsubr '?grindmacro)
       (remsubr 'comment-form)
       (remsubr 'pagewidth)
       (remsubr 'comspace)
       (remsubr 'lispgrind)
       (remsubr 'cnvrgrind)
       (remsubr 'page)
       (remsubr 'topwidth)
       (remsubr 'rem/;)
       (ifnio (remsubr 'newlinel))
       (ifnio (remsubr 'grchrct))
       (remsubr 'rem/;/;)
       (remsubr 'tj6)
       (remsubr 'prin50com)
       (remsubr 'prinallcmnt)
       (remsubr 'semi-comment)
       (remsubr 'putgrind)
       (remsubr 'lambda-form)
       (remsubr 'prog-form)
       (remsubr 'if-form)
       (remsubr 'def-form)
       (remsubr 'coment-form)
       (remsubr 'block-form)
       (remsubr 'mem-form)
       (remsubr 'setq-form)
       (remsubr 'setq-predict)
       (remsubr 'remsem1)
       (remsubr 'remsemi)
       (remsubr 'popl)
       (remsubr 'semi?)
       (remsubr 'semisemi?)
       (remsubr 'indent)
       (remsubr 'indent-to)
       (remsubr 'pprin)
       (remsubr 'form)
       (remsubr 'sprint)
       (remsubr 'grind-unbnd-vrbl)
       (remsubr 'sprinter)
       (remsubr 'sprint1)
       (remsubr 'grindargs)
       (remsubr 'done?)
       (remsubr 'gblock)
       (remsubr 'gprin1)
       (remsubr 'maxpan)
       (remsubr 'panmax)
       (remsubr 'prog-predict)
       (remsubr 'block-predict)
       (remsubr 'gflatsize)
       (remsubr 'flatdata)
       (remsubr 'grindslew)
       (remsubr 'remlsubr)
       (remfsubr 'remgrind)
       (remsubr 'remfsubr)
       (remsubr 'remsubr)
       ((lambda (nn)
		(do mm 0 (1+ mm) (= mm nn)
		   (mapc 
		    '(lambda (x) 
			(cond ((getl x '(grindfn grindpredict grindmacro))
				(remprop x 'grindfn)
				(remprop x 'grindpredict)
				(remprop x 'grindmacro))))
		     ((lambda (x)
			(cond ((and x (atom x)) (ncons x))
			      (x)))
		        (obarray mm)) )))
	  (cadr (arraydims 'obarray)))
       (makunbound 'merge)
       (makunbound 'grindpredict )
       (makunbound 'predict)
       (makunbound 'grindfn)
       (makunbound 'grindmacro)
       (makunbound 'programspace)
       (makunbound 'topwidth)
       (makunbound '/;)
       (makunbound '/;/;)
       (makunbound 'user-paging)
       (makunbound 'pagewidth)
       (makunbound 'comspace)
       (makunbound 'prog?)
       (makunbound 'comnt)
       (makunbound '/;/;?)
       (makunbound 'cnvrgrindflag)
       (makunbound 'remsemi) 
       (makunbound 'grindlinct) 
       (makunbound 'global-lincnt) 
       (makunbound 'grindproperties) 
       (makunbound 'grindef)
       (makunbound 'grindreadtable)
       (makunbound 'grind-standard-quote)
       (makunbound 'grind-use-original-readtable)
       (*rearray 'gtab/|)
       (gctwa)) 


(defun grindef fexpr (atoms)                                           ;(grindef <atoms>) grinds the properties
       (prog (traced fn props)                         ;of the atoms listed on
             (set-linel)
             (cond ((get 'conniver 'array)                             ;"grindproperties". (grindef
                    (or cnvrgrindflag (cnvrgrind))))
             (cond (atoms (setq grindef atoms))                        ;(additional properties) <atoms>) grinds
                   ((setq atoms grindef)))                             ;the additional properties as well.
             (setq props grindproperties)
        a    (cond ((null atoms) (return (ascii 0.))))
             (setq fn (car atoms) atoms (cdr atoms))
             (cond ((atom fn))
                   ((setq props (append fn props)) (go a)))
             (cond ((setq traced (and (cond ((status sstatus feature) (status feature trace))
					    ((get 'trace 'fexpr)))
                                      (memq fn (trace))))              ;flag for fn being traced
                    (terpri)
                    (terpri)
                    (princ '/;traced)))
             (do ((plist (cdr fn) (cddr plist))
                  (ind 'value (car plist))
                  (prop (and (boundp fn) (symeval fn)) (cadr plist))
                  (valueless (not (boundp fn)) t))                        ;needed in case there are value properties
                 (nil)
             (cond ((and traced (memq ind '(expr fexpr macro)))        ;ignore first fn property if traced
                    (setq traced nil)
                    (go b))
                   ((not (memq ind props)) (go b))                     ;grindef only desired properties.
                   ((eq ind 'value)
                    (cond ((not valueless)
                           (terpri)
                           (terpri)
                           (sprint (list 'setq
                                         fn
                                         (list 'quote
                                               prop))
                                   linel
                                   0.)))
                    (go b)))
             (terpri)
             (terpri)                                                  ;terpri's placed here to avoid
             (cond ((eq ind 'theorem)                                  ;terpri'ing when no properties.
                    (sprint (cons (car prop) (cons fn (cdr prop)))
                            linel
                            0.))
                   ((and (memq ind '(expr fexpr macro))                ;lambda -> defun
                         (eq (car prop) 'lambda))
                    (sprint (cons 'defun
                                  (cons fn
                                        (cond ((eq ind 'expr)
                                               (cdr prop))
                                              ((cons ind
                                                     (cdr prop))))))
                            linel
                            0.))
                   ((eq ind 'cexpr)
                    (sprint (cons 'cdefun (cons fn prop))
                            linel
                            0.))
                   ((sprint (list 'defprop fn prop ind)
                            linel
                            0.)))
        b    (or plist (return nil)))   ;exit from do when no more properties
         (go a)               ;look for more atoms to do.
          ))

;;;assigning special formats

(defun unformat fexpr (x)                                              ;(unformat fn1 fn2 ...) or (unformat
       (or (atom (car x)) (setq x (car x)))                            ;(fn1 fn2 ...))
       (mapc '(lambda (x) (remprop x 'grindfn)
                          (remprop x 'grindmacro)
                          (remprop x 'grindpredict))
             x)) 

(defun grindmacro fexpr (y)                                            ;eg (grindmacro quote /')
       (putgrind (car y) (cdr y) 'grindmacro)) 

(defun grindfn fexpr (y)                                               ;eg (grindfn (prog thprog) prog-form)
       (putgrind (car y) (cdr y) 'grindfn)) 

(defun putgrind expr (fn prop ind)                                     ;like putprop
       (cond
        ((atom fn)
         (setq prop
               (cond ((atom (car prop))
                      (and (get (car prop) 'grindpredict)
                           (putprop fn
                                    (get (car prop)
                                         'grindpredict)
                                    'grindpredict))
                      (car prop))
                     (t (and (eq (caar prop) 'readmacroinverse)
                             (putprop fn
                                      (get 'readmacroinverse
                                           'grindpredict)
                                      'grindpredict))
                        (cons 'lambda (cons nil prop)))))
         (putprop fn prop ind))
        ((mapc '(lambda (x) (putgrind x prop ind)) fn)))) 

;;;read macros

(defun readmacro fexpr (y)                                             ;eg (readmacro quote /' [optional])
       (putgrind (car y)                                               ;where optional means macro cons not
                 (list (cons 'readmacroinverse                         ;list
                             (cons (cadr y) (cddr y))))
                 'grindmacro)) 

(defun unreadmacro fexpr (y) (remprop y 'grindmacro)) 

(defun ?grindmacro (x) 
       (prog (y) 
             (cond ((and cnvrgrindflag
                         (setq y (get x 'grindmacro)))
                    (return (list (cddr (caddr y)))))
                   (t (return nil))))) 

(defun grindmacrocheck (x l) 
       (cond ((and (equal x '((t))) (cdr l)))
             ((and (equal x '(nil)) (= (length l) 2.)))
             ((and (equal x '((cnvr-optional))) (cdr l))))) 

(defun readmacroinverse fexpr (x)                                      ;(fn l)--><macro char><pretty-print l>. 
       (prog (sprarg) 
             (cond ((grindmacrocheck (list (cdr x)) l)                 ;macro-char = atom or list of ascii
                    (cond ((atom (car x)) (princ (car x)))             ;values. macro must have arg to execute
                          ((mapc 'tyo (car x))))                       ;inverse
                    (setq sprarg (cond ((null (cdr x)) (cadr l))
                                       ((eq (cadr x) t) (cdr l))
                                       ((= (length (cdr l)) 1.)
                                        (cond ((null (cadr l))
                                               (tyo 32.)
                                               (return t))
                                              (t (cadr l))))
                                       (t (cdr l))))
                    (cond ((sprint1 sprarg (grchrct) m) (prin1 sprarg)))
                    (return t))
                   (t (return nil))))) 

;;predefined formats

(defun lambda-form nil 
       (form 'line)                                                    ;format for lambda's 
       (and (< (grchrct) (gflatsize (testl)))                          ;prohibits form3 if args do not fit on
            (setq form 'form2))                                        ;line.
       (form 'block)) 

(defun prog-form nil 
       (form 'line)                                                    ;format for thprog's and prog's
       (setq prog? t)
       (setq form (cond ((and predict (< (grchrct) (gflatsize (testl))))   ;prohibits form3 if args do not fit on
                         'form2)                                       ;line.
                        (arg)))
       (form 'block)) 

(defun if-form nil
       (setq prog? t)
       (form 'line)
       (cond ((atom (testl)) (form 'line)))
       (setq form (cond ((and predict (< (grchrct) (gflatsize (testl))))
                         'form2)
                        (arg)))
       (form 'list))

(defun def-form nil 
       (prog nil 
             (cond ((eq (car l) 'cdefun) (setq prog? t)))
             (form 'line)
             (form 'line)
        go   (cond ((memq (testl)
                          '(expr fexpr macro thnoassert cexpr))
                    (form 'line)
                    (go go)))
             (setq form
                   (cond ((and predict (< (grchrct) (gflatsize (testl))))  ;prohibits form3 if args do not fit on
                          'form2)                                      ;line.
                         (arg)))
             (return (form 'block)))) 

(defun comment-form nil (gblock (- (grchrct) 1. (gflatsize (car l)))))     ;grinds l with args outputed as list.

(defun block-form nil (gblock (grchrct))) 


(defun mem-form nil 
       (prog (p gm) 
             (form 'line)                                              ;quoted second arg ground as block
             (remsemi)
             (catch (and (setq p (panmax (car l) (grchrct) 0.))
                         (cond ((< (panmax (car l) n 0.) p))
                               ((setq n (grchrct))))))
             (cond ((sprint1 (car l) n 0.) (prin1 (car l))))
        a    (cond ((null (cdr l))
		    (setq l (error 'mem-form l 'fail-act))
		    (go a)))
	     (popl)
        go   (indent-to n)
             (setq m (1+ m))
             (cond ((eq (caar l) 'quote)
                    (princ '/')
                    (cond ((pprin (cadar l) 'block)) ((prin1 (cadar l)))))
                   ((setq gm (sprint1 (car l) n m))
                    (cond ((and cnvrgrindflag (grindmacrocheck gm l))
                           (princ '/./ )
                           (sprint1 l (- n 2.) m)
                           (setq l nil)
                           (return nil))
                          (t (prin1 (car l))))))
             (popl)
             (cond (l (go go)) ((return nil))))) 

(defun setq-form nil 
       (cond ((catch (prog (mm) 
                           (setq mm (maxpan (cdr l) arg))              ;standard form
                           (setq n arg)                                ;committed to at least standard form
                           (defprop setq
                                    (setq-predict l n m)
                                    grindpredict)                      ;prediction in special form computed to
                           (and (< mm                                  ;compare to p.
                                   (panmax l
                                           (prog2 nil
                                                  (1+ n)
                                                  (setq n arg))
                                           m))                         ;setq form
                                (return t))
                           (form 'line)
                      d    (or l (return nil))
                           (indent-to n)
                           (form 'line)
                           (form 'code)
                           (remsemi)
                           (go d)))
              (defprop setq nil grindpredict)                          ;setq-predict causes throw when variable
              (form 'line)                                             ;name is very long.  therefore, it is
              (setq form n))))                                         ;not used all the time but only inside
                                                                       ;setq-form.


(defun setq-predict (l n m)                                            ;returns number of lines to print args
       (prog (mm nn)                                                   ;as name-value pairs.
             (setq n (- n 2. (gflatsize (car l))))                     ;n = space for name<space>value.  2 =
             (setq mm 0.)                                              ;space for ( and <space preceding
        a    (and (null (setq l (cdr l))) (return mm))                 ;variable>.
             (and (semi? (car l)) (go a))
             (setq nn (- n 2. (gflatsize (car l))))                    ;nn = space for value. 2 = space for )
        b    (cond ((null (cdr l))	                               ;and <space preceding value>.
                    (setq l (error 'setq-predict l 'wrng-no-args))
		    (go b)))
	     (setq l (cdr l))
             (and (semi? (car l)) (go b))
             (setq mm (+ mm (panmax (car l) nn 0.)))
             (go a))) 


;;;format control

(defun predict args (setq predict (cond ((= args 0.)) ((arg 1.)))))    ;(predict) <=> (predict t) =>
                                                                       ;super-careful sprint considering all
                                                                       ;formats.  (predict nil) => less careful
                                                                       ;but quicker.


;;;the following format fns are used only in grinding files.  however,
;;;they may appear in a grind (init) file which is loaded by gfn.
;;;hence, they are defined in gfn to avoid undf error.

(defun slashify fexpr (chars) (mapc 'slashify1 chars))                 ;(eg (slashify $).  preserve slashes
                                                                       ;preceding user read macros.

(defun unslashify fexpr (chars) (mapc 'unslashify1 chars)) 

(defun slashify1 (char)                                                ;make char '-like readmacro.
       ((lambda (readtable) 
         (or
          (null (getchar char 2.))                                     ;will be null only if char is single
          (setq char (error 'slashify char 'wrng-type-arg)))
         (setsyntax char
                    'macro
                    (subst char
                           'char
                           '(lambda nil (list 'char (read)))))
         (apply 'readmacro (list char char)))
        grindreadtable)) 


;(declare (noargs nil))                                                        ;args prop for user-level tj6 fns.

(defun unslashify1 (char) 
       ((lambda (readtable) 
         (or
          (null (getchar char 2.))
          (setq char (error 'unslashify char 'wrng-type-arg)))
         (setsyntax char 'macro nil)
         (apply 'unreadmacro (list char)))
        grindreadtable)) 

(defun programspace (x) 
       (setq programspace (newlinel x))
       (setq comspace (- pagewidth gap programspace))) 

(defun pagewidth (w x y z) 
       (setq pagewidth w)
       (setq gap y)
       (setq programspace x)
       (setq comspace z)) 

(defun comspace (x) 
       (setq comspace x)
       (setq programspace (- pagewidth gap comspace))) 

(defun page nil (tyo 12.) (setq grindlinct global-lincnt)) 

(defun fill args (setq fill (cond ((= args 0.)) ((arg 1.)))))          ;(fill) <=> (fill t) => spaces gobbled
                                                                       ;in ; comments.  (fill nil) => spaces
                                                                       ;not gobbled.  triple semi comments are
                                                                       ;never filled but are retyped exactly
                                                                       ;inuser's original form.

(defun merge args (setq merge (cond ((= args 0.)) ((arg 1.)))))        ;(merge) <=> (merge t) => adjoining ;
                                                                       ;and ;; comments are merged. (merge nil)
                                                                       ;=> adjoining comments not merged. 
                                                                       ;;;;... are never merged.

(defun user-paging args                                                ;(user-paging) <=> (user-paging t) 
       (setq user-paging (cond ((= args 0.)) ((arg 1.)))))             ;grind does not insert any formfeeds,
                                                                       ;but preserves paging of user's file.
                                                                       ;(user-paging nil) => grind inserts
                                                                       ;formfeed every 59 lines.  attempts to
                                                                       ;avoid s-expr pretty-printed over page
                                                                       ;boundary.  ignores users paging. paging
                                                                       ;of user's file.

(defun topwidth (x) (setq topwidth x)) 

;(declare (noargs t))                                                  ;args prop for user-level tj6 fns.

;;user defined formats
(defun remsemi nil
 (prog (retval)
        loop (cond ((remsem1) (setq retval t)) ((return retval)))
             (go loop)))


(defun remsem1 nil                                                     ;remsemi switch t for grinding files,
       (and remsemi (cond ((rem/;) (rem/;/;) t) ((rem/;/;)))))         ;nil for grindef.  speeds up grindef. 
                                                                       ;also, prevents possible illegal memory
                                                                       ;reference by rem/; caar on pnames.

(defun popl nil (setq l (cdr l)) (remsemi) l) 

(defun semisemi? (k) 
 (cond ((null remsemi) nil)                             ;check for any ;;'s
        ((eq k /;/;))
          ((atom k) nil)
           ((or (semisemi? (car k)) (semisemi? (cdr k))))))             ;at any depth

(defun semi? (k) (and remsemi (or (eq (car k) /;) (eq (car k) /;/;)))) 

(defun indent (nn)                                                     ;indents additonal nn spaces.
	(cond ((minusp (setq nn (- (grchrct) nn)))
		(error 'indent/ beyond/ linel? nn 'fail-act)
		(terpri))
	      ((indent-to nn)))) 

(defun stat-tab macro (x) (list 'quote (status tabsize)))              ;replaced by compiler by tab (8 its, 10. ;Multics)

(defun indent-to (nn)                                                  ;chrct set to nn
       ((lambda (nct tab) 
                (declare (fixnum nct tab))
                (cond ((or (< nct 0.) (> nn nct))                      ;chrct may become negative from
                       (turpri)                                        ;prin50com.
                       (setq nct linel)))
                (cond ((< nn nct)                                      ;some indentation is necessary
                       (setq tab (+ nct 
				    (- (stat-tab))
                                    (\ (- linel nct) (stat-tab))))     ;position as a result of first tab.
                       (cond ((< tab nn) (grindslew (- nct nn) 32.))    ;tabs do not move 8, but
                             ((tyo 9.)					;to nearest multiple of 8
                              (setq nct tab)
                              (cond ((< nn nct) 
				     (grindslew (// (setq nct (- nct nn)) (stat-tab)) 9.)
                                     (grindslew (\ nct (stat-tab)) 32.))))))))
        (grchrct)
        0.)) 

(defun grindslew (nn x) (do mm nn (1- mm) (zerop mm) (tyo x)))


(defun pprin (l tp) 
       (cond ((and cnvrgrindflag (atom l) (?grindmacro l)) nil)
             ((atom l) (prin1 l) t)                                    ;l is ground as line if tp = 'line, as a
             ((eq tp 'line)
              (cond ((gprin1 l n)(prin1 l))) t )                               ;block if tp = 'block or as a function
             ((eq tp 'block)                                           ;followed by a list
              (or (and (atom (car l))
                       ((lambda (x) (and x (apply x nil)))
                         (get (car l) 'grindmacro)))
                  (progn (princ '/()                                           ;of arguments if l = 'list, or normally
                       (gblock (grchrct))                                              ;if tp = 'code.
                       (princ '/)))))
             ((eq tp 'list)
              (or (and (atom (car l))
                       ((lambda (x) (and x (apply x nil)))
                         (get (car l) 'grindmacro)))
                  (progn (princ '/()
                       (gblock (- (grchrct) 1. (gflatsize (car l))))
                       (princ '/)))))
             ((eq tp 'code) (sprint1 l (grchrct) m) t))) 

(defun turpri nil 
       (and remsemi comnt (prin50com))                                 ;cr with line of outstanding single semi
       (terpri)                                                        ;comment printed, if any.  grindlinct =
       (setq grindlinct (cond ((= grindlinct 0.) global-lincnt)                ;lines remaining on page.
                              ((1- grindlinct))))) 

(ifnio (defun grchrct nil (- linel (charpos (and outfiles (car outfiles))))))


(defun testl args 
       (prog (k nargs) 
             (setq k l nargs (cond ((= 0. args) 0.) ((arg 1.))))
        a    (cond ((null k) (return nil))
                   ((semi? (car k)) (setq k (cdr k)) (go a))
                   ((= 0. nargs)
                    (return (cond ((= 2. args) k) (t (car k)))))
                   ((setq nargs (1- nargs))
                    (setq k (cdr k))
                    (go a))))) 

(defun form (x)                                                        ;pprin the car of l, then pops l.
       (cond ((remsemi) (form x))                                      ;no-op if l is already nil. process
             (l (cond ((pprin (car l) x)                                       ;initial semi-colon comment, if any,
	               (and (cdr l) (tyo 32.))                                ;then try again. pretty-print c(car l)
	               (setq l (cdr l)))
                      ((and cnvrgrindflag (grindmacrocheck (?grindmacro (car l)) l))
                       (princ '/./ )
                       (gprin1 l (- n 2.))
                       (setq l nil form nil))
                      (t (prin1 (car l))
                         (and (cdr l) (tyo 32.))
                         (setq l (cdr l)))))))                                  ;in desired format. if l is not yet nil,
                                                                       ;output a space. return popped l. 



;;local functions

(defun sprinter (l)                                                    ;pretty print over whole width
       (prog nil 
             (set-linel)
             (turpri)
             (turpri)
             (sprint l linel 0.)
             (turpri)
             (return '*))) 

(defun sprint (l n m) (fillarray 'gtab/| '(nil)) (sprint1 l n m)) 

;;;sprint formats
;;;form1 = (s1    form2 = (s1 s2    form3 = (s1 s2 (sprint1 last))
;;;         s2                s3)
;;;         s3)

(defun sprint1 (l n m)                                                 ;expression l to be sprinted in space n
       (prog (form arg fn args p prog? grindfn form3? gm)              ;with m unbalanced "/)" hanging. p is
             (and (remsemi) (null l) (return nil))                     ;number lines to sprint1 as form2
             (setq /;/;? nil)
             (indent-to n)
             (and (atom l) 
		  (cond (cnvrgrindflag)
			((setq gm (?grindmacro l)) (return gm))
                        (t (prin1 l) (return nil))))
             (cond ((and grind-standard-quote 			      ;This is an explicit check for QUOTE.
			 (eq (car l) 'quote)			      ;The alternative is to use the standard grindmacro
			 (cdr l) 				      ;To use your own personal readmacro for quote,
			 (null (cddr l)))			      ;setq grind-standard-quote to nil.
		    (princ '/') 
		    (and (setq gm (sprint1 (cadr l) (grchrct) m))
			 cnvrgrindflag 
			 (cond ((grindmacrocheck gm (cdr l))
				(princ '/./ )
				(sprint1 (cdr l) (- (grchrct) 2) m))
			       (t (prin1 (car l)))))
		    (return nil)))
	     (and (atom (car l))
                  (setq fn (car l))
                  ((lambda (x) (and x (apply x nil)))
                   (get (car l) 'grindmacro))
                  (return nil))
             (cond ((semisemi? l))                      ;if a ;; comnt, force multi-line
                   ((< (+ m -1. (gflatsize l)) (grchrct))
                    (return (gprin1 l n))))
             (princ '/()
             (setq n (grchrct))
             (setq arg (- n (gflatsize (car l)) 1.))
             (and
              (atom (setq args
                          (cond ((setq grindfn (get fn
                                                    'grindfn))
                                 (apply grindfn nil)
                                 (and (numberp form)
                                      (setq n form)
                                      (go b))
                                 (and (null l)
                                      (princ '/))
                                      (return nil))
                                 l)
                                ((cdr l)))))
              (go b))
             (catch                                                    ;catch exited if space insufficient.
              (and
               (setq p (maxpan args arg))                              ;p = # of lines to sprint l in standard
               (cond (predict (not (< (maxpan args n) p)))             ;format. exit if miser more efficient
                     (fn))                                             ;than standard in no-predict mode, use
               (setq n arg)                                            ;miser format on all non-fn-lists.
               (cond                                                   ;committed to standard format.
                (grindfn (or (eq form 'form2)
                          (> (maxpan args (grchrct)) p) (setq n (grchrct))))
                ((prog nil 
                       (or predict (go a))                             ;skip form3 is predict=nil.
                       (catch
                        (setq 
                         form3?                                        ;l cannot be fit in chrct is it more
                         (and (not (eq (car (last l)) /;))             ;efficient to grind l form3 or form2
                              (< (maxpan (last l)
                                         (- (grchrct)
                                            (- (gflatsize l)
                                               (gflatsize (last l)))))
                                 p))))
                  a    (cond ((setq gm (gprin1 (car l) n))
                              (cond ((grindmacrocheck gm l)
                                     (princ '/./ )
                                     (gprin1 l (- n 2.))
                                     (setq l nil)
                                     (go b1))
                                    (t (prin1 (car l))))))
                       (tyo 32.)
                       (and (cdr (setq l (cdr l))) form3? (go a))
                  b1   (setq n (grchrct)))))))
        b    (grindargs l n m))) 

(defun grindargs (l nn mm)                                             ;elements of l are ground one under the
       (prog (gm sprarg1 sprarg2)                                      ;next
        a    (and (done? nn) (return nil))                             ;prints closing paren if done.
             (setq sprarg1 (cond ((and cnvrgrindflag (eq (car l) '/"aux/")) (+ nn 6.))
                                 ((and prog?
                                       (car l)
                                       (or (atom (car l))
                                           (and cnvrgrindflag (eq (caar l) ':))))
                                  (+ nn 5.))                           ;exception of tags which are unindented
                                 (nn)))                                ;5
             (setq sprarg2 (cond ((null (cdr l)) (1+ mm))
                                 ((atom (cdr l))
                                  (+ 4. mm (gflatsize (cdr l))))
                                 (0.)))
             (cond ((setq gm (sprint1 (car l) sprarg1 sprarg2))
                    (cond ((grindmacrocheck gm l)
                           (princ '/./ )
                           (sprint1 l (- sprarg1 2.) sprarg2)
                           (setq l nil)
                           (go a))
                          (t (prin1 (car l))))))
             (setq l (cdr l))
             (go a)))
 

(defun done? (nn) 
       (cond ((atom l)
              (and /;/;? (indent-to nn))                               ;if previous line a ;; comment, then do
              (cond (l (princ '/ /./ ) (prin1 l)))                     ;not print closing paren on same line as
              (princ '/))                                              ;comment.
              t)))                                                     ;prints closing "/)" if done


(defun gblock (n)                                                      ;l printed as text with indent n.
       (prog (gm) 
             (and (remsemi) (or l (return nil)))
        a    (cond ((setq gm (gprin1 (car l) n))
                    (cond ((grindmacrocheck gm l)
                           (princ '/./ )
                           (gprin1 l (- n 2.))
                           (return (setq l nil)))
                          (t (prin1 (car l))))))
             (or (popl) (return nil))
             (cond ((< (gflatsize (car l)) (- (grchrct) 2. m))
                    (tyo 32.)
                    (go a))
                   ((and (not (atom (car l)))                          ;non-atomic elements occuring in block
                         (< (- n m) (gflatsize (car l))))              ;too large for the line are sprinted. 
                    (cond ((setq gm (sprint1 (car l) n m))             ;this occurs in the variable list of a
                           (cond ((grindmacrocheck gm l)               ;thprog.
                                  (princ '/./ )
                                  (sprint1 l (- n 2.) m)
                                  (return (setq l nil)))
                                 (t (prin1 (car l))))))
                    (or (popl) (return nil))))
             (indent-to n)                                             ;new line
             (go a)))
 


(defun gprin1 (l nn)                                                   ;prin1 with grindmacro feature.
       (cond ((and cnvrgrindflag (atom l) (?grindmacro l)))
             ((atom l) (prin1 l) nil)
             ((prog (gm) 
                    (remsemi)
                    (and (atom (car l))
                         ((lambda (x) (and x (apply x nil)))
                          (get (car l) 'grindmacro))
                         (return nil))
                    (princ '/()
               a    (cond ((setq gm (gprin1 (car l) nn))
                           (cond ((grindmacrocheck gm l)
                                  (princ '/./ )
                                  (gprin1 l (- nn 2.))
                                  (setq l nil)
                                  (go a1))
                                 (t (prin1 (car l))))))
                    (popl)
               a1   (and (done? nn) (return nil))
                    (tyo 32.)
                    (go a))))) 

;;prediction functions

(defun maxpan (l n) 
       (prog (g)                                                       ;estimates number of lines to sprint1
             (setq g 0.)                                               ;list of s expression one under the next
        a    (setq g                                                   ;in space n
                   (+ g
                      (panmax (car l)
                              n
                              (cond ((null (setq l (cdr l))) (1+ m))
                                    ((atom l) (+ m 4. (gflatsize l)))
                                    (0.)))))
             (and (atom l) (return g))
             (go a))) 

(defun panmax (l n m) 
       (cond ((< (+ m -1. (gflatsize l)) n) 1.)                        ;estimates number of lines to sprint1 an
             ((or (< n 3.) (atom l)) (throw 40.))                      ;s expression in space n.  less costly
             ((or (not (atom (car l))) (atom (cdr l)))                 ;than sprint
              (maxpan l (sub1 n)))
             ((eval (get (car l) 'grindpredict)))                      ;as it always chooses form2.  if
             ((maxpan (cdr l) (- n 2. (gflatsize (car l)))))))         ;insufficient space, throws.

(defun prog-predict (l n m) 
       ((lambda (nn) (+ (block-predict (cadr l) nn 1.)
                        (maxpan (cddr l) nn)))
        (- n 2. (gflatsize (car l))))) 

(defprop lambda-form (prog-predict l n m) grindpredict) 

(defprop prog-form (prog-predict l n m) grindpredict) 

(defun block-predict (l n indent)                                      ;indent=spaces indented to margin of
       (cond ((> 1. (setq n (- n indent))) (throw 50.))                ;block. throw if insuff remaining space.
             ((1+ (// (- (gflatsize l) indent) n)))))                  ;number of lines approx by dividing size
                                                                       ;of l by block width.

(defprop comment-form
         (block-predict l n (+ (gflatsize (car l)) 2.))
         grindpredict) 

(defprop block-form (block-predict l n 1.) grindpredict) 

(defprop readmacroinverse (panmax (cadr l) (1- n) m) grindpredict) 

(defun gflatsize (data)
    ((lambda (nn bucket)
             (setq bucket (gtab/| nn))
             (cdr (cond ((and bucket (assq data bucket)))
                        (t (car (store (gtab/| nn)
                                       (cons (setq data (cons data (flatsize data)))
                                             bucket)))))))
        (\ (maknum data) 127.) nil)) 

;;conniver macros

(setq cnvrgrindflag nil) 

(defun cnvrgrind nil 
       ((lambda (readtable) 
                (setsyntax ':
                           'macro
                           'grindcolmac)
                (setsyntax '@ 'macro 'grindatmac)
                (setsyntax '/,
                           'macro
                           'grindcommac)
                (setsyntax '! 'macro 'grindexmac)
                (readmacro : :)
                (readmacro /, /,)
                (readmacro @ @ t)
                (readmacro !$ (33. 36.) t)
                (readmacro !/" (33. 34.) t)
                (readmacro !@ (33. 64.) t)
                (readmacro !? (33. 63.) cnvr-optional)
                (readmacro !/, (33. 44.) cnvr-optional)
                (readmacro !< (33. 60.) cnvr-optional)
                (readmacro !> (33. 62.) cnvr-optional)
                (readmacro !/; (33. 59.) cnvr-optional)
                (readmacro !/' (33. 39.) cnvr-optional)
                (setq cnvrgrindflag t sgploses grind-standard-quote grind-standard-quote nil)
                'conniver-macros-learned)
        grindreadtable)) 

(defun lispgrind nil 
       ((lambda (readtable) 
                (setsyntax ': 'macro nil)
                (setsyntax '@ 'macro nil)
                (setsyntax '/, 'macro nil)
                (setsyntax '! 'macro nil)
                (mapc 'unreadmacro
                      '(: /, @ !$ !/" !@ !? !/' !/, !< !> !/;))
                (setq cnvrgrindflag nil grind-standard-quote sgploses)
                'conniver-macros-forgotten)
        grindreadtable)) 


;;default formats

						;"quote" is explicitly checked, and the inverse
                                                ;macro function ignored if this flag is non-nil.
                                                ;To have your own macro for quote take effect, 
                                                ;set grind-standard-quote to nil.
(readmacro quote /')                           ;Still ned to define the standard macro

(grindfn (grindfn grindmacro) (form 'line)
                              (form 'block)) 

(grindfn lambda lambda-form) 

(grindfn (if-added if-needed if-removed) if-form)

(grindfn (defun cdefun) def-form) 

(grindfn prog prog-form) 

(grindfn (comment remob **array *fexpr *expr *lexpr special
          unspecial) comment-form) 

(grindfn (member memq map maplist mapcar mapcon mapcan mapc assq
          assoc sassq sassoc getl) mem-form) 

(grindfn setq setq-form) 

(grindfn csetq setq-form) 

(predict nil) 


;;;the following default formats are relevant only to grinding files.
;;;however, they appear here since the format fns are not defined
;;;in gfile and gfn is not loaded until after gfile.
;;default formats

(pagewidth 120. 70. 1. 49.) 

(topwidth 110.) 

(merge t) 

(fill t) 

(user-paging nil) 

;;;read the user's start_up.grind [Multics] or grind (init) [ITS] file.

(cond ((status feature its)
       (prog (form ^w h l)		;loader for grind (init) file
             (setq h (list nil) l (crunit))
             (apply 'crunit (list 'dsk (status udir)))
             (cond ((cond ((get 'uprobe 'fsubr)
			    (cond ((uprobe grind /(init/)) 
				   (uread grind /(init/))
					   t)
				  (t (go dn1)))) 
			  ((errset (uread grind /(init/)) nil)))
                    (terpri)
                    (princ '/;loading/ grind/ /(init/)/ dsk/ )
                    (princ (cadr (crunit)))
                    (setq ^q t))
                   (t (go done)))
       init (cond ((and ^q (not (eq h (setq form (read h))))) (eval form) (go init)))
       done (apply 'crunit l)
       dn1  (gctwa)
            (return '*)) )
      (t (errset (load (list (status udir)       ;loader for start_up.grind file
                             'start_up
                             'grind))
                 nil)))
