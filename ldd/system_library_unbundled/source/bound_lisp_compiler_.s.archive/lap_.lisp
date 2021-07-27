;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1974 *
;;; *                                                            *
;;; **************************************************************
;;;;;;
;;;;;; Multics Lisp Assembly Program (LAP)
;;;;;;
;;;;;; Written July 1974 by D Reed
;;;;;; Modified January 1975 by D Moon for bugs + features galore


(declare
     (special
	pc				; actual pc of code generated.
	codelist				; list of internal code representations, reverse order of generation.
	constant-list			; list of all constants referenced by program, maintained by get-const.
	constant-size			; space occupied by all constants referenced by program.
	literal-list			; list of all literal constants referenced by program, maintained by get-const.
	literal-size			; space occupied by all literal constants referred to in code.
	max-literal-bound			; maximum boundary required by literals
	literal-start			; offset of literals from where we originally expected them to be...kept by
					; pass2 and initialize slot-types, used by pass2.
	in-literal			; flag for assembling a literal
          entry-list
	functions-called			; list of names for functions called within compiled code.
	fcn-size				; counter used in allocating space for function links.
	pl1-link-list			; list of "foo$bar" links
	pl1-link-size			; next available address in linkage section
	array-type			; type of array just referenced.
	array-links			; list of array links.
	array-size			; counter used in allocating array-link space.
	functions-defined			; list of name-entrypoint pairs for functions compiled.
	relocation			; aux result from laprel1
	text-relocation			; count of text relocations
	link-relocation			; .. link ..
	static-relocation			; .. static ..
	barfp				; used to detect compiler errors in debug mode.
	dataerrp
	nowarn
	first-eof
	messioc
	current-function
	seg-name				; free variable passed from pass 1, contains name of segment.
	time-option
	total-time 			; on if times are to be printed on console...
	base				; good old output base...
	*nopoint				; and format controller...we must force base 10 output sometimes.
	lapreadtable			; readtable for lap code.
	being-compiled
	errflag
	source-map
     )
     (array* (notype (fcn-table ?) (const-table ?)))
     (fixnum pc relocation text-relocation link-relocation static-relocation constant-size
	   literal-size fcn-size array-size base)

     (do i (read) (read) (equal i ''END) (eval i))	; read up compile time operations.
    )



; compile time operations:
(sstatus macro /! '(lambda () (list 'quote
			      ((lambda (x)
				(or (get x '/!) (error "undefined compile time constant" x)))
			       (read)) )))

(setq vertical-status (status macro /|))
(sstatus macro /| nil)

(defun setm fexpr (l) (do x l (cddr x) (null x) (putprop (car x) (cadr x) '/!)))

(setm	bit29	100
	*	20
	lpI	-377777777700
	abIx7	100000000117
	abInil	100012000100
	abIt	100014000100
	ic	4
	Text18	20		;relocation codes
	-Text18	21
	Link18	22
	-Link18	23
	Link15	24
	Static18	30
	Static15	31
	fixnum-type	40047
	flonum-type	20047
	const-table-size 111.		; size of constant hash table.
	fcn-table-size	111.		; size of function hash table, used to detect identical calls.
     )

'END		; end of compile time operations.


(setq
      time-option nil
	total-time	nil
   )

(array const-table t !const-table-size)
(array fcn-table t !fcn-table-size)


(declare (defpl1 cg-util "lisp_cg_utility_"
	(char(*)) (lisp) (char(*)) (lisp) (lisp) (lisp) (lisp) (lisp)
	(lisp) (lisp) (lisp) (lisp) (lisp) (lisp) (lisp) (lisp)))

(declare (defpl1 make_lisp_listing "" (char(*)) ))

(declare (defpl1 get_alm_op_ "" (char(*)) (return fixed bin(35.)) ))


(%include compiler-macros)		;get macros common to pass 1 and pass 2


(defun logor macro (x) (displace x (cons 'boole (cons 7 (cdr x)))))

(defun logand macro (x) (displace x (cons 'boole (cons 1 (cdr x)))))

(defun left macro (x) (displace x (list 'lsh (cadr x) 18.)))


;;; functions to assign addresses to literals and constants referenced by the code.

(defun get-literal-addr (const)
    (or (car const)			;literal has address or nil in car
        (barf const "literal has not had address assigned" barf)))
				;address supposed to be assigned by make-literal

(defun get-constant-addr (const)	; takes arg in standard "uniquized" representation for constant, returns addr.
	(cond ((cddr const))	; cddr is address if already assigned.
	      (t	(rplacd (cdr const) constant-size)	; assign new address
		(setq constant-size (+ 2 constant-size)	; and up the length of constants.
		      constant-list (cons const constant-list))
		(cddr const))))

(defun get-fcn-addr (const)		; assign address for function link, if not already assigned.
    (cond ((cddddr (cdr const)))	; if already assigned, address is cdddddr of function representation.
	(t (rplacd (cddddr const) fcn-size) ; put new address in representation for future use.
	   (setq fcn-size (+ fcn-size 2)	; 2 words allocated for link.
	         functions-called (cons const functions-called)) ; note that we have to make the link later.
	   (- fcn-size 2))))	; return the address of the link.


(defun get-array-link-addr (x)	;assign address for array link
    (cond	((cddddr (cdr x)))		;already assigned.
	(t (rplacd (cddddr x) array-size)	;insert address
	   (setq array-size (+ array-size 4))	;allow for 4-word block.
	   (setq array-links (cons x array-links))
	   (- array-size 4))))	;return the address of the array-link.

(defun get-pl1-link (name)
    ((lambda (address)
	(setq pl1-link-size (+ address 2))
	(push name pl1-link-list)
	(list address '*link))		;return relocatable address
     pl1-link-size))

(defun get-function (x snap? type nargs)	; function to maintain unique function representation.
    ((lambda (hash bucket)			; some temp variables.
	(setq bucket (fcn-table hash))
	(do ((scan bucket (cdr scan)))
	    ((null scan)
		(store (fcn-table hash)
		       (cons (setq x (list 'function x snap? type nargs)) ; make unique representation if not found
			   bucket))
		x)
	  (and    (eq x (cadar scan))		; if all 4 components are eq, then use this existing representation.
		(eq snap? (caddar scan))
		(eq type (cadddr (car scan)))
		(eq nargs (cadddr (cdar scan)))
		(return (car scan)))))

     (abs (\ (cond ((eq (car x) 'temp) (cadr x))	; if in a temp, hash by temp offset.
	         (t (sxhash (cadr x))))	; otherwise, must be (quote < > ), hash by object.
	   !fcn-table-size))
     nil))

(defun make-const (x)	; function to uniquize the representation of a constant.

(cond
 ((eq x nil) '(quote nil))
 ((eq x t) '(quote t))
 (t ((lambda (hash bucket)			; some temporary variables.
	(setq bucket (const-table hash))	; get hash table bucket.
	(do ((scan bucket (cdr scan)))	; look down bucket for already created representation.
	    ((null scan)			; when no more...
	     (store (const-table hash) (cons (setq x (list 'quote x))
				        bucket))  ; put newly created representation in bucket.
	     x)				; return new representation.

	  (cond ((equal (cadar scan) x) (return (car scan))))))
     (abs (\ (sxhash x) !const-table-size))
     nil))))

(defun get-const (const)	; given (quote <constant>), get unique representation.
    (cond ((smallnump (setq const (cadr const)))
	 (make-literal (list nil 2 !fixnum-type const)))
	((floatp const)
	 (make-literal (list nil 2 !flonum-type const)))
	(t (make-const const)) ))

(defun make-literal (x)	;literals are:  (address boundary . data-list)
			;note - cadr is a smallnum which fakes out lapinst
    (setq max-literal-bound (max max-literal-bound (cadr x)))
  (do ((l literal-list (cdr l))
       (loc 0 (1+ loc)))
      ((= loc literal-size)	;see if can overlap with existing literals
	(do () ((zerop (\ loc (cadr x))))
	   (rplacd (last literal-list) (list 0))
	   (setq loc (1+ loc)))
       (setq literal-size (+ loc (length (cddr x))))
       (setq literal-list (nconc literal-list (append (cddr x) nil)))
       (rplaca x loc)
       x)
     (and (zerop (\ loc (cadr x)))		;if on right boundary,
	(list-equal (cddr x) l)		;and same data
	(return (rplaca x loc))) ))		;then put it here
					;Note this doesn't catch the case where
					;and initial segment of it ends literal-list

(defun list-equal (x y)	;compare two lists of numbers.  eq because could be fix or flo.
    (cond	((null x)		;first list ends, count as a match
	 t)
	((null y)		;second list ends before first, no match
	 nil)
	((eq (car x) (car y))
	 (list-equal (cdr x) (cdr y)) )))	;sure wish this lisp had a jcall...

(defun make-call-link (fn-name snap? type nargs)

     (logor 
	  (cond ((eq (car fn-name) 'temp)
	         (lsh (cadr fn-name) 12.))
	        ((logor 2000 (lsh (+ 1 (get-constant-addr fn-name)) 12.)) )) ; constant-list munged by now.
	  (cond (snap? 4000) (t 0))
	  (cond ((eq type 'fsubr) 1001)
		((eq type 'lsubr) 777)
		(t nargs))))


(defun make-array-link (array type ndims)
     (get-function (make-const array) type 'array ndims))

(defun add-right-half (x y)	; add x to right halfword of y, returning left half of y logor result of add.
     (logor (logand 777777_18. y)
	  (logand 777777 (+ x y))))

;(defun clear-out-useless-fns ()	; gets rid of functional temp references...
;     (do i 0 (1+ i) (= i !fcn-table-size)
;          (mapc '(lambda (x) (and (eq (caadr x) 'temp)	; is temp.
;			    (rplacd (cddddr x) nil)	; forget we had one.
;			))
;	     (fcn-table i))))

(defun make-array-link-control-word (array type ndims)
    (logor (lsh (cond ((eq type 'fixnum) 2)
		  ((eq type 'flonum) 3)
		  (t 0))
	      27.)			;type code
	 (lsh ndims 18.)			;number of dimensions
	 (1+ (get-constant-addr array))))

(defun finish-code ()
     (prog (function-rel array-link-rel type-list def-length intime)
	(setq intime (runtime))

        (setq function-rel 0 def-length 0
              type-list (subst nil nil '((fixnum) (flonum) (string) (bignum) (symbol)(list))))
         (map '(lambda (l)
                   (setq function-rel (1+ function-rel))
                   (rplaca l (analyze (cadar l) type-list)))
              constant-list)
         (mapc '(lambda (x) (rplaca x (cdr (assq (car x) '((nil . 0)
                                                           (expr . 1_18.)
                                                           (lexpr . 2_18.)
                                                           (fexpr . 3_18.)))))
                            (rplacd x (analyze (cdr x) type-list))
		        (setq def-length (1+ def-length)))
               functions-defined)
         (fix-type-list type-list)
         (setq array-link-rel (+ function-rel (length entry-list) (length functions-called)))
         (map '(lambda (l) (rplaca l (get-object-offset (car l))))
              constant-list)
         (map '(lambda (l) (rplaca l (logor (caar l) (get-object-offset (cdar l)))))
              functions-defined)

        (map '(lambda (l)
	      (and (not (atom (car l)))
		 (cond ((eq (caar l) 'function)
		        (rplaca l (+ (cdar l) (lsh function-rel 19.))))
		       ((eq (caar l) 'array)
		        (rplaca l (+ (cdar l) (lsh array-link-rel 19.)))))))
               codelist)
        (cg-util seg-name (cdr (nreverse codelist)) "Multics LISP Assembly Program, Version 1.1, January 1975"
	       (cons (length source-map) (nreverse source-map))
                 (car type-list)
                 (cadr type-list)
                 (caddr type-list)
                 (cadddr type-list)
                 (car (cddddr type-list))
                 (cadr (cddddr type-list))
                 (cons (length entry-list) (nreverse entry-list))
                 (cons function-rel (nreverse constant-list))
                 (cons (length functions-called) (nreverse functions-called))
                 (cons def-length (nreverse functions-defined))
	       (cons (length array-links) (nreverse array-links))
	       (cons (length pl1-link-list) (nreverse pl1-link-list)) )
        (and total-time (iog vt (terpri) (princ "Object creation time = ")
			   (prin1 (//$ (float (- (runtime) intime)) 1000000.0))
			   (terpri)))))


(defun init-code-generator ()
     (setq constant-size 0 fcn-size 0 array-size 0 functions-defined nil array-links nil
           pl1-link-list nil pl1-link-size 10 functions-called nil entry-list nil constant-list nil)
     (fillarray 'fcn-table '(nil))
     (fillarray 'const-table '(nil))
     (setq pc 0 codelist (ncons nil)))

;;; function to analyze constants referenced by lisp compiled code

(defun analyze (x type-lists)
     ;; x is the object, type-lists is a list of the form
     ;; ((fixnum ...)
     ;;  (flonum ...)
     ;;  (string ...)
     ;;  (bignum ...)
     ;;  (symbol ...)
     ;;  (list ..))

  ((lambda (type)
     ((lambda (l)
	(cons type		; returns (<type> .<index-in-type>)
                 (cond ((eq type 'nil) 0)
                       ((eq type 'list)
                        (do ((scan (cdr l) (cdr scan))
                             (last l scan)
                             (i 1 (1+ i)))
		        ((null scan) (list-analyze x i last type-lists))
                          (cond ((eq x (caar scan)) (return i)))))
		   (t
		    (do ((scan (cdr l) (cdr scan))
		         (last l scan)
                             (i 1 (1+ i)))
                            ((null scan) (rplacd last (ncons x)) i)
		      (cond ((equal x (car scan)) (return i))))))))
        (assq type type-lists)))
    (and x (typep x))))

;;; function to insert list-type objects into type-lists.
;;; relies on the fact that sublists are not eq to existing lists.
;;; thus inserts all of the skeleton into the type-list and analyze's only
;;; the fringes.

(declare (special list-offset list-last))

(defun list-analyze (x list-offset list-last type-lists)
    (setq x (ncons (cons x (cons (lanalyze (car x) type-lists) (lanalyze (cdr x) type-lists)))))	; changes list-last, list-offset.
    (rplacd list-last x)
    list-offset)

(defun lanalyze (x type-lists)		; basic analyzer
    (cond ((atom x) (analyze x type-lists))	; if atomic, use ordinary analyzer.
	(t  (setq x (ncons (cons nil (cons (lanalyze (car x) type-lists) (lanalyze (cdr x) type-lists)))))
				;; note that we forget the value of x here, unlike in list-analyze, because
				;; we know that its value will never be eq to any other list we will see.
	    (rplacd list-last x)
	    (prog2 (setq list-last (cdr list-last))	; update the end of the list pointer
		 (cons 'list list-offset)
		 (setq list-offset (+ list-offset 1))))))	; update the count of items.
(declare (special type-offsets))

(defun fix-type-list (type-list)
    ;; takes type-list, and rplaca's lengths into type buckets, and
    ;; fixes up the cons list to be a list of 36 bit numbers.
    ;; generates the special variable type-offsets for use by
    ;; get-object-offset

  ((lambda (base-offset)
        (setq type-offsets (ncons (cons nil 0)))
        (mapc '(lambda (tl)
                    (setq type-offsets (cons (cons (car tl) base-offset) type-offsets))
                    (cond ((eq (car tl) 'list)
                                (map '(lambda (x)
                                          (rplaca x
                                                  (logor (lsh (get-object-offset (cadar x)) 18.)
                                                         (get-object-offset (cddar x)))))
                                    (cdr tl))))
                    (setq base-offset (+ (car (rplaca tl (length (cdr tl))))
                                         base-offset)))
              type-list))
   0))


(defun get-object-offset (x)     ;; returns absolute offset in constant table of object
    (+ (cdr x) (cdr (assq (car x) type-offsets))))


(defun lapup (fn-name type nargs)	; main lap interface....

     ((lambda (intime literal-size max-literal-bound in-literal
		literal-start literal-list readtable)

;	(clear-out-useless-fns)		; not needed because no temp-size reloc
          (setq entry-list (cons (logor (left nargs) pc) entry-list))
          (setq functions-defined (cons (cons (cdr (assq type '((subr . expr) (lsubr . lexpr) (fsubr . fexpr)))) fn-name)
	          functions-defined))
	(setq codelist (cons nil codelist))	; mark our entry point.

	(do word (read) (read) (null word)
	    (lapword word))			; gobble down words

	(cond (literal-list		; put literals
		(do () ((zerop (\ pc max-literal-bound)))
		   (push 0 codelist)
		   (setq pc (1+ pc)))
		(setq literal-start pc)
		(setq codelist (nreconc literal-list codelist))
		(setq pc (+ pc literal-size)) ))

          (do scan functions-called (cdr scan) (or (null scan) (fixp (car scan)))
             (rplaca scan (make-call-link (car (setq type (cdar scan)))
                                          (cadr type)
                                          (caddr type)
                                          (cadddr type))))
	(do scan array-links (cdr scan) (or (null scan) (fixp (car scan)))
	   (rplaca scan (make-array-link-control-word (car (setq type (cdar scan)))
					      (cadr type)
					      (cadddr type))))
	(do ((code codelist (cdr code)) (word))
	    ((null (setq word (car code)))		; if found the beginning of this function...
			; delete out the nil.
			(rplaca code (cadr code))
			(rplacd code (cddr code)))		; splice in code to init types
	   (cond	((numberp word))
		((eq (car word) 'literal)
		 ((lambda (word)
		    (or (= (logand word 17) !ic)	;if not ,ic reference,
		        (setq word (cons !Text18 word))); is relocatable
		    (rplaca code word))
		  (+ (cdr word) (left literal-start))))
		((eq (car word) 'relocate) (rplaca code (lapreloc (cadr word) (caddr word) (cdddr word) 0)))
		((eq (car word) 'function) )
		((eq (car word) 'array) )
		((eq (car word) 'bindliteral) (rplaca code (add-right-half literal-start (cdr word))))))
	(flushsyms)
	(and time-option (iog vt (princ "LAP Assembly time for ") (prin1 fn-name) (princ ":") (princ (quotient (- (runtime) intime) 1.0e6))(terpri)))
	)

      (runtime)
      0
      0
      nil
      0
      nil
      lapreadtable))

(defun lapword (word)			; assemble one word
  ((lambda (tem)
    (cond ((numberp word) (outwrd (lapeval word)))
	((atom word)
	 (and in-literal (warn word "tag in literal"))
	 (lapdefsym word (list pc '*text)))
	((eq tem 'defsym) (eval word))
	((eq tem 'equ) (equ| (cdr word)))
	((eq tem 'entry) (entry| (cdr word)))
	((eq tem 'comment))
	((eq tem 'eval) (mapc (function eval) (cdr word)))
	((eq tem 'get-linkage)		; getlp pseudo op except gets lb
	 (outwrd 213000)			; epaq 0
	 (outwrd -77751012617))		; lprplb sb|lot_ptr,*au
	((eq tem 'block) (block| (cadr word)))
	((eq tem 'ascii) (ascii| (cdr word)))
	((eq tem 'bind) (bind| (cdr word)))
	((eq tem 'sprip)			; should be spri p, addr but for alm deficiency...
	 (lapcode (cons (implode (append '(s p r i) (list (lapregch (cadr word)))))
		      (cddr word))))
	((get tem 'macro)			;expand a macro
	 (mapc 'lapcode (macro-expand word (get tem 'macro))))
	((setq tem (get tem 'EIS))
	 (lapeis word tem))
	(t (lapcode word))))
     (or (atom word) (car word)) ))		;bind tem to operation name

(defun macro-expand (x f)	;returns expanded macro - x is form, f is functional (macro property)
    (cond	((errset (setq x (funcall f x)))
	 x)		;win, return expanded result
	(t (barf x "lisp error during macro expansion" data)
	 ''nil) ))	;again, lose, make result nil

; lisp_cg_utility_ takes relocation bits as follows:
;   if codelist contains ( number . number ) then the cdr is the word and
;   the car has the du relocation in its dl and the dl relocation in its du (zero=abs)

(defun lapreloc (dl du ptr relocation)

    (setq dl (laprel1 dl)
	relocation (left relocation)
	du (laprel1 du))

    (setq ptr
          (cond ((null ptr) (logor (logand dl 777777) (left du)))
	  (t 
	   (setq relocation (cond ((= relocation !Link18) !Link15)
			      ((= relocation !Static18) !Static15)
			      ((= relocation 0) 0)
			      (t (barf nil "improper relocation" data))))
	   (logor (lsh (laprel1 ptr) 33.)
		(logand dl 777777)
		(left (logand du 77777))))))

    (cond ((zerop relocation)
	 ptr)			;non relocatable word
	((cons relocation ptr))))	;relocatable word

(defun laprel1 (reloc) 
       ((lambda (text-relocation link-relocation static-relocation) 
	      (prog2 nil
		   (cond ((numberp reloc) reloc)
		         ((+ (car reloc)
			   (- (lapsymsum (cadr reloc) 1)
			      (lapsymsum (cddr reloc) -1)))))
		   (setq relocation
		         (logor relocation
			      (cond ((not (zerop text-relocation))
				   (or (zerop (logor link-relocation static-relocation))
				       (barf nil "mixed relocation" data))
				   (cond ((= text-relocation 1) !Text18)
				         ((= text-relocation -1) !-Text18)
				         ((barf text-relocation
					      "multiple relocation"
					      data))))
				  ((not (zerop link-relocation))
				   (or (zerop static-relocation)
				       (barf nil "mixed relocation" data))
				   (cond ((= link-relocation 1) !Link18)
				         ((= link-relocation -1) !-Link18)
				         ((barf link-relocation
					      "multiple relocation"
					      data))))
				  ((not (zerop static-relocation))
				   (or (= static-relocation 1)
				       (barf static-relocation "multiple relocation" data))
				   !Static18)
				  (t 0))))))
        0
        0
        0))

(defun pense-au-relocation (thing direction)
    (cond ((eq thing '*text)
	 (setq text-relocation (+ text-relocation direction)))
	((eq thing '*link)
	 (setq link-relocation (+ link-relocation direction)))
	((eq thing '*static)
	 (setq static-relocation (+ static-relocation direction)))
	((barf thing "bad relocation" barf)) ))

(defun lapsymsum (thing direction)
    (cond ((null thing) 0)
	((fixp thing) thing)
	((memq thing '(*text *link *static))	;relocation flag could be here too
	 (pense-au-relocation thing direction)
	 0)
	((fixp (car thing))
	 (+ (car thing) (lapsymsum (cdr thing) direction)))
	((memq (car thing) '(*text *link *static))	;relocation flag
	 (pense-au-relocation (car thing) direction)
	 (lapsymsum (cdr thing) direction))
	((+ (lapsymsum (or (get (car thing) 'sym)
		         (prog2 (barf (car thing) " undefined symbol." data) 0))
		     direction)
	    (lapsymsum (cdr thing) direction)))))

(declare (special symlist))
(setq symlist nil)

(defun lapevaln (x)		;ensure numeric result
  ((lambda (xx)
    (or (smallnump xx) (barf x "cannot be reduced to a number" data))
    xx)
  (lapeval x)))

(defun lapeval (x)
    (cond ((null x) 0)
	((eq x '*) pc)
	((floatp x) x)			; flonums as words or literals...
	((smallnump x) x)
	((bigp x) (logor (lsh 1 35.) (haipart x -35.)))	;e.g. 777777777777 -> fixnum -1
	((atom x) (lapsymval x))
	((eq (car x) '+) (lap+l (mapcar 'lapeval (cdr x))))
	((eq (car x) '-) (lap-l (mapcar 'lapeval (cdr x))))
	(t (lapeval (cons '+ x)))))

(defun lapsymval (name)
 ((lambda (val)
    (or val (list 0 (ncons name))))
  (get name 'sym)))

(defun lap+l (list)
    (cond ((null list) 0)
	(t (lap+ (car list) (lap+l (cdr list))))))

(defun lap-l (list)
    (cond ((null list) 0)
	((null (cdr list))			;(- a) => -a not a
	 (lap- 0 (car list)))
	(t (lap- (car list) (lap+l (cdr list))))))

(defun lap+ (x y)
    (cond ((and (fixp x) (fixp y)) (+ x y))
	((fixp x) (cons (+ x (car y)) (cdr y)))
	((fixp y) (lap+ y x))
	(t (cons (+ (car x) (car y))
		(cons (append (cadr x) (cadr y))
		      (append (cddr x) (cddr y)))))))

(defun lap- (x y)
    (cond ((and (fixp x) (fixp y)) (- x y))
	((fixp x) (cons (- x (car y))
		      (cons (cddr y) (cadr y))))
	((fixp y) (cons (- (car x) y) (cdr x)))
	(t (cons (- (car x) (car y))
	         (cons (append (cadr x) (cddr y))
		     (append (cddr x) (cadr y)))))))

(defun lapdefsym (name val)
     (putprop name val 'sym)
     (setq symlist (cons name symlist)))

(defun flushsyms () (mapc '(lambda (x) (remprop x 'sym)) symlist))


(defun outlap (dl du)
    (outwrd (cond ((and (fixp dl) (fixp du))
		(logor (logand dl 777777) (left du)))
	        (t (list 'relocate dl du)))))

(defun outwrd (wrd)
    (or in-literal (setq pc (1+ pc)))
    (push wrd codelist))

(defun lapcode (word)
  ((lambda (opcode)			;look up in ALM symbol table
    (cond ((= opcode -1))		;not known to alm, proceed using lap evaluation
	((= opcode -2)		;e.g. epp bp, foo
	 (setq word (cons (intern (make_atom (catenate (car word) (lapregch (cadr word)))))
		      (cddr word))))
	(t (rplaca word opcode)))	;ordinary opcode, stick it in

    (cond ((null (cdr word)) (lapinst (car word) 0 0))
	((null (cddr word)) (lapinst (car word) (cadr word) 0))
	((eq (caddr word) '/|)
		(lapcode (cons (car word) (cdddr word)))
		(setq word (lapeval (cadr word)))
		(rplaca codelist (addbaseref (car codelist) word)))
	(t (lapinst (car word) (cadr word) (caddr word)))))
    (cond ((eq (typep (car word)) 'symbol)
	 (get_alm_op_ (car word)))
	(t -1)) ))

(defun lapregch (x)			;make lap expression into character of register number
    (substr "01234567" (1+ (logand 7 (lapevaln x))) 1))

(defun addbaseref (word ptr)

    (cond ((and (fixp ptr) (fixp word))
	 (cond ((minusp ptr)		;us pointer = ab|,x7
	        (logor !abIx7 (logand word 77777777777)))
	       ((logor (lsh ptr 33.) !bit29 (logand word 77777777777)))))
	((fixp word) (cons 'relocate (cons (logor !bit29 (logand 777777  word ))
				(cons (logand 777777 (lsh word -18.))
				      ptr))))
	(t (cons 'relocate (cons (lap+ !bit29 (cadr word)) (cons (caddr word) ptr))))))

; this is a pretty poor way to get around this bit 29 loss

(defun addbaserefeiskludge (word ptr)

    (cond ((and (fixp ptr) (fixp word))
	 (cond ((minusp ptr)		;us pointer = ab|,x7
	        (barf nil "Sorry, can't have us| in an EIS descriptor." data))
	       ((logor (lsh ptr 33.) (logand word 77777777777)))))
	((fixp word) (cons 'relocate (cons (logand 777777  word )
				(cons (logand 777777 (lsh word -18.))
				      ptr))))
	(t (cons 'relocate (cons (cadr word) (cons (caddr word) ptr))))))

; Assemble a literal

(defun lap-literal (x)
  ((lambda (codelist pc in-literal alignment)	;use regular assembler, different codelist
    (cond ((eq 'symbol (typep (cadr x)))	;one word literal
	 (lapword (cdr x)))
	((mapc 'lapword (cdr x))))		;multi-word literal
    (make-literal (cons nil (cons alignment (nreverse codelist)))))
  nil pc t (cond ((eq (car x) '%) 1) (t 2)) ))

(defun lapinst (opc addr tag)

    (and (eq tag '*) (setq tag 20))	; * in tag context differs.

    (cond ((eq tag '$)		; -*,ic reference
	 (setq tag 'ic)
	 (setq addr (list '- addr '*))))

    (setq tag (lapeval tag)
	opc (lap+ (lapeval opc) tag))

    (or (fixp tag) (warn tag " strange tag."))

    (cond ((fixp addr) (outlap opc addr))
	((floatp addr) (outlap opc (lsh addr -18.)))	;1.0,du
	((atom addr) (outlap opc (lapsymval addr)))
	((memq (car addr) '(% %%))	;literal
	 (or (numberp opc) (barf addr " ill literal" data))
	 (setq addr (get-literal-addr (lap-literal addr)))	;assemble literal
	 (outwrd
	   (cons 'literal
	    (cond ((and (not in-literal) (or (= tag 0) (= tag !*)))	;can use relative addressing
		 (logor opc !ic (left (- addr pc))))
		((logor opc (left addr)))) )))	;have to use absolute addressing
	((eq (car addr) 'quote)
	    (or (and (numberp opc) (zerop tag))
	        (prog2 (barf addr " illegal quote." data) (setq opc 0)))
	    (setq addr (get-const addr))	; add addr to tables.
	    (cond ((eq (cadr addr) nil) (outwrd (logor opc !abInil)))
		((eq (cadr addr) t)   (outwrd (logor opc !abIt)))
		((or (smallnump (cadr addr)) (floatp (cadr addr)))
			(outwrd (cons 'literal
				    (logor opc !ic (left (- (get-literal-addr addr) pc))))))
		((outwrd (logor opc !lpI (left (1+ (get-constant-addr addr))))))))

	((eq (car addr) 'special)
	 (or (and (fixp opc) (eq (typep (cadr addr)) 'symbol) (= tag 0))
	     (prog2 (barf addr " illegal special reference." data) (setq opc 0)))
	 (outwrd (logor opc !* !lpI (left (1+ (get-constant-addr (make-const (cadr addr))))))))
	((eq (car addr) 'array)
	 (or (and (fixp opc) (= tag 0) (eq (typep (cadr addr)) 'symbol)
		(memq (caddr addr) '(t nil fixnum flonum)) (fixp (cadddr addr)))
	     (barf addr " illegal array reference" data))
	 (outwrd (cons 'array
		     (logor opc !lpI !* (left (1+ (get-array-link-addr
					     (make-array-link (cadr addr) (caddr addr) (cadddr addr)))))))))
	((and (eq (car addr) 'function) (eq (caddr addr) '/|))	;temp function
	 (or (and (fixp opc) (= tag 0)) (barf addr "illegal temp function reference" data))
	 (or (signp e (lapeval (cadr addr)))	;base better be ap
	     (barf addr "temp function reference not to marked stack" data))
	 (or (signp l (setq tag (lapeval (cadddr addr))))		;offset from ap
	     (barf addr "illegal temp function reference" data))
	 (outwrd (cons 'function (logor opc !lpI !* (left (1+ (get-fcn-addr
			(get-function (list 'temp tag) nil (car (cddddr addr)) (cadr (cddddr addr)) ))))))))
	((eq (car addr) 'function)
	 (or (and (fixp opc) (= tag 0) (eq (typep (cadr addr)) 'symbol))
	     (prog2 (barf addr " illegal function reference." data) (setq opc 0)))
	(outwrd (cons 'function (logor opc !lpI !*  (left (1+ (get-fcn-addr (get-function (make-const (cadr addr))
								  t
								  (caddr addr)
								  (cadddr addr)))))))))
	((eq (car addr) 'external)
	 (or (and (fixp opc) (= tag 0) (eq (typep (cadr addr)) 'string))
	     (barf addr " illegal external reference" data))
	 (outlap (logor opc !*) (get-pl1-link (cadr addr)))
	 (rplaca codelist (addbaseref (car codelist) 5)))	;lb -> linkage
	((get (car addr) 'macro)
	 (lapinst opc (macro-expand addr (get (car addr) 'macro)) 0))
	(t (outlap opc (lapeval addr)))))

(defun lapeis (word prop)
    (cond ((eq (car prop) 'inst)
	 (lapeisinst word (cadr prop) (caddr prop)))
	((eq (car prop) 'desc)
	 (lapeisdesc word (cadr prop) (caddr prop) (cadddr prop)))
	((barf word "bad EIS operation" barf))))

(defun lapeisdesc (word codebits bytesize type)
    (prog (ptr addr offset length scale)
      (setq addr (lapeval (cadr word)) word (cddr word))
      (and (eq (car word) '/|)
	 (setq ptr addr
	       addr (lapeval (cadr word))
	       word (cddr word)))
      (cond ((not (atom (car word)))	;(offset)
	   (setq offset (lapeval (caar word)))
	   (setq word (cdr word)))
	  ((setq offset 0)))
      (setq length (lapevaln (car word)) word (cdr word))
      (setq scale 0)
      (and word (setq scale (lapevaln (car word))))
      (setq length (logand length 7777) scale (logand scale 77))

      (setq word
       (cond ((eq type 'bit)
	    (logor length		;make RH of desc
		 (lsh (// offset 9) 16.)
		 (lsh (\ offset 9) 12.)))
	  ((eq type 'char)
	   (and (= bytesize 9) (setq offset (* 2 offset)))
	   (logor length (lsh offset 15.) codebits))
	  ((eq type 'num)
	   (and (= bytesize 9) (setq offset (* 2 offset)))
	   (logor length (lsh scale 6) (lsh offset 15.) codebits))
	  (t (barf type "bad EIS desc type" barf) 0)))
      (outlap word addr)
      (and ptr (rplaca codelist (addbaserefeiskludge (car codelist) ptr)))
    ))

(defun lapeisinst (word opc type)
  (prog (mfctr item tem)
    (setq mfctr 0)		;next modifier field
a   (cond ((null (setq word (cdr word)))
	 (outwrd opc)
	 (return nil) ))
    (setq item (car word))
    (setq opc (logor opc		;or in cruft from next item
    (cond ((or (null item) (not (atom item)))	;mf
	 (setq item (lapevaln
		   (sublis '((pr . 100) (rl . 40) (id . 20)) item)))
	 (lsh (logand item 177)
	       (cond ((= (setq mfctr (1+ mfctr)) 1) 0)
		   ((= mfctr 2) 18.)
		   (t 27.) )) )
	((eq item 'ascii) (lsh 1 35.))	;flags
	((eq item 'enablefault) (lsh 1 26.))
	((eq item 'round) (lsh 1 25.))
	((setq tem (assq item '((mask 777 27.)
			    (bool 17 27.)
			    (fill 777 27.))))
	 (and (eq item 'fill) (eq type 'bit) (setq tem '(fill 1 35.)))	;kludge
	 (setq word (cdr word) item (lapevaln (car word)))
	 (lsh (logand item (cadr tem)) (caddr tem)))
	((barf item "bad field in EIS instruction" data)) )))
    (go a)))

(defun defsym fexpr (l)
    (do l l (cddr l) (null l)
	(putprop (car l) (eval (cadr l)) 'sym)
     ))
(defun equ| (l)
    (do l l (cddr l) (null l)
	(putprop (car l) (lapevaln (cadr l)) 'sym)
     ))


(defun lap fexpr (l)
    ((lambda (f type nargs being-compiled source-map)
	(init-code-generator)
	(lapup f type nargs)
	((lambda (seg-name) (finish-code) (load seg-name))
	 (catenate "[pd]>" f ".fasl")))
     (car l) (cadr l) (caddr l) (car l) nil))

(defun entry| expr (l)
   ((lambda (fn-name type nargs)
	(setq entry-list (cons (logor (left nargs) pc) entry-list))
	(setq functions-defined (cons (cons (cdr (assq type '((subr.expr) (lsubr.lexpr) (fsubr.fexpr)))) fn-name)
		functions-defined))
      )
    (car l) (cadr l) (caddr l)))

(defun block| (n)
      (do n n (1- n) (signp le n)
	(outwrd 0) ))

(defun ascii| (l)
    (do l (mapcan (function exploden) l)	;get list of chars
          (cddddr l) (null l)			;and take 4 at a time (cdr nil=nil)
       (outwrd (do ((i 4 (1- i))
		(l l (cdr l))
		(w 0))
	         ((zerop i) w)
	       (setq w (logor w (lsh (or (car l) 0)
			         (- (* i 9.) 9.))))) )))

(defun bind| (l)
  (prog (type symb offset)
    (or (eq 'symbol (typep (setq symb (car l))))
        (barf symb "cannot bind symbol" data))
    (setq l (cdr l))
    (setq symb (get-const (list 'quote symb)))	;get address of var to be bound
    (cond ((cdr l)	;p|q
	 (or (= 0 (lapeval (car l)))	;better be ap|
	     (barf l "illegal bind word" data))
	 (or (eq '/| (cadr l)) (barf l "illegal bind word" data))
	 (setq l (cddr l))
	 (cond ((null l)		;no tag
	        (setq type 1))
	       (t (or (eq '* (cadr l)) (barf (cadr l) "illegal modifier in bind word" data))
	          (setq type 6)))	;temp indirect
	 (setq offset (lapeval (car l))))
	((eq (car l) t)		;other random things to bind to...
	 (setq type 0 offset 14))
	((eq (car l) nil)
	 (setq type 0 offset 12))
	((eq (car l) '*nargs)
	 (setq type 5 offset 0))
	((eq (car l) '*argatom)
	 (setq type 4 offset 0))
	((atom (car l))		;special var
	 (setq type 7
	       offset (1+ (get-constant-addr (get-const (list 'quote (car l)))))))
	((eq (caar l) 'quote)		;constant or literal
	 (setq offset (get-const (car l)))
	 (cond ((or (smallnump (cadr offset)) (floatp (cadr offset)))
	        (setq type 3 offset (cons 'bindliteral (- (get-literal-addr offset) pc))))
	       ((setq type 2 offset (1+ (get-constant-addr offset))))))
	((barf l "unrecognized expression in binding word" data)))
    (outlap offset (logor (lsh type 15.) (logand symb 77777))) ))

(defsym			; Symbols not defined by ALM
	sprpms	540000
	sprpop	541000
	sprptp	542000
	sprpcp	543000
	sprprp	545000
	adwpms	050000
	adwpop	051000
	adwptp	052000
	adwpcp	053000
	adwprp	151000
	call	272000	;=tspbp
	tspms	270000
	tspop	271000
	tsptp	272000
	tspcp	273000
	tsprp	675000
	sprims	250000
	spbpms	250400
	spriop	251400
	spbpop	251000
	spritp	252000
	spbptp	252400
	spricp	253400
	spbpcp	253000
	sprirp	651400
	spbprp	651000
	eawpms	310000
	easpms	310400
	eawpop	311400
	easpop	311000
	eawptp	312000
	easptp	312400
	eawpcp	313400
	easpcp	313000
	eawprp	331400
	easprp	331000
	eppus	627000		;=eax7
	eppms	350000
	epbpms	350400
	eppop	351400
	epbpop	351000
	epptp	352000
	epbptp	352400
	eppcp	353400
	epbpcp	353000
	epprp	371400
	epbprp	371000
	lprpms	760000
	lprpop	761000
	lprptp	762000
	lprpcp	763000
	lprprp	765000

	n	0
	au	1
	qu	2
	du	3
	ic	4
	al	5
	ql	6
	dl	7

	x0	10
	x1	11
	x2	12
	x3	13
	x4	14
	x5	15
	x6	16
	x7	17

	*	20
	au*	21
	qu*	22
	ic*	24
	al*	25
	ql*	26

	x0*	30
	x1*	31
	x2*	32
	x3*	33
	x4*	34
	x5*	35
	x6*	36
	x7*	37

	f1	40
	itp	41
	its	43
	sd	44
	scr	45
	f2	46
	f3	47

	ci	50
	i	51
	sc	52
	ad	53
	di	54
	dic	55
	id	56
	idc	57

	*n	60
	*au	61
	*qu	62
	*du	63
	*ic	64
	*al	33
	*ql	66
	*dl	67

	*x0	70
	*x1	71
	*x2	72
	*x3	73
	*x4	74
	*x5	75
	*x6	76
	*x7	77

	ap	0
	ab	1
	bp	2
	bb	3
	lp	4
	lb	5
	sp	6
	sb	7

	ms	0		;in case he reads lisp listings
	op	1
	tp	2
	cp	3
;	lp	4
	rp	5
;	sp	6
;	sb	7
	us	-1		;special unmkd kludge


	nil-offset 12
	t-offset 14

	bind	020
	unbind	022
	errset1 024
	errset2 026
	unerrset 030
	catch1	034
	catch2	036
	uncatch 040
	iogbind 046
	badgo	050
	throw1	052
	throw2	054
	signp	056
	type-fields 060
	return	062
	err	064
	cons	072
	ncons	074
	xcons	076
	begin-list 	100
	append-list 	102
	terminate-list 	104
	store-op	116
	float-store-op	120
	create-string-desc	130
	create-array-desc	132
	pl1-call		134
	cons-string	136
	create-varying-string  140
	compare	106

	fixnum-type 40047
	flonum-type 20047

	fixtype	400_24.
	flotype	200_24.
	bigtype	010_24.
	numtype	610_24.
	atomtype	770_24.
	strtype	40_24.
	subrtype	20_24.
)


; EIS instructions and descriptors

(mapc '(lambda (x) (putprop (car x) (cons 'desc (cdr x)) 'EIS))
       '(
(descb 000000 1 bit)
(desc9a 000000 9 char)
(desc6a 020000 6 char)
(desc4a 040000 4 char)
(desc9fl 000000 9 num)
(desc9ls 010000 9 num)
(desc9ts 020000 9 num)
(desc9ns 030000 9 num)
(desc4fl 040000 4 num)
(desc4ls 050000 4 num)
(desc4ts 060000 4 num)
(desc4ns 070000 4 num) ))

(mapc '(lambda (x) (putprop (car x) (list 'inst (cadr x) (or (caddr x) 'char)) 'EIS))
      '(
(mve 020400)
(mvne 024400)
(csl 060400 bit)
(csr 061400 bit)
(sztl 064400 bit)
(sztr 065400 bit)
(cmpb 066400 bit)
(mlr 100400)
(mrl 101400)
(cmpc 106400)
(scd 120400)
(scdr 121400)
(scm 124400)
(scmr 125400)
(mvt 160400)
(tct 164400)
(tctr 165400)
(ad2d 202400)
(sb2d 203400)
(mp2d 206400)
(dv2d 207400)
(ad3d 222400)
(sb3d 223400)
(mp3d 226400)
(dv3d 227400)
(mvn 300400)
(btd 301400)
(cmpn 303400)
(dtb 305400)
))

(defun cmp1 nil	
; translate a file compiling those S-expressions which try to define functions.

(catch  (prog (form tem being-compiled)


    a   (or (errset (setq form (read)))
	  first-eof
	  (progn	  (printmes nil "There is probably a missing "")""." nil)
		  (or (null current-function) (equal current-function '(nil))
		      (printmes current-function "was the last thing compiled." nil))
		  (return nil)))

        (setq current-function '(nil) being-compiled nil)
  b    (cond ((atom form))		;ignore atoms since no side effects to evaluation
	   ((eq (car form) '%include)		;interpreter %include statement is changed to compiler include dcl.
	    (cond ((errset ((lambda (errset)
			     (eval form))
			nil))
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

	  ((or (and (eq (car form) 'defprop) (eq (caddr form) 'macro))
	       (and (eq (car form) 'defun) (eq (cadr form) 'macro))
	       (and (eq (car form) 'defun) (eq (caddr form) 'macro)))
	   (eval form))			;do macro definition now
	  ((setq tem (get (car form) 'macro))		;do macro & rescan
	   (or (setq tem (errset (funcall tem form)))
	       (go c))
	   (setq form (car tem))
	   (go b))
            ((eq (car form) 'declare)
	    (setq current-function '(declare))
              (and (null (errset (mapc (function eval) (cdr form)))) 
	                   	          ;unless declarations lose, do them 
                   (go c))              ;and go to next expression in file
              (go a))
	   ((eq (car form) 'lap)
		(lapup (setq current-function (cadr form) being-compiled current-function) (caddr form) (cadddr form))
		(go a))
	   ((eq (car form) 'comment))		;no need to keep comments around
	   (t (put-in-tree form)))
        (go a)






    c (prog (^r ^w) (setq ^r nil ^w t)
	  (apply 'ioc messioc)
	  (princ "
lap: lisp error during declaration at top 
	level; the losing form is ")
	  (prinb form 5. 20.)			;display the losing form but limit the amount of typeout
	  (terpri)
	  (cond (dataerrp (princ "Please correct and type $p") (break dataerrp t) ))
	 )
      (go a))
     e-o-f)	;end of catch way back there
(finish-code)
t)




(defun cf (x)				;compile a file
        (prog (start-time start-runtime start-paging line tem ^w ^q ^r current-function
	      pc codelist constant-list functions-called functions-defined)
	(setq infile (openi x))
	(setq first-eof t)
	(setq source-map (list (namestring (names infile))))
	(eoffn infile (function (lambda (a1 a2)
				(cond (first-eof (setq first-eof nil) a1 a2 t)	;retry in case eof in obj
				      ((throw nil e-o-f)) ))))

	(setq seg-name (get_pname (cadr (names infile))))
	(setq start-time (status daytime))
	(setq start-runtime (status runtime) start-paging (status paging))
	(ioc q)

	(init-code-generator)
c	(cond ((atom (setq tem (errset (cmp1))))	;compile some function definitions
	       (setq ^q t ^w t ^r t line (cons current-function line))
	       (cond ((null tem)
		    ((lambda (^r ^w)
			(apply 'ioc messioc)
			(princ "
*** LISP ERROR WHILE ASSEMBLING ")
			(princ current-function)
			(princ "
    The error message from Lisp appears above.
")
			(break barfp barfp)		;in debug mode ,  let user fiddle.
			(go c))
		      nil t) ))
	       (go c)))				;keep on compiling the file
	(ioc svt)					;switch all i/o to tty
	(and line (printmes (sort line 'alphalessp) "- failed to assemble." nil))
	(close infile)
	(and total-time (prog (base *nopoint)		;print compiling statistics
		(setq base 10.)
		(setq *nopoint t)
		(princ "
Assembly finished.  Elapsed time = ")
		(pr-time (prog (a b c)
				(setq a (mapcar 'difference (status daytime) start-time))
				(setq c (caddr a) b (cadr a)
 a (car a))
				(and (minusp c) (setq c (+ c 60.) b (1- b)))
				(and (minusp b) (setq b (+ b 60.) a (1- a)))
				(and (minusp a) (setq a (+ a 24.)))	;if we crossed a midnight, patch it up.
								;;3-day compilations will still lose.
				(return (list a b c))))
		(princ ", runtime = ")
		(prin1 (//$ (float (setq start-runtime (difference (status runtime) start-runtime))) 1000000.0))
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
        ))



(defun pr-time(3list)		;routine to print out a time
				;called with base = 10., (status *nopoint) = t
	(pr-tim1 (car 3list))
	(tyo 72)			; ":"
	(pr-tim1 (cadr 3list))
	(tyo 72)
	(pr-tim1 (caddr 3list))  )

(defun pr-tim1(x)			;print 2 digit number with leading zero 
	(and (lessp x 10.) (tyo 60))	;put leading zero if needed
	(prin1 x))

(defun command-interface nil	;interpret the arguments of the 'lap xxx -opt' command
     (setq errlist '((init1)))	;we only want to get called once
     (terpri)
     (prog (i arg file hold listing-desired seg-name)
	(setq i 1)
nextarg	(or (setq arg (status arg i)) (go last-arg))	;go if no more arguments to do
	(cond ((equal (substr (get_pname arg) 1 1) "-")	;process an option
	       (cond   
		   ((memq arg '(-pathname -pn -p))
		    (setq file (status arg (setq i (1+ i)))))
		   ((eq arg '-eval)
		    (eval (readlist (exploden (status arg (setq i (1+ i)))))))
		   ((memq arg '(-tm -time -times)) (setq time-option t total-time t))
		   ((memq arg '(-tt -total -total_time))
		    (setq total-time t))
		   ((memq arg '(-nw -nowarn)) (setq nowarn t))
		   ((eq arg '-ioc) (eval (list 'ioc (status arg (setq i (1+ i))))))
		   ((memq arg '(-list -ls))
		    (setq listing-desired t))
		   ((memq arg '(-hd -hold)) (setq hold t))	;remain in lisp after compiling
		   (t (princ "lap: Unrecognized control argument ")
		      (princ arg)
		      (princ " has been ignored.")
		      (terpri))
		))
	      ((null file)
	       (setq file (mergef arg '(*.lap))))	;read pathname, put .lap on end. (use -pn if you don't want .lap)
	      (t (princ "lap: extra argument has been ignored: ")
	         (princ arg)
	         (terpri) ))
	(setq i (1+ i))
	(go nextarg)

last-arg	(and (null file) (return nil))		;if no file specified, enter lisp so he can use cf
	(princ "LISP Assembly Program
")						;announce ourselves
	(cf file)					;compile file
	(and listing-desired			;if -list option used, call make_lisp_listing
	     (make_lisp_listing seg-name))
	(or hold (quit))		;quit unless -hd option was given
     ))


(defun printmes(w msg warn)
  (or (and nowarn				;suppress warning s if called with the -nowarn option
	 (or (null warn) (eq warn 'warn)))
      ((lambda (^r ^w)
	  (apply 'ioc messioc)
	  (or warn (setq ^r nil))		;suppress output of random msgs to the defsubr file
	  (and warn being-compiled (progn
		(terpri)
		(princ "*** DIAGNOSTICS FOR ")
		(princ being-compiled)
		(terpri)
		(setq being-compiled nil)))	;so this header is only printed once per function in error.

	  (or (= (chrct (car outfiles)) (linel (car outfiles))) (terpri))	;get to left margin.
	  (princ (cdr (assq warn '(		;put message prefix
			(warn . "Warning: ")
			(nonfatal . "Error: ")
			(data . "Severe Error: ")
			(barf . "LAP Internal Error: ")
			(nil . "lap: ") ))))
	  (cond (w (cond (warn (prinb w 5. 20.)) ((prin1 w))) (tyo 40)))	;if there is a datum, print it
					;but limit the length of the output.
            (princ msg)                   	;print out the message
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
%%%%%%%% lap error - contact the lap maintenance persons %%%%%%%%")
                             (break barf barfp)
                             (err 'barf)))))
           nil  ;no value in particular
                )
      nil t)))


(defun prinb(x nlevels atom-cnt)		;print with limited output - for printmes
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



(defun prin-atom(x)		;routine to print an atom for printmes - knows about renaming.
    ((lambda (y)
	(and y (progn		;x is renamed version of y
		(and barfp	;in debug mode,...
		     (princ x)	;explain what's going on.
		     (princ '=))
		(setq x y))))	;and change atom to print to user's name for it
       (get x 'rename))
     (prin1 x))

(defun put-in-tree(x)
    (push (cons nil x) functions-defined))


; initialize environment.

(declare (eval (read)))
(progn (setsyntax '/! '/$ nil) (sstatus macro /| vertical-status))

(setq lapreadtable (array nil readtable t))

((lambda (readtable) (setsyntax '/| 'single nil)) lapreadtable)



(setq errlist '((command-interface) (init1)))

(defun init1 () (ioc stev) (terpri) (princ "At LISP top level: ") nil)


(sstatus interrupt 0 (function (lambda (args) (iog vt (prin1 current-function) (terpri)))))

(sstatus charmode nil)

(setq messioc '(vt)
      seg-name nil
      barfp nil
      dataerrp nil
      time-option nil
      total-time nil
      nowarn nil
   )
