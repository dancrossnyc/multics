;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

;; Be careful.  lisp_defun_ uses this file, so don't use &mumbles
;; in procedure definitions.
;; setf is needed at runtime since push, pop, etc. expand into it.
;; This file corresponds in part to LSPSRC;UMLMAC on MIT-MC.

(%include defmacro)
(%include macro_macros)
(%include setf)

;; Functional macros.  These should be redefined as open-codable subrs.

(defmacro logand (&rest x) `(boole 1 . ,x))
(defmacro logior (&rest x) `(boole 7 . ,x))
(defmacro logxor (&rest x) `(boole 6 . ,x))
(defmacro lognot (x)       `(boole 10. ,x -1))
  
(defmacro bit-test  (x y) `(not (= (logand ,x ,y) 0)))
(defmacro bit-set   (x y) `(boole 7 ,x ,y))
(defmacro bit-clear (x y) `(boole 2 ,x ,y))

(defmacro fifth   (x) `(car (cddddr ,x)))
(defmacro sixth   (x) `(cadr (cddddr ,x)))
(defmacro seventh (x) `(caddr (cddddr ,x)))
(defmacro eighth  (x) `(cadddr (cddddr ,x)))

(defmacro rest5 (x) `(cdr (cddddr ,x)))
(defmacro rest6 (x) `(cddr (cddddr ,x)))
(defmacro rest7 (x) `(cdddr (cddddr ,x)))
(defmacro rest8 (x) `(cddddr (cddddr ,x)))

(defmacro evenp   (x)   `(not (oddp ,x)))
(defmacro neq     (x y) `(not (eq ,x ,y)))
(defmacro nequal  (x y) `(not (equal ,x ,y)))
(defmacro fixnump (x)   `(eq (typep ,x) 'fixnum))
(defmacro flonump (x)   `(eq (typep ,x) 'flonum))

;; This is now incompatible.  It will make a very bad macro with its new
;; definition, since that definition must also check for NIL.

(defmacro listp (object) `(not (atom ,object)))

(defmacro copylist (list) `(append ,list nil))

(defmacro aref rest `(arraycall t . ,rest))

;; Must be careful of the order of evaluation here.
;; (defmacro aset (val . rest) `(store (aref . ,rest) ,val))
;; will result in "rest" being evaluated before val.  A good open-codable
;; subr mechanism must be able to handle this.

(defmacro aset (val . rest)
	(let ((var (gensym)))
	     `((lambda (,var)
		     (store (arraycall t . ,rest) ,var))
	       ,val)))

;; (<= A B) --> (NOT (> A B))
;; (<= A B C) --> (NOT (OR (> A B) (> B C)))
;; Funny arglist to check for correct number of arguments.

(defmacro <= (arg1 arg2 &rest rest)
  (<=-expander '> (list* arg1 arg2 rest)))

(defun <=-expander (relation args)
  (cond ((null (cddr args))
         `(not (,relation ,(car args) ,(cadr args))))
        (t (do ((l (reverse args) (cdr l))
	      (nargs nil (cons (cond ((and (atom (car l))
				     (or (null vars)
				         (not (symbolp (car l)))))
				(car l))
			         (t (setq vals (cons (car l) vals))
				  (let ((x (gensym)))
				    (setq vars (cons x vars))
				    x)))
			   nargs))
	      (vars nil)
	      (vals nil))
	     ((null l)
	      (do ((l (cdr nargs) (cdr l))
		 (forms (list `(,relation ,(car nargs) ,(cadr nargs)))
		        (cons `(,relation ,(car l) ,(cadr l)) forms)))
		((null (cdr l))
		 (let ((form `(not (or ,.(nreverse forms)))))
		   (cond ((null vars) form)
		         (t `((lambda ,vars ,form) ,.vals)))))))))))

;; (>= A B) --> (NOT (< A B))
;; (>= A B C) --> (NOT (OR (< A B) (< B C)))
;; Funny arglist to check for correct number of arguments.

(defmacro >= (arg1 arg2 &rest rest)
  (<=-expander '< (list* arg1 arg2 rest)))


;; Control structure macros

;; It is important that (IF NIL <FORM>) returns NIL as Macsyma code depends
;; upon this in places.  Macsyma unfortunately also relies on the ability to
;; have multiple else clauses.

(defmacro ITS-if (predicate then &rest else)
          (cond ((null else) `(cond (,predicate ,then) (t nil)))
                (t `(cond (,predicate ,then) (t . ,else)))))

(defmacro ITS-ifn (predicate then &rest else)
	(cond ((null else) `(cond ((not ,predicate) ,then) (t nil)))
	      (t `(cond ((not ,predicate) ,then) (t . ,else)))))

;; For the benefit of people who use the Multics Emacs version of if we try
;; to avoid redefining it if it is already defined.

(eval-when (eval load compile)
	 (cond ((null (get 'if 'macro))
	        (putprop 'if (get 'ITS-if 'macro) 'macro)
	        (putprop 'ifn (get 'ITS-ifn 'macro) 'macro))))

;; Funny arglists so as to do better argument checking.

(defmacro when (predicate then-1 . then-rest)
	`(cond (,predicate ,then-1 . ,then-rest)))

(defmacro unless (predicate then-1 . then-rest)
	`(cond ((not ,predicate) ,then-1 . ,then-rest)))

;; Variations on setf
;; (push a x)     --> (setq x (cons a x))
;; (pop x)        --> (setq x (cdr x))
;; (incf x)       --> (setq x (1+ x))
;; (decf x)       --> (setq x (1- x))
;; (negf x)       --> (setq x (- x))
;; (notf x)       --> (setq x (not x))

(defmacro push (val var) `(setf ,var (cons ,val ,var)))

(defmacro pop (var &optional (into nil into?))
  (if into?
      `(prog1 (setf ,into (car ,var)) (setf ,var (cdr ,var)))
      `(prog1 (car ,var) (setf ,var (cdr ,var)))))

(defmacro incf (counter &optional increment)
  (if increment
      `(setf ,counter (+ ,counter ,increment))
      `(setf ,counter (1+ ,counter))))

(defmacro decf (counter &optional decrement)
  (if decrement
      `(setf ,counter (- ,counter ,decrement))
      `(setf ,counter (1- ,counter))))

(defmacro negf (integer) `(setf ,integer (- ,integer)))
(defmacro notf (switch) `(setf ,switch (not ,switch)))

;; Dispatchers

(defmacro case x `(select . ,x))
(defmacro caseq x `(selectq . ,x))

;; Give (select x ((a b) 3)), (cond ((memq x (list a b)) 3)) is generated.
;; select and select-equal should be rewritten to instead generate
;; (cond ((or (eq x a) (eq x b)) 3)).  This doesn't cons and can save
;; additional computing if "a" and "b" are both forms, and the first case
;; is true.

(defmacro select (var . lists)
  (once-only (var)
    (do ((lists lists (cdr lists))
         (ans nil (cons (cons (cond ((memq (caar lists) '(t otherwise)) t)
			      ((atom (caar lists))
			       `(eq ,var ,(caar lists)))
			      (t `(memq ,var (list . ,(caar lists)))))
			(cdar lists))
		    ans)))
        ((null lists) `(cond . ,(nreverse ans))))))

(defmacro selectq (var . lists)
  (once-only (var)
    (do ((lists lists (cdr lists))
         (ans nil (cons (cons (cond ((memq (caar lists) '(t otherwise)) t)
			      ((atom (caar lists))
			       `(eq ,var ',(caar lists)))
			      (t `(memq ,var ',(caar lists))))
			(cdar lists))
		    ans)))
        ((null lists) `(cond . ,(nreverse ans))))))

;; select-equal and selectq-equal are not found in the LM.
;; they are like select and selectq, but use equal and member
;; JRDavis 19 March 1981

(defmacro select-equal (var . lists)
  (once-only (var)
    (do ((lists lists (cdr lists))
         (ans nil (cons (cons (cond ((memq (caar lists) '(t otherwise)) t)
			      ((atom (caar lists))
			       `(equal ,var ,(caar lists)))
			      (t `(member ,var (list . ,(caar lists)))))
			(cdar lists))
		    ans)))
        ((null lists) `(cond . ,(nreverse ans))))))

(defmacro selectq-equal (var . lists)
  (once-only (var)
    (do ((lists lists (cdr lists))
         (ans nil (cons
		(cons
		  (cond ((memq (caar lists) '(t otherwise)) t)
		        ((atom (caar lists)) `(equal ,var ',(caar lists)))
		        (t `(member ,var ',(caar lists))))
		  (cdar lists))
		ans)))
        ((null lists) `(cond . ,(nreverse ans))))))

(defmacro dotimes ((var form) . body)
	(once-only (form)
               `(do ((,var 0 (1+ ,var)))
                    ((not (< ,var ,form)))
                    . ,body)))

(defmacro dolist ((var form) . body)
          (let ((dum (gensym)))
               `(do ((,dum ,form (cdr ,dum))
		 (,var))
                    ((null ,dum))
                    (setq ,var (car ,dum))
                    . ,body)))

;; Perhaps we should do a code walk over the setq and macroexpand all
;; forms until the compiler is able to do this.  -cwh

(defmacro defconst (var . initp)
  (cond (initp
	`(progn 'compile
	        (declare (special ,var))
	        (setq ,var ,(car initp))))
        (t `(declare (special ,var)))))

(defmacro ITS-defvar (var . initp)
  (cond (initp
	`(progn 'compile
	        (declare (special ,var))
	        (or (boundp ',var)
		  (setq ,var ,(car initp)))))
        (t `(declare (special ,var)))))

;; Just like if we have to be careful about redefining the e-macros defvar.

(eval-when (eval load compile)
	 (cond ((null (get 'defvar 'macro))
	        (putprop 'defvar (get 'ITS-defvar 'macro) 'macro))))

;; (*CATCH 'TAG (COMPUTE)) --> (CATCH (COMPUTE) TAG)
;; This is a kludge to handle the common cases.  This should be implemented
;; correctly in the future.

(defmacro *catch (tag body)
  (cond ((or (memq tag '(t nil))
	   (and (not (atom tag)) (eq (car tag) 'quote)))
         `(catch ,body ,(cadr tag)))
        (t (error "*catch: Tag must be a quoted symbol - " tag
	        'wrng-type-arg))))

(defmacro *throw (tag body)
  (cond ((or (memq tag '(t nil))
	   (and (not (atom tag)) (eq (car tag) 'quote)))
         `(throw ,body ,(cadr tag)))
        (t (error "*throw: Tag must be a quoted symbol - " tag
	        'wrng-type-arg))))

;; This checks for an even number of arguments.

(defmacro psetq (var value . rest)
  (cond (rest `(setq ,var (prog1 ,value (psetq . ,rest))))
        (t `(setq ,var ,value))))

(defmacro lexpr-funcall (function &rest args)
  (cond ((null args) `(funcall ,function))
        ((null (cdr args)) `(apply ,function ,(car args)))
        ((null (cddr args)) `(apply ,function (cons . ,args)))
        (t `(apply ,function (list* . ,args)))))

;; We can't simply write (let ((,var (nointerrupt t))) (unwind-protect ...))
;; Since we may unwind the stack after entering the "let" and before
;; entering the "unwind-protect"

(defmacro without-interrupts (&rest body &aux (var (gensym)))
  `(let ((,var 'not-set-yet))
     (unwind-protect
       (progn (setq ,var (nointerrupt t)) . ,body)
       (unless (eq ,var 'not-set-yet)
	     (nointerrupt ,var)))))

(defmacro without-tty-interrupts (&rest body &aux (var (gensym)))
  `(let ((,var 'not-set-yet))
    (unwind-protect
      (progn (setq ,var (nointerrupt 'tty)) . ,body)
      (unless (eq ,var 'not-set-yet)
	    (nointerrupt ,var)))))

;; This crock is necessary since (open f 'in) and (open f 'out) don't
;; work.  Have to use openi and openo.

;; (defmacro with-open-file ((stream filename options) . body)
;;   `(let ((,stream nil))
;;     (unwind-protect (progn (setq ,stream (open ,filename ,options))
;; 		       . ,body)
;; 		(if ,stream (close ,stream)))))

(defmacro with-open-file ((stream filename options) . body)
  (let ((keyword (cadr options))
        (open-form))
    (setq open-form (cond ((eq keyword 'in)     `(openi ,filename))
		      ((eq keyword 'out)    `(openo ,filename))
		      ((eq keyword 'append) `(opena ,filename))
		      (t `(open ,filename ,options))))
    `(let ((,stream nil))
      (unwind-protect (progn (setq ,stream ,open-form)
		         . ,body)
		  (if ,stream (close ,stream))))))

(defmacro circular-list (&rest args)
  `(let ((x (list . ,args)))
     (rplacd (last x) x)
     x))

(sstatus feature other_other)
