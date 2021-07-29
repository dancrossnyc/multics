;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

;; Lisp Machine sharpsign reader macro
;; Written:	May 1980 by Alan Bawden
;; Modified:	June 1981 by Carl Hoffman and Richard Lamson to remove
;;		defmacro dependencies for installation.
;;		October 1982 by Carl Hoffman and Richard Lamson for
;;		installation in bound_lisp_library_

(%include backquote)

(declare (special sharpsign-data-list))
(setq sharpsign-data-list nil)

(defun sharpsign-set-syntax (ch type fun)
  (cond ((numberp ch))
        ((or (symbolp ch)
	   (stringp ch))
         (setq ch (CtoI ch)))
        (t (error "sharpsign-set-syntax: Not a character - " ch 'fail-act)))
  (or (memq type '(macro peek splicing peek-macro peek-splicing nil))
      (error "sharpsign-set-syntax: Not a valid type - " ch 'fail-act))
  (or (< ch 141)			;#/a
      (> ch 172)			;#/z
      (setq ch (- ch 40)))		;upper case it.
  (let ((peekp   (not (null (memq type '(peek peek-macro peek-splicing)))))
        (splicep (not (null (memq type '(splicing peek-splicing)))))
        (tem     (assoc ch sharpsign-data-list)))
       (cond ((or (null fun) (null type))
	    (or (null tem)
	        (setq sharpsign-data-list (delq tem sharpsign-data-list))))
	   ((null tem)
	    (setq sharpsign-data-list
		`((,ch ,peekp ,splicep . ,fun) . ,sharpsign-data-list)))
	   (t (rplacd tem `(,peekp ,splicep . ,fun))))))

(defun sharpsign-reader-macro ()
 (do ((ch) (arg nil))
     (nil)
   (setq ch (tyipeek))
   (cond ((not (or (< ch 60)			;#/0
	         (> ch 71)))		;#/9
	(tyi)
	(cond ((null arg) (setq arg (- ch 60)))
	      (t (setq arg (+ (* arg 10.) (- ch 60))))))
         (t (cond ((not (null arg)))
	        ((= ch 2)
	         (tyi)
	         (setq ch (tyipeek))
	         (setq arg 'control))
	        ((= ch 3)
	         (tyi)
	         (setq ch (tyipeek))
	         (setq arg 'meta))
	        ((= ch 6)
	         (tyi)
	         (setq ch (tyipeek))
	         (setq arg 'control-meta)))
	  (or (< ch 141)		;#/a
	      (> ch 172)		;#/z
	      (setq ch (- ch 40)))	;upper case it.
	  (let ((x (assoc ch sharpsign-data-list)))
	   (and (null x)
	    (error "sharpsign-reader-macro: Unknown character following # - "
		 (ItoC ch) 'fail-act))
	   (and (null (cadr x)) (tyi))
	   (return
	     (cond ((null (caddr x))
		  (list (funcall (cdddr x) arg)))
		 (t (funcall (cdddr x) arg)))))))))

;; Defsharp is used by this file at eval/compile time.  We also place
;; the macro in the object segment for the benefit of users who wish
;; to extend the sharpsign syntax.

(declare (macros t))

(eval-when (eval load compile)
  (defprop defsharp defsharp-macro macro)
  (defun defsharp-macro (form)
    (let ((ch   (cadr form))
	(type (caddr form))
	(args (cadddr form))
	(body (cddddr form)))
       (let ((name (intern (make_atom (catenate "sharpsign-" ch "-macro")))))
	  `(progn 'compile
		(sharpsign-set-syntax ',ch ',type ',name)
		(defun ,name ,args . ,body))))))

(defsharp // macro (arg) arg ;Ignored
	(tyi))
	
(defsharp /M splicing (arg) arg ;Ignored
	nil)

(defsharp /Q splicing (arg) arg ;Ignored
	(read)
	nil)

(defsharp /N splicing (arg) arg ;Ignored
	(read)
	nil)

(defsharp /R macro (arg)
	(or (numberp arg)
	    (error "#<digits>R please, not: " arg 'fail-act))
	(let ((ibase arg))
	     (read)))

(defsharp /O macro (arg) arg ;Ignored
	(let ((ibase 8.)) (read)))

(defsharp /D macro (arg) arg ;Ignored
	(let ((ibase 10.)) (read)))

;; This doesn't work completely since a sequence of letters and digits
;; will be read as a symbol rather than a fixnum, i.e. "#x 10" reads as 16
;; decimal, but "#x A" reads as the symbol A.  Perhaps this is what
;; the (sstatus + t) is trying to fix? -cwh

(defsharp /X macro (arg) arg ;Ignored
	(unwind-protect
	  (let ((ibase 16.))
	       (sstatus + t)
	       (read))
	  (sstatus + nil)))

(defsharp /' macro (arg) arg ;Ignored
	(list 'function (read)))

(defsharp /. macro (arg) arg ;Ignored
	(eval (read)))

(declare (special gofoo compiler-state))

(defsharp /, macro (arg) arg ;Ignored
	(cond ((and (boundp 'compiler-state) compiler-state)
	       (cons (read) gofoo))
	      (t (eval (read)))))

(defsharp /# macro (arg) arg ;Ignored
	(error "Barf! ## is obsolete, you should use #/  " nil 'fail-act))

(defsharp /^ macro (arg) arg ;Ignored
	(boole 1 37 (tyi)))

(declare (special sharpsign-character-alist))

(setq sharpsign-character-alist
      '(("null" . 0)
        ("bell" . 7)
        ("bs" . 10) ("backspace" . 10)
        ("tab" . 11)
        ("lf" . 12) ("linefeed" . 12) ("newline" . 12)
        ("vt" . 13)
        ("ff" . 14) ("form" . 14) ("formfeed" . 14)
        ("return" . 15) ("cr" . 15)
        ("altmode" . 33) ("alt" . 33) ("escape" . 33) ("esc" . 33)
        ("space" . 40) ("sp" . 40)  
        ("help" . 77)
        ("delete" . 177) ("rubout" . 177)))

(defsharp /\ macro (arg) arg ;Ignored
  (let ((name (get_pname (read))))
    (cdr (or (assoc name sharpsign-character-alist)
	   (error "Unknown symbolic name following #\ - " name 'fail-act)))))

(defsharp /+ splicing (arg) arg ;Ignored
	(or (sharpsign-feature-test (read))
	    (read))
	nil)

(defsharp /- splicing (arg) arg ;Ignored
	(and (sharpsign-feature-test (read))
	     (read))
	nil)

(defun sharpsign-feature-test (frob)
  (cond ((atom frob)
         (apply 'status `(feature ,frob)))
        ((eq (car frob) 'and)
         (do ((l (cdr frob) (cdr l)))
	   ((atom l) t)
	   (or (sharpsign-feature-test (car l))
	       (return nil))))
        ((eq (car frob) 'or)
         (do ((l (cdr frob) (cdr l)))
	   ((atom l) nil)
	   (and (sharpsign-feature-test (car l))
	        (return t))))
        ((eq (car frob) 'not)
         (not (sharpsign-feature-test (cadr frob))))
        (t (error "sharpsign-feature-test: Bad form after #+ or #- - "
	        frob 'fail-act))))

(setsyntax '/# 'splicing 'sharpsign-reader-macro)

(sstatus feature sharpsign)
