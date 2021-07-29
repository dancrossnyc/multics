;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

;; Written:  January 1980 by Alan Bawden
;; Modified:  March 1981 by Carl Hoffman
;;  Observe compiler "macros" flag.  Don't output an argument count check when
;;  the lambda list is (&optional ... &rest ...) or (&rest ...).
;; Modified:  March 1982 by Alan Bawden
;;  Now includes a macro macro.  That is, (macro foo (x) ...) is like 
;;  the good old (defun foo macro (x) ...).
;; Modified:  October 1982 by Carl Hoffman for installation in bound_lisp_library_

(%include backquote)
(%include destructuring_let)
(declare (macros t))

(declare (special macros))
(declare (*expr let-macro-flush-declares let-macro-cons-declares))

;; Note:  The "let" package must also be loaded at run time since defmacro
;; expands into "let" forms.  defmacro.incl.lisp does this.

(declare (special *rest* *normal* *optional* *body*))

(defun grok-&keyword-list (pattern body)
        (prog (pat x)
              (setq pat pattern)
	    (setq *body* body)
         norm (cond ((null pat) (return t))
                    ((atom pat) (setq *rest* pat) (return t)))
              (setq x (car pat))
              (cond ((eq x '&optional)
                     (go opt))
                    ((memq x '(&rest &body))
                     (go rst))
                    ((eq x '&aux)
                     (go ax)))
              (setq *normal* (cons x *normal*)
                    pat (cdr pat))
              (go norm)
          opt (cond ((null (setq pat (cdr pat))) (return t))
                    ((atom pat) (setq *rest* pat) (return t)))
              (setq x (car pat))
              (cond ((eq x '&optional)
                     (go barf))
                    ((memq x '(&rest &body))
                     (go rst))
                    ((eq x '&aux)
                     (go ax)))
              (cond ((atom x)
                     (setq *optional* (cons (list x nil) *optional*)))
                    (t
                     (setq *optional* (cons x *optional*))))
              (go opt)
          rst (or (and (not (null (setq pat (cdr pat))))
                       (not (atom pat))
                       (atom (setq x (car pat))))
                  (go barf))
              (setq *rest* x)
              (or (setq pat (cdr pat))
                  (return t))
              (and (or (atom pat)
                       (not (eq (car pat) '&aux)))
                   (go barf))
           ax (setq *body* `((let* ,(cdr pat) . ,*body*)))
              (return t)
         barf (error "Bad &keyword argument list: " pattern 'fail-act)))

(defprop defmacro defmacro/ macro macro)

(defun defmacro/ macro (x)
 (displace x
  (let (((nil name pattern . body) x)
        (var (gensym))
        *normal* *optional* *rest* *body*
        nname check-args guts tail)
    ;; make_atom returns an interned result.
    (setq nname (make_atom (catenate name " macro")))
    (grok-&keyword-list pattern (let-macro-flush-declares body))
    (setq check-args (length *normal*))
    ;; nreconc is broken.  This uses esoteric features of nconc:
    ;; (nconc '(a b c) 'd) --> (a b c . d)
    ;; (nconc nil 'x) -> x
    (setq *normal*
	(nconc (nreverse *normal*)
	       (cond ((null *optional*) *rest*)
		   (t (setq tail (gensym))))))
    (setq check-args
      (let ((cnd (cond ((null *rest*)
		    (cond ((null tail)
			 `(= (length ,var) ,(1+ check-args)))
			(t `(and (> (length ,var) ,check-args)
			         (< (length ,var)
				  ,(+ 2 check-args 
				      (length *optional*)))))))
		   ;; If we have a rest var, then there is no upper bound.
		   ;; If *normal* is atomic, then there is no lower bound.
		   ((and (atom *normal*) *rest*) nil)
		   (t `(> (length ,var) ,check-args)))))
        (and cnd
	   `((or ,cnd (error "Wrong number of args in macro form: "
			 ,var 'fail-act))))))
    (setq guts
      `((,var)
        .,(let-macro-cons-declares body
           `((comment args = ,pattern)
	   ,@check-args
	   (displace ,var
	     (let ((,*normal* (cdr ,var)))
	        . ,(cond (tail
		        (do ((o *optional* (cdr o))
			   (ps nil (cond ((null (cddar o)) ps)
				       (t (cons (caddar o) ps))))
			   (b (cond (*rest*
				    `((let ((,*rest* (cdr ,tail)))
					 . ,*body*)))
				  (t *body*))
			      `((let ((,(caar o)
				      (cond (,(cond ((null (cdr o))
						 tail)
						(t
						  `(setq ,tail (cdr ,tail))))
					    ,@(cond ((null (cddar o)) nil)
						  (t `((setq ,(caddar o)
							   t))))
					    (car ,tail))
					  (t ,(cadar o)))))
				   . ,b))))
			  ((null o)
			   (cond ((null ps) b)
			         (t `((let ,ps . ,b)))))))
		       (t *body*))))))))
    `(eval-when ,(cond ((and (boundp 'macros) macros) '(eval load compile))
		   (t '(eval compile)))
	      (defprop ,name ,nname macro)
	      (defun ,nname . ,guts)))))

;;;Probably defmacro should use this, but lets not toy with success.
(defprop macro macro/ macro macro)

(defun macro/ macro (x)
 (displace x
  (let (;; make_atom returns an interned result.
        (nname (make_atom (catenate (cadr x) " macro"))))
    `(eval-when ,(cond ((and (boundp 'macros) macros) `(eval load compile))
		   (t `(eval compile)))
	      (defprop ,(cadr x) ,nname macro)
	      (defun ,nname . ,(cddr x))))))

(sstatus feature defmacro)
