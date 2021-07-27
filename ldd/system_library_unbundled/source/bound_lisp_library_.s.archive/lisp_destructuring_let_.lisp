;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

(%include backquote)
(declare (macros t))

(declare (special let-macro-vals))

(defprop let let/ macro macro)

(defun let/ macro (x)
 (displace x
  (do ((body (let-macro-flush-declares (cddr x)))
       (pairs (reverse (cadr x)) (cdr pairs))
       (vars nil)
       (let-macro-vals nil)
       (tem))
      ((null pairs)
       (setq body (let-macro-cons-declares (cddr x) body))
       (cond ((or (not (null vars))
	        (not (null (cdr body))))
	    `((lambda ,vars . ,body) . ,let-macro-vals))
	   (t (car body))))
      (cond ((atom (car pairs))
	   (or (symbolp (car pairs))
	       (error "Garbage found in LET pattern: " (car pairs) 'fail-act))
	   (setq vars (cons (car pairs) vars))
	   (setq let-macro-vals (cons nil let-macro-vals)))
	  (t
	    (setq tem vars)
	    (setq vars (let-macro-get-vars (caar pairs) vars))
	    (or (eq tem vars)
	        (setq body (nconc (let-macro-hair (caar pairs)
					  (cadar pairs)
					  let-macro-vals)
			      body))))))))

(defun let-macro-get-vars (pattern vars)
  (cond ((null pattern) vars)
        ((atom pattern)
         (or (symbolp pattern)
	   (error "Garbage found in LET pattern: " pattern 'fail-act))
         (setq let-macro-vals (cons nil let-macro-vals))
         (cons pattern vars))
        (t (let-macro-get-vars (cdr pattern)
			 (let-macro-get-vars (car pattern) vars)))))

(defun let-macro-flush-declares (body)
  (cond ((or (atom body)
	   (atom (car body))
	   (not (eq (caar body) 'declare)))
         body)
        (t (let-macro-flush-declares (cdr body)))))

(defun let-macro-cons-declares (obody nbody)
  (cond ((or (atom obody)
	   (atom (car obody))
	   (not (eq (caar obody) 'declare)))
         nbody)
        (t (cons (car obody) (let-macro-cons-declares (cdr obody) nbody)))))

(defprop desetq desetq/ macro macro)

(defun desetq/ macro (x)
 (displace x
  (do ((p (cdr x) (cddr p))
       (body nil)
       (tem))
      ((null p) `(progn . ,body))
      (cond ((atom (cdr p))
	   (error "Odd number of args to DESETQ: " x 'fail-act))
	  ((atom (car p))
	   (or (symbolp (car p))
	       (error "Garbage found in DESETQ pattern: " (car p) 'fail-act))
	   (and (null (car p))
	        (error "Bad DESETQ pattern: " (car p) 'fail-act))
	   (setq body (nconc body `((setq ,(car p) ,(cadr p))))))
	  (t
	    (setq tem (cons nil nil))
	    (setq body (nconc body
			  `((setq ,(let-macro-get-last-var (car p))
				. ,tem)
			    . ,(let-macro-hair (car p) (cadr p) tem)))))))))

(defun let-macro-get-last-var (pattern)
       (cond ((atom pattern) pattern)
             (t
              (or (let-macro-get-last-var (cdr pattern))
                  (let-macro-get-last-var (car pattern))))))

(defun let-macro-hair (pattern code cell)
       (cond ((null pattern) nil)
             ((atom pattern)
              (rplaca cell code)
              nil)
             (t
              ((lambda (avar dvar)
                    (cond ((null avar)
                           (cond ((null dvar) nil)
                                 (t (let-macro-hair (cdr pattern)
                                                    `(cdr ,code)
                                                    cell))))
                          ((null dvar)
                           (let-macro-hair (car pattern)
                                           `(car ,code)
                                           cell))
                          (t
                           (rplaca cell code)
                           ((lambda (acell dcell)
                                 (cons `(setq ,avar . ,acell)
                                       (nconc (let-macro-hair (car pattern)
                                                              `(car ,dvar)
                                                              acell)
                                              (cons `(setq ,dvar . ,dcell)
                                                    (let-macro-hair (cdr pattern)
                                                                    `(cdr ,dvar)
                                                                    dcell)))))
                            (cons nil nil)
                            (cons nil nil)))))
               (let-macro-get-last-var (car pattern))
               (let-macro-get-last-var (cdr pattern))))))

(defprop let* let*/ macro macro)

(defun let*/ macro (x)
 (displace x
  (cond ((null (cdadr x)) `(let . ,(cdr x)))
        (t
	(do ((a (reverse (cadr x)) (cdr a))
	     (b (let-macro-flush-declares (cddr x))
	        `((let (,(car a)) . ,b))))
	    ((null (cdr a))
	     `(let (,(car a)) . ,(let-macro-cons-declares (cddr x) b))))))))

(sstatus feature destructuring_let)
