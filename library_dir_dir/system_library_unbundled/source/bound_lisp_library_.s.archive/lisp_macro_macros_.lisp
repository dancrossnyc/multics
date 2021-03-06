;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

(%include defmacro)

;; Macros used in the definition of other macros.

(defmacro once-only (vars . body)
   (setq vars (reverse vars))
   (do ((l vars (cdr l))
        (nils nil (cons nil nils))
        (bind-vars (gensym))
        (bind-vals (gensym))
        (tem (gensym)))
       ((null l)
        `((lambda (,bind-vars ,bind-vals)
	   ((lambda (result)
		  (cond ((null ,bind-vars)
		         result)
		        (t
			`((lambda ,,bind-vars ,result) . ,,bind-vals))))
	    ((lambda ,vars
		   . ,body)
	     . ,(do ((l vars (cdr l))
		   (inits nil (cons `(cond ((atom ,(car l))
				        ,(car l))
				       (t
				         (let ((,tem (gensym)))
					    (setq ,bind-vars (cons ,tem ,bind-vars))
					    (setq ,bind-vals (cons ,(car l) ,bind-vals))
					    ,tem)))
				inits)))
		  ((null l) (nreverse inits))))))
	nil
	nil))
       (or (symbolp (car l))
	 (error "Not a symbol (ONCE-ONLY): " (car l)))))

(sstatus feature macro_macros)
