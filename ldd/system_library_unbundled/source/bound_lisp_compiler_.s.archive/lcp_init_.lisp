;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1973 *
;;; *                                                            *
;;; **************************************************************
;;; this segment serves the sole purpose of initializing the
;;; obarray handling mechanism of the Multics LISP compiler.
;;; Coded by D. Reed 11/23/73

; first, make an obarray to load the compiler in on, and
; an obarray to use to generate new virgin obarrays.

(makoblist 'initial-readin-obarray)
(makoblist 'compiler-obarray)

; now define a function for convenient obarray switching.

(defun use fexpr (x)
	(setq x (getchar (car x) 1))		; get the first char of the argument.
	(cond ((samepnamep x "c") (setq obarray (get 'compiler-obarray 'array))
			      'compiler-obarray)
	      ((samepnamep x "w") (setq obarray (get 'obarray 'array))
			      'working-obarray)
	      ((samepnamep x "n") (setq obarray (get 'initial-readin-obarray 'array))
			      (makoblist 'obarray)	; copy it
			      (setq obarray (get 'obarray 'array))
			      'new-working-obarray)
	      (t (princ "
use: argument must be c, w, or n.
")
		nil)))



(defun global fexpr (x)
   ((lambda (obarray)
     (mapc '(lambda (y)
		(setq x (intern y))
		(cond ((eq x y))		; successful intern
		      (t (remob x)		; force it to be global
		         (intern y)
		         (princ "
global: forcing the atom """)
			(prin1 y)
		         (princ """ to be global by remob'ing one with the same name")
			)))
	 x))
    (get 'obarray 'array)))
