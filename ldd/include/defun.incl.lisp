;; -*- Mode: Lisp; Lowercase: True -*-

;; defun.incl.lisp - Loads lisp_destructuring_let_, lisp_defmacro_,
;; and lisp_defun_ into either the compiler or interpreter environment.
;; Written:  October 1982 by Carl Hoffman

;; defun needs destructuring_let and defmacro to run.  The code produced contains
;; let forms, and an internal routine in defmacro is called.

(eval-when (eval compile)
  (or (status feature destructuring_let)
      (load (catenate (car (namelist (truename infile))) ">lisp_destructuring_let_")))
  (or (status feature defmacro)
      (load (catenate (car (namelist (truename infile))) ">lisp_defmacro_")))
  (or (status feature defun)
      (load (catenate (car (namelist (truename infile))) ">lisp_defun_"))))
