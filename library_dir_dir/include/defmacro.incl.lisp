;; -*- Mode: Lisp; Lowercase: True -*-

;; defmacro.incl.lisp - Loads lisp_backquote_, lisp_destructuring_let_,
;; and lisp_defmacro_ into either the compiler or interpreter environment.
;; Written:  October 1982 by Carl Hoffman

;; Defmacro needs destructuring_let to run.
;; It can run without backquote, but would be useless.

(eval-when (eval compile)
  (or (status feature backquote)
      (load (catenate (car (namelist (truename infile))) ">lisp_backquote_")))
  (or (status feature destructuring_let)
      (load (catenate (car (namelist (truename infile))) ">lisp_destructuring_let_")))
  (or (status feature defmacro)
      (load (catenate (car (namelist (truename infile))) ">lisp_defmacro_"))))

;; This is necessary for (defprop a b macro) forms and defuns produced
;; by defmacro to appear in the object segment.  Let the default be
;; the right thing for naive users.
(declare (macros t))
