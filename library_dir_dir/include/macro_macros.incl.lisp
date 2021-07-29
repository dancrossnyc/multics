;; -*- Mode: Lisp; Lowercase: True -*-

;; macro_macros.incl.lisp - Loads lisp_macro_macros_ into either the compiler or
;; interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature macro_macros)
      (load (catenate (car (namelist (truename infile))) ">lisp_macro_macros_"))))
