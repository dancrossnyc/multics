;; -*- Mode: Lisp; Lowercase: True -*-

;; defstruct.incl.lisp - Loads lisp_defstruct_ and lisp_setf_ into either the
;; compiler or interpreter environment.
;; Written:	October 1982 by Alan Bawden, Carl Hoffman, and Rich Lamson

(eval-when (eval compile)
  (or (status feature setf)
      (load (catenate (car (namelist (truename infile))) ">lisp_setf_")))
  (or (status feature defstruct)
      (load (catenate (car (namelist (truename infile))) ">lisp_defstruct_"))))
