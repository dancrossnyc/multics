;; -*- Mode: Lisp; Lowercase: True -*-

;; other_other.incl.lisp - Loads lisp_setf_ and lisp_other_other_ into either
;; the compiler or interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature setf)
      (load (catenate (car (namelist (truename infile))) ">lisp_setf_")))
  (or (status feature other_other)
      (load (catenate (car (namelist (truename infile))) ">lisp_other_other_"))))
