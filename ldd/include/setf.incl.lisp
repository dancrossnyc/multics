;; -*- Mode: Lisp; Lowercase: True -*-

;; setf.incl.lisp - Loads lisp_setf_ into either the compiler or
;; interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature setf)
      (load (catenate (car (namelist (truename infile))) ">lisp_setf_"))))
