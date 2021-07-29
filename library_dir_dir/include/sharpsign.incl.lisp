;; -*- Mode: Lisp; Lowercase: True -*-

;; sharpsign.incl.lisp - Loads lisp_sharpsign_ into either the compiler or
;; interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature sharpsign)
      (load (catenate (car (namelist (truename infile))) ">lisp_sharpsign_"))))
