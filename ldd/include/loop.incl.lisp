;; -*- Mode: Lisp; Lowercase: True -*-

;; loop.incl.lisp - Loads lisp_loop_ into either the compiler or
;; interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature loop)
      (load (catenate (car (namelist (truename infile))) ">lisp_loop_"))))
