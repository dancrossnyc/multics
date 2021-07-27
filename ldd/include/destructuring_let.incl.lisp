;; -*- Mode: Lisp; Lowercase: True -*-

;; destructuring_let.incl.lisp - Loads lisp_destructuring_let_ into either
;; the compiler or interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature destructuring_let)
      (load (catenate (car (namelist (truename infile))) ">lisp_destructuring_let_"))))
