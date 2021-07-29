;;;
;;; backquote.incl.lisp - BSG 10/9/79
;;; Loads lisp_backquote_ into either the compiler or interpreter
;;; environment.
;;;
;;; Modified 10/30/82 by Richard Lamson to use eval-when and
;;;				   (status feature backquote)
;;;
(eval-when (eval compile)
  (or (status feature backquote)
      (load (catenate (car (namelist (truename infile))) ">lisp_backquote_"))))
