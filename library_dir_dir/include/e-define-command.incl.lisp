;;; BEGIN INCLUDE FILE e-define-command.incl.lisp

;;;	Include File to Load Emacs Command Definition Macro

;;; HISTORY COMMENTS:
;;;  1) change(79-08-27,Palter), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     Initial version.
;;;  2) change(82-10-09,Margolin), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     to look in the wdir first
;;;  3) change(85-01-06,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to check and set (status feature e-defcom).
;;;                                                      END HISTORY COMMENTS

;;;

(eval-when (compile eval)
(or (status feature e-defcom)
    (errset (load "e_define_command_") nil)	;don't print error
    (load (catenate (car (namelist infile)) ">e_define_command_"))
    )
(sstatus feature e-defcom)
)

;;; END INCLUDE FILE e-define-command.incl.lisp
