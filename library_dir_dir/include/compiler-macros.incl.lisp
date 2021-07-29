
;;; BEGIN INCLUDE FILE compiler-macros.incl.lisp

;;; This file contains useful macros used by the lisp_compiler.

;    (defun compiler-displace macro (l) 
;        ((lambda (a1 a2) (rplaca a1 (car a2)) (rplacd a1 (cdr a2)) (list 'quote a1))
;            (eval (cadr l))
;            (eval (caddr l))))
    (defun barf macro (x) 
	(displace x (list 'printmes (cadr x) (caddr x) (list 'quote (cadddr x))) ))

    (defun warn macro (x) (displace x (list 'printmes (cadr x) (caddr x) ''warn)))
    (defun specialp macro (x) (displace x  (list 'get (cadr x) '(quote special))))

    (defun memq-max macro (x) (displace x (list 'quote 11.)))	;   optimize (memq x '(...)) into
						;   an or of 10. or fewer clauses.

    (defun assq-max macro (x) (displace x (list 'quote  4)))	;max 3-dotted-pair inline assq
    (defun push macro (x) 
        (displace x (list 'setq (caddr x) (list 'cons (cadr x) (caddr x))))) 
    (defun pop macro (x) (displace x (list 'setq (cadr x) (list 'cdr (cadr x)))))
    (defun pnamep macro (x) (displace x (subst (cadr x) 'x '(eq (typep x) 'symbol))))


;;; END INCLUDE FILE compiler-macros.incl.lisp

