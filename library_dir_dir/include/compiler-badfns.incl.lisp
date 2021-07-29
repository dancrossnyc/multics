
;;; BEGIN INCLUDE FILE compiler-badfns.incl.lisp

;;; This defines the badfns macro, which expands into a list
;;; of the system functions which are not compiled in line
;;; and (but?) which can have bad side-effects (change special variables)

;;; extracted from main code of compiler and made correct 8 May 1973 DAM.


(defun macro badfns (x) (displace x (list 'quote '(
	*rset	;in case we change this to use a special variable.
	apply 	break	cline	;you never know
	close	eval	includef  inpush	ioc	iog
	makunbound	read	readch	readstring
	status	sstatus	tyi
	tyipeek	ufile	uread	uwrite	ukill
))))

;;; END INCLUDE FILE compiler-badfns.incl.lisp

