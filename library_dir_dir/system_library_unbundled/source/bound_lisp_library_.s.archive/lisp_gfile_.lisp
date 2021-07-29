;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************

;;;   ***********************************************************************
;;;   ***** Maclisp ****** S-expression formatter for files (grind) *********
;;;   ***********************************************************************
;;;   ** (c) Copyright 1974 Massachusetts Institute of Technology ***********
;;;   ****** this is a read-only file! (all writes reserved) ****************
;;;   ***********************************************************************

;;;This version of Grind works in both ITS Maclisp and Multics Maclisp
;;; copied from (mc gfile 384).

;;;gfile - fns for pretty-printing and grinding files
;;;about 700. instructions when ncomplr'd


(declare (array* (notype (gtab/| 128.)))
         (noargs t)
         (special merge readtable grindreadtable remsemi ~r
                  grindpredict grindproperties grindef predict
                  grindfn grindmacro programspace topwidth
                  grindlinct global-lincnt /; /;/; user-paging form prog? n m l h
                  arg linel pagewidth gap comspace fill nomerge comnt
                  /;/;? ^d macro unbnd-vrbl cnvrgrindflag)
         (*expr form topwidth programspace pagewidth comspace
                nomerge remsemi grchrct indent-to page panmax sprint1
	      turpri)
         (*fexpr trace slashify unslashify grindfn grindmacro
                 unreadmacro readmacro grindef)
         (*lexpr merge predict user-paging fill testl)
         (mapex t)
         (genprefix gr+)
         (fixnum nn
                 mm
                 (grchrct) (newlinel fixnum)
                 (prog-predict notype fixnum fixnum)
                 (block-predict notype fixnum fixnum)
                 (setq-predict notype fixnum fixnum)
                 (panmax notype fixnum fixnum)
                 (maxpan notype fixnum)
                 (gflatsize)))

(defun macex macro (x) (list 'defun (cadr x) 'macro (caddr x) (eval (cadddr x))))

(defun ifoio macro (x) 
       (cond ((not (status feature newio)) (cadr x)) ('(comment ifoio not taken))))

(defun ifnio macro (x)
       (cond ((status feature newio) (cadr x)) ('(comment ifnio not taken))))

(macex newlineseq (x)
    (cond ((status feature Multics)
	 ''(list (ascii 12)))
	(t ''(list (ascii 15)(ascii 12)))))

(macex version (x) 
        (subst (maknam 
                  (nconc (newlineseq)
                         (explodec '/;loading/ grind/ )
                         (explodec (cond ((status feature newio) (caddr (names infile)))
                                         ((cadr (status uread)))))
		     (newlineseq)))
                 'version 
                 ''(iog nil (princ 'version) (ascii 0))))

(ifnio (defun newlinel macro (x) (subst (cadr x) 'nn '(setq linel nn))))
(ifoio (defun grchrct macro (x) 'chrct))
(ifnio (defun macro set-linel (x) '(setq linel (linel (and outfiles (car outfiles))))))
(ifoio (defun macro set-linel (x) '(comment linel)))

(macex prin50com-chrct-fiddle (x) 
       (cond ((status feature newio) ''(comment))
	     (''(setq chrct (- chrct (- pagewidth linel))))))


(version)

;;*user-paging


(prog nil 	;some initializations

(and (not (boundp 'grind-use-original-readtable))       ;grind-use-original-readtable = nil =>
							;use current readtable: 
     (setq grind-use-original-readtable t))		;otherwise use original readtable (default).


(and (or (not (boundp 'grindreadtable)) (null grindreadtable))
     ((lambda (readtable) (setsyntax 12. 'single nil)                  ;^l made noticeable.
                          (sstatus terpri t)                           ;the grindreadtable is tailored for
                          (setsyntax '/;                               ;grind. no cr
                                     'splicing
                                     'semi-comment))                   ;are inserted by lisp when print exceeds
      (setq grindreadtable (*array nil 'readtable grind-use-original-readtable)))) 

(setq macro '/;  /; (copysymbol '/; nil) /;/; (copysymbol '/;/; nil))
(setq grindlinct 8. global-lincnt 59. comnt nil /;/;? nil)
(sstatus feature grind)) 



;;;Multics versions of grind and grind0
(ifnio (defun grind fexpr (file)                ;grinds and files file.
	  ((lambda (x)
	    (cond ((stringp (car file)))        ;already filed.
	          ((apply 'ufile x)))
	    file)
	   (apply 'grind0 file))))

(ifnio (defun grind0 fexpr (file)               ;grinds file and returns file
;	      (or (status feature grindef)
	      (or (get 'sprinter 'subr)	;do it this way for now due to bug? in status feature
                        ;load other part of grind if necessary and prior to any use of grind
                    (load (get 'sprinter 'autoload)))                             ;global variables like programspace.
       (prog (remsemi linel *nopoint readtable base l ^q ^r ^w ^d outfiles
              eof n /;/;? comnt) 
             (setq base 10. linel programspace)
             (setq readtable grindreadtable remsemi t)
             (cond ((stringp (car file))
                    (inpush (openi (car file)))
                    (setq outfiles
                         (list
                          (openo
                           (mergef
                            (cond ((null (cdr file))
                                   (princ '/
Filing/ as/ /!grind/.output/
 nil)
                                   '(* !grind output))
                                  ((cadr file)))
                            (cons (car (names nil)) '*)
                            (names infile))))))
                   (t (apply 'uread file)
                      (uwrite)))

             (setq eof (list nil) n topwidth)
             (setq ^q t ^r t ^w t grindlinct global-lincnt)
        read (and (= (tyipeek 47791616.) 59.)                           ;catch top-level splicing macro
                  (readch)
                  (cond ((eq (car (setq l (car (semi-comment)))) /;)
                         (rem/;)
                         (go read))
                        (t (go read1))))
             (and (null ^q) (setq l eof) (go read1))                    ;catch eof in tyipeek
             (and (eq (car (setq l (read eof))) /;)                    ;store /; strings of /; comments.
                  (rem/;)
                  (go read))
        read1(prinallcmnt)                                             ;print stored /; comments
             (or (eq eof l) (go process))
        exit (terpri)
             (ioc t)
             (and (stringp (car file)) (close (car outfiles)))         ;won't get ufile'd
             (return file)
        process
             (cond ((eq l (ascii 12.))                                 ;formfeed read in ppage mode
                    (or user-paging (go read))                         ;ignore ^l in user-paging mode.
                    (and (equal (tyipeek t) 3.) (go exit))             ;do not formfeed if at eof
                                                                       ;BUG:  can lose semicolon comments
                    (terpri)
                    (page)
                    (setq /;/;? t)
                    (go read))
                   ((eq (car l) /;/;)                                  ;toplevel ;;... comment
                    (newlinel topwidth)
                    (or /;/;? (= linel (grchrct)) (turpri) (turpri))                ;produces  blank line preceding new
                    (rem/;/;)                                          ;block of /;/; comments. (turpri is
                    (newlinel programspace)                            ;already in rem/;/;).  a total of 3
                    (go read)))                                        ;turpri's are necessary if initially
             (fillarray 'gtab/| '(nil))                                    ;chrct is not linel, ie we have just
             (cond (user-paging (turpri) (turpri))                     ;finished a line and have not yet cr.
                   ((< (turpri) (catch (\ (panmax l (grchrct) 0.) 60.)))   ;clear hash array
                    (page))
                   ((turpri)))
             (cond ((sprint1 l linel 0.) (prin1 l)))
             ;;(tyo 32.)                                                ;prevents toplevel atoms from being
             (go read))))                                               ;accidentally merged by being separated
                                                                       ;only by cr.



;;;ITS versions of grind and grind0

(ifoio (defun grind fexpr (file) (apply 'ufile (apply 'grind0 file))))

(ifoio (defun grind0 fexpr (file)
    ((lambda (crunit ~r)                             ;read in remainder of grind package if
       (or (status feature grindef)            ;necessary and prior to any use of grind
           (and (cond ((status feature dec10)        ;global variables like programspace.
                       (fasload gfn fsl sys))     ;read from sys: device on other pdp-10's.
                      (t (fasload gfn fasl com)))  ;read from com: device on ITS.
             (apply 'crunit crunit))))
       (crunit) nil)
       (prog (remsemi linel *nopoint readtable base l ^q ^r ^w ^d outfiles
              eof n /;/;? comnt) 
             (setq base 10. linel programspace)
             (setq readtable grindreadtable remsemi t)
             (apply 'uread file)
             (uwrite)
             (setq eof (list nil) n topwidth)
             (setq ^q t ^r t ^w t grindlinct global-lincnt)
        read (and (= (tyipeek 47791616.) 59.)                           ;catch top-level splicing macro
                  (readch)
                  (cond ((eq (car (setq l (car (semi-comment)))) /;)
                         (rem/;)
                         (go read))
                        (t (go read1))))
             (and (null ^q) (setq l eof) (go read1))                    ;catch eof in tyipeek
             (and (eq (car (setq l (read eof))) /;)                    ;store /; strings of /; comments.
                  (rem/;)
                  (go read))
        read1(prinallcmnt)                                             ;print stored /; comments
             (or (eq eof l) (go process))
        exit (terpri)
             (ioc t)
             (return file)
        process
             (cond ((eq l (ascii 12.))                                 ;formfeed read in ppage mode
                    (or user-paging (go read))                         ;ignore ^l in user-paging mode.
                    (and (equal (tyipeek t) 3.) (go exit))             ;do not formfeed if at eof
                    (terpri)
                    (page)
                    (setq /;/;? t)
                    (go read))
                   ((eq (car l) /;/;)                                  ;toplevel ;;... comment
                    (newlinel topwidth)
                    (or /;/;? (= linel (grchrct)) (turpri) (turpri))                ;produces  blank line preceding new
                    (rem/;/;)                                          ;block of /;/; comments. (turpri is
                    (newlinel programspace)                            ;already in rem/;/;).  a total of 3
                    (go read)))                                        ;turpri's are necessary if initially
             (fillarray 'gtab/| '(nil))                                    ;chrct is not linel, ie we have just
             (cond (user-paging (turpri) (turpri))                     ;finished a line and have not yet cr.
                   ((< (turpri) (catch (\ (panmax l (grchrct) 0.) 60.)))   ;clear hash array
                    (page))
                   ((turpri)))
             (cond ((sprint1 l linel 0.) (prin1 l)))
             (tyo 32.)                                                ;prevents toplevel atoms from being
             (go read))))                                               ;accidentally merged by being separated
                                                                       ;only by cr.

;;prediction

(putprop /; 0. 'grindpredict) 

(putprop /;/; 1. 'grindpredict) 


;;semi-colon comments

(defun rem/; nil 
       (prog (c retval) 
        a    (cond ((atom l) (return retval))
                   ((eq (car l) /;)
                    (setq c (cdr l))
                    (setq retval 'car)
                    (setq l nil))
                   ((and (null (atom (car l))) (eq (caar l) /;))
                    (setq c (cdar l))
                    (setq retval 'caar)
                    (setq l (cdr l)))
                   (t (cond ((and (eq retval 'caar)                    ;look ahead to separate comments.
                                  (cdr l)
                                  (null (atom (cdr l)))
                                  (null (atom (cadr l)))
                                  (eq (caadr l) /;))
                             (prinallcmnt)
                             (indent-to n)))
                      (return retval)))
        b    (cond ((null comnt) (setq comnt c))
                   ((< comspace (length comnt)) (turpri) (go b))
                   ((nconc comnt (cons '/  c))))
             (go a))) 


(defun rem/;/; nil 
       (prog (c retval) 
        a    (cond ((atom l)
                    (and (eq retval 'caar) (indent-to n))
                    (return retval))
                   ((eq (car l) /;/;)
                    (setq c (cdr l))
                    (setq retval 'car)
                    (setq l nil))
                   ((and (null (atom (car l))) (eq (caar l) /;/;))
                    (setq c (cdar l))
                    (setq retval 'caar)
                    (setq l (cdr l)))
                   (t (and (eq retval 'caar) (indent-to n))            ;restore indentation for upcoming code
                      (return retval)))
             (prinallcmnt)
             (and (null /;/;?) (turpri))
             (prog (comnt pagewidth comspace macro) 
                   (setq comnt c)
                   (and (or (memq (car c) '(/; *))
                            (null merge))                              ;nomerge.  update pagewidth, comspace
                        (setq /;/;? '/;/;/;)                           ;appropriate for a total line of
                        (setq pagewidth topwidth                       ;topwidth
                              comspace (+ n (- topwidth linel)))
                        (go prinall))
                   (setq pagewidth linel)
                   (cond ((eq /;/;? /;/;)                              ;preceding comnt.  merge.
                          (setq comnt (cons '/  comnt))
                          (setq macro (ascii 0.))
                          (setq comspace (grchrct))
                          (prin50com))
                         ((setq /;/;? /;/;)))
                   (setq comspace n)
              prinall
                   (setq macro /;/;)
                   (prinallcmnt))
             (tj6 c)
             (go a))) 

(defun tj6 (x)                                                         ;tj6 commands: ;;*--- or ;;*(...) (...)
       (and
        (eq (car x) '*)
        (setq x (cdr x))
        (turpri)
        (cond
         ((errset
           (cond ((atom (car (setq x
                                   (readlist (cons '/(
                                                   (nconc x
                                                          '(/))))))))
                  (eval x))
                 ((mapc 'eval x)))))
         ((error '/;/;*/ error x 11.))))) 



(defun prin50com nil                                                   ;prints one line of ; comment
       (do ((next) (linel linel)) ()              ;prog binding linel to linel instead of nil.
             (newlinel pagewidth)
             (prog (comnt) (indent-to comspace))
             (princ macro)
        pl   (cond ((null comnt) (return nil))
                   ((eq (car comnt) '/ )
                    (setq comnt (cdr comnt))
                    (setq next                                         ;number of characters till next space.
                          (do ((x comnt (cdr x)) (num 2. (1+ num)))
                              ((or (null x) (eq (car x) '/ ))
                               num)))
                    (cond ((and (or (eq macro /;) (eq /;/;? /;/;))
                                fill
                                (= next 2.)
                                (go pl)))
                          ((and (not (eq macro (ascii 0.)))
                                (> next comspace)))
                          ((< (grchrct) next)
                           (return nil)))
                    (tyo 32.)
                    (go pl))
                   ((> (grchrct) 0.)
                    (princ (car comnt))
                    (and (or (eq macro /;) (eq /;/;? /;/;))
                         fill
                         (eq (car comnt) '/.)
                         (eq (cadr comnt) '/ )
                         (tyo 32.)))
                   (t (return nil)))
             (setq comnt (cdr comnt))
             (go pl))
       (prin50com-chrct-fiddle)                   ;may restore chrct to be negative
          )

(defun prinallcmnt nil (cond (comnt (prin50com) (prinallcmnt))))       ;prints \ of ; comment

(defun semi-comment nil                                                ;converts ; and ;; comments to exploded
       (prog (com last char)                                           ;lists
             (setq com (cons /; nil) last com)
             (setq char (readch))                                      ;decide type of semi comment
             (cond ((eq char '/
) (return (list com)))
		   ((eq char '/;) (rplaca last /;/;))
                   ((rplacd last (cons char nil))
                    (setq last (cdr last))))
        a    (setq char (readch))
             (cond ((eq char '/
) (return (list com)))
		   ((rplacd last (cons char nil))
                    (setq last (cdr last))
                    (go a))))) 

;;conniver macros

(defun grindcolmac nil (list ': (read))) 

(defun grindcommac nil (list '/, (read))) 

(defun grindatmac nil (cons '@ (read))) 

(defun grindexmac nil 
       (prog (c f) 
             (setq c (grindnxtchr))
             (cond ((setq f (assq c '((/" !/") (@ !@) ($ !$))))
                    (tyi)
                    (return (cons (cadr f) (read))))
                   ((setq f (assq c
                                  '((? !?) (/' !/') (> !>) (/, !/,)
                                    (< !<) (/; !/;))))
                    (tyi)
                    (setq f (cadr f)))
                   (t (ioc v)
                      (print (list 'bad
                                   '!
                                   'macro
                                   c))
                      (ioc g)))
             (return (cond ((grindseparator (grindnxtchr))
                            (list f nil))
                           ((atom (setq c (read))) (list f c))
                           (t (cons f c)))))) 

(defun grindnxtchr nil (ascii (tyipeek))) 

(defun grindseparator (char) (memq char '(/  /   /)))) 
