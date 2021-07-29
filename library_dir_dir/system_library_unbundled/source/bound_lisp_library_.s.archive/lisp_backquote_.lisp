;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Lisp -*-

;;;BACKQUOTE:
;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a              ;the NIL flag is used only when a is NIL
;;;      T: [a] => a              ;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a)
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;    \ car   ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|      |
;;;  cdr \     ||                 |    T or NIL     |                |                |
;;;====================================================================================
;;;    |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d]) |
;;;    NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a    |
;;; QUOTE or T || LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC  (a [d]) |
;;;   APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC  (a [d]) |
;;;   NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a . d) |
;;;    LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC  (a [d]) |
;;;    LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d]) |
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead of ",@a)"

(setsyntax '/` 'macro 'xr-backquote-macro)
(setsyntax '/, 'macro 'xr-comma-macro)

(declare (special **backquote-flag**
	        **backquote-count**
	        **backquote-/,-flag**
	        **backquote-/,/@-flag**
	        **backquote-/,/.-flag**))

(setq **backquote-flag**)
(setq **backquote-count** 0)
(setq **backquote-/,-flag** (copysymbol '|`,| nil))
(setq **backquote-/,/@-flag** (copysymbol '|`,@| nil))
(setq **backquote-/,/.-flag** (copysymbol '|`,.| nil))

(defun xr-backquote-macro ()
       ((lambda (**backquote-count** **backquote-flag** thing)
                (setq thing (backquotify (read)))
                (cond ((eq **backquote-flag** **backquote-/,/@-flag**)
                       (error " "",@"" right after a ""`"" : " thing 'fail-act))
                      ((eq **backquote-flag** **backquote-/,/.-flag**)
                       (error " "",."" right after a ""`"" : " thing 'fail-act))
                      (t
                       (backquotify-1 **backquote-flag** thing))))
        (1+ **backquote-count**)
        nil
        nil))

(defun xr-comma-macro ()
       (or (> **backquote-count** 0)
           (error "Comma not inside a backquote. " nil 'fail-act))
       ((lambda (c **backquote-count**)
                (cond ((= c 100)                        ;#/@
                       (tyi)
                       (cons **backquote-/,/@-flag** (read)))
                      ((= c 56)                         ;#/.
                       (tyi)
                       (cons **backquote-/,/.-flag** (read)))
                      (t (cons **backquote-/,-flag** (read)))))
        (tyipeek)
        (1- **backquote-count**)))

(defun backquotify (code)
       (prog (aflag a dflag d)
             (cond ((atom code)
                    (cond ((null code)
                           (setq **backquote-flag** nil)
                           (return nil))
                          ((or (numberp code)
                               (eq code t))
                           (setq **backquote-flag** t)
                           (return code))
                          (t (setq **backquote-flag** 'quote)
                             (return code))))
                   ((eq (car code) **backquote-/,-flag**)
                    (setq code (cdr code))
                    (go comma))
                   ((eq (car code) **backquote-/,/@-flag**)
                    (setq **backquote-flag** **backquote-/,/@-flag**)
                    (return (cdr code)))
                   ((eq (car code) **backquote-/,/.-flag**)
                    (setq **backquote-flag** **backquote-/,/.-flag**)
                    (return (cdr code))))
             (setq a (backquotify (car code)))
             (setq aflag **backquote-flag**)
             (setq d (backquotify (cdr code)))
             (setq dflag **backquote-flag**)
             (and (eq dflag **backquote-/,/@-flag**)
                  (error " "",@"" after a ""."" : " code 'fail-act))
             (and (eq dflag **backquote-/,/.-flag**)
                  (error " "",."" after a ""."" : " code 'fail-act))
             (cond ((eq aflag **backquote-/,/@-flag**)
                    (cond ((null dflag)
                           (setq code a)
                           (go comma)))
                    (setq **backquote-flag** 'append)
                    (return (cond ((eq dflag 'append)
                                   (cons a d))
                                  (t (list a (backquotify-1 dflag d))))))
                   ((eq aflag **backquote-/,/.-flag**)
                    (cond ((null dflag)
                           (setq code a)
                           (go comma)))
                    (setq **backquote-flag** 'nconc)
                    (return (cond ((eq dflag 'nconc)
                                   (cons a d))
                                  (t (list a (backquotify-1 dflag d))))))
                   ((null dflag)
                    (cond ((memq aflag '(quote t nil))
                           (setq **backquote-flag** 'quote)
                           (return (list a)))
                          (t (setq **backquote-flag** 'list)
                             (return (list (backquotify-1 aflag a))))))
                   ((memq dflag '(quote t))
                    (cond ((memq aflag '(quote t nil))
                           (setq **backquote-flag** 'quote)
                           (return (cons a d)))
                          (t (setq **backquote-flag** 'list*)
                             (return (list (backquotify-1 aflag a)
                                           (backquotify-1 dflag d)))))))
             (setq a (backquotify-1 aflag a))
             (and (memq dflag '(list list*))
                  (setq **backquote-flag** dflag)
                  (return (cons a d)))
             (setq **backquote-flag** 'list*)
             (return (list a (backquotify-1 dflag d)))
       comma (cond ((atom code)
                    (cond ((null code)
                           (setq **backquote-flag** nil)
                           (return nil))
                          ((or (numberp code)
                               (eq code 't))
                           (setq **backquote-flag** t)
                           (return code))
                          (t (setq **backquote-flag**
                                   **backquote-/,-flag**)
                             (return code))))
	         ((eq (car code) 'quote)
		(setq **backquote-flag** 'quote)
		(return (cadr code)))
                   ((memq (car code) '(append list list* nconc))
                    (setq **backquote-flag** (car code))
                    (return (cdr code)))
                   ((eq (car code) 'cons)
                    (setq **backquote-flag** 'list*)
                    (return (cdr code)))
                   (t (setq **backquote-flag** **backquote-/,-flag**)
                      (return code)))))

(defun backquotify-1 (flag thing)
       (cond ((or (eq flag **backquote-/,-flag**)
                  (memq flag '(t nil)))
              thing)
             ((eq flag 'quote)
              (list 'quote thing))
             ((eq flag 'list*)
              (cond ((null (cddr thing))
                     (cons 'cons thing))
                    (t (cons 'list* thing))))
             (t (cons flag thing))))

(sstatus feature backquote)
