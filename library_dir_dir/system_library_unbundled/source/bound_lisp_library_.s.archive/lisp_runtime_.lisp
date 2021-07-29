;;; **************************************************************
;;; *                                                            *
;;; * Copyright, (C) Massachusetts Institute of Technology, 1982 *
;;; *                                                            *
;;; **************************************************************
;; -*- Mode: Lisp; Lowercase: True -*-

(%include sharpsign)
(%include defun)
(%include other_other)

;; Function cell manipulators

(defconst functional-properties-list 
	'(subr lsubr fsubr expr fexpr array macro autoload))

(defun fboundp (symbol)
  (if (getl symbol functional-properties-list) t))

(defun fmakunbound (symbol)
  (mapc (function (lambda (property) (remprop symbol property)))
        functional-properties-list)
  t)

(defun fsymeval (symbol)
  (let ((plist (getl symbol functional-properties-list)))
       (if (eq (car plist) 'macro)
	 (cons 'macro (cadr plist))
	 (cadr plist))))

(defun fset (symbol object)
  (fmakunbound symbol)
  (cond ((symbolp object)
         (putprop symbol object 'expr))         
        ((atom object)
         (putprop symbol object 'subr))
        ((eq (car object) 'macro)
         (putprop symbol (cdr object) 'macro))
        ((eq (car object) 'lambda)
         (putprop symbol object 'expr))
        (t (error "Random functional object - FSET"
	        object 'wrng-type-arg))))

;; Byte operations

(defun ldb (ppss x)
  (boole 1 (1- (lsh 1 (boole 1 77 ppss)))
         (lsh x (- 0 (boole 1 77 (lsh ppss -6))))))

(defun dpb (x ppss y)
  (let ((pp (boole 1 77 (lsh ppss -6)))
        (m (1- (lsh 1 (boole 1 77 ppss)))))
       (boole 7 (boole 2 (lsh m pp) y)
	    (lsh (boole 1 m x) pp))))

;; Lisp Machine list manipulation functions

(defun firstn (n list)
  (declare (fixnum n))
  (do ((old-list list (cdr old-list))
       (new-list nil (cons (car old-list) new-list))
       (count n (1- count)))
      ((zerop count) (nreverse new-list))))

(defun butlast (list)
  ;; (check-arg list (or (null list) (not (atom list))) "a list")
  (cond ((null list) nil)
        (t (do ((list list (cdr list))
              (new-list nil (cons (car list) new-list)))
             ((null (cdr list)) (nreverse new-list))))))

(defun nbutlast (list)
  ;; (check-arg list (or (null list) (not (atom list))) "a list")
  (cond ((null list) nil)
        (t (do ((list list (cdr list)))
             ((null (cddr list)) (rplacd list nil)))
         list)))

;; Thus must understand the ':initial-value keyword to be
;; compatible with the current Lispm definition.

(defun make-list (n &optional entry)
  (declare (fixnum n))
  (do ((list nil (push entry list)))
      ((= n 0) list)
      (setq n (1- n))))

;; MEM works like MEMQ and MEMBER except that it can take an arbitrary
;; comparison predicate, i.e. (MEM 'EQ 3 LIST) = (MEMQ 3 LIST).

(defun mem (predicate element list)
  ;; (check-arg list (or (null list) (not (atom list))) "a list")
  (do ((list list (cdr list)))
      ((null list) nil)
      (if (funcall predicate element (car list)) (return t))))

;; FIND-POSITION-IN-LIST looks down LIST for an element which is eq to OBJECT,
;; like MEMQ.  It reutrns the numeric index in the list at which it found the
;; first occurrence of OBJECT, or nil if it did not find it at all.
;; (find-position-in-list 'a '(a b c)) --> 0
;; (find-position-in-list 'e '(a b c)) --> nil

(defun find-position-in-list (object list)
  ;; (check-arg list (or (null list) (not (atom list))) "a list")
  (do ((l list (cdr l))
       (i 0 (1+ i)))
      ((null l) nil)
      (declare (fixnum i))
      (if (eq object (car l)) (return i))))

;; Generalized ASSOC -- first argument is a comparison predicate which
;; is used instead of EQUAL.

(defun ass (predicate item alist)
  ;; (check-arg alist (or (null alist) (not (atom alist)))
  ;;            "an association list")
  (dolist (pair alist)
          (if (funcall predicate item (car pair)) (return pair))))

;; Reverse ASSQ -- like ASSQ but tries to find an element of the alist whose
;; cdr (not car) is EQ to the object.

(defun rassq (item alist)
  ;; (check-arg alist (or (null alist) (not (atom alist)))
  ;;            "an association list")
  (dolist (pair alist)
          (if (eq item (cdr pair)) (return pair))))

;; Reverse ASSOC -- like ASSOC but tries to find an element of the alist
;; whose cdr (not car) is EQUAL to the object.

(defun rassoc (item alist)
  ;; (check-arg alist (or (null alist) (not (atom alist)))
  ;;          "an association list")
  (dolist (pair alist)
          (if (equal item (cdr pair)) (return pair))))

;; REM, REMQ, REMOVE are non-destructive versions of DEL, DELQ, DELETE.
;; These algorithms could be made more efficient by sharing the tail of the
;; returned list with the original.

(defun rem (predicate item list &optional (count -1))
  (do ((l list (cdr l))
       (result nil))
      ((null l) (nreverse result))
      (if (or (not (funcall predicate item (car l))) (zerop count))
	(push (car l) result)
	(decf count))))

(defun remq (item list &optional (count -1))
  (do ((l list (cdr l))
       (result nil))
      ((null l) (nreverse result))
      (if (or (neq item (car l)) (zerop count))
	(push (car l) result)
	(decf count))))

(defun remove (item list &optional (count -1))
  (do ((l list (cdr l))
       (result nil))
      ((null l) (nreverse result))
      (if (or (nequal item (car l)) (zerop count))
	(push (car l) result)
	(decf count))))

;; This algorithm works in two steps.  First cdr down the list to find the
;; cons we are going to return or until we reach the end of the list.
;; When we find the cons to return, cdr down from there splicing out
;; appropriate cons cells.

(defun del (predicate item list &optional (count -1))
  (do ()
      ((null list) nil)
      (cond ((and (not (zerop count)) (funcall predicate item (car list)))
	   (pop list)
	   (decf count))
	  (t (return nil))))
  (do ((first list)
       (second (cdr list)))
      ((null second))
      (cond ((and (not (zerop count)) (funcall predicate item (car second)))
	   (rplacd first (cdr second))
	   (decf count))
	  (t (pop first)))
      (pop second))
  list)

;; (circular-list-last (circular-list 1 2 3)) --> (3 1 2 3 1 2 ...)
;; Useful for manipulating kill-rings implemented as circular lists.
;; Provides the inverse operation of cdr.

(defun circular-list-last (list)
  (do ((previous list (cdr previous))
       (next (cdr list) (cdr next)))
      ((or (null next) (eq next list))
       previous)))

;; For PDP10 MacLisp compatibility.  Try to use something else if you can.

(defun symbolconc (&rest rest)
  (make_atom (apply (function catenate) rest)))

;; Note: bignum-ash and fixnum-ash are not standard MacLisp or ZetaLisp
;; functions.  Bignum-ash is compatible with the ZetaLisp.  Fixnum-ash is
;; compatible with PDP10 MacLisp.  (defprop ash fixnum-ash expr) if you want
;; ash to mean the fixnum definition rather than the bignum definition.

(defun bignum-ash (x b)
  (if (minusp b)
      (if (minusp x)
	(sub1 (quotient (add1 x) (expt 2 (minus b))))
	(quotient x (expt 2 (minus b))))
      (times x (expt 2 b))))

(defun fixnum-ash (x b)
  (declare (fixnum x b))
  (if (minusp b)
      (if (minusp x)
	(logxor -1 (lsh (logxor -1 x) b))
	(lsh x b))
      (logior (logand #.(rot 1 -1) x)
	    (logand #.(lsh -1 -1) (lsh x b)))))

(defprop ash bignum-ash expr)
