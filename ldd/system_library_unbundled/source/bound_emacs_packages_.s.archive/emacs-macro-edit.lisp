;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************
;;;
;;;
;;;	Emacs keyboard macro editor
;;;	February 16-18, 1979 by BSG
;;; Modified: 30 January 1984, Barmar: to fix parsing of ESC <+/->NUM.
;;;


(%include e-macros)
(declare (defpl1 date_time_af_ "date_time" (return char (26.) varying)))
(declare (special fill-column macedit-whats-escape-today-vbl comment-prefix
	        include-dir comment-column macedit-source-buffer))
(declare (*expr begin-defun find-file-subr find-key-in-buf get-key-binding
	      get-key-name key-prompt kill-line-contents
	      kmacro-display-interpret lisp-mode macomp-compile-to-expr
	      macomp-output-to-buffer one-back-is-a parse-key-description))

(defun macedit-find-all-macros ()
       (let ((l nil))
	  (mapatoms '(lambda (x)
			 (let ((y (get x 'editor-macro)))
			      (if y (setq l (cons (cons x y) l))))))
	  l))				;Return the gotten list

(defun macedit-display-all-macros-to-buffer ()
       (mapc '(lambda (mac)
		  (macedit-display-to-buffer
		    (car mac)(cdr mac)
		    (find-key-in-buf (car mac) macedit-source-buffer))
		  (new-line)
		  (new-line))
	   (macedit-find-all-macros)))

(defun macedit-display-to-buffer (fun list key)
       (insert-string (catenate "macro " fun))
       (if key (insert-string (catenate " on " key)))
       (new-line)
       (insert-string "    ")
       (mapc 'macedit-display-one-enmacroed-command-to-buffer
	   (save-excursion-buffer
	     (go-to-buffer macedit-source-buffer)
	     (kmacro-display-interpret list)))
       (if (line-is-blank)(without-saving (kill-line-contents))
	 else (new-line))
       (insert-string (catenate "end-macro " fun)))

(defun macedit-display-one-enmacroed-command-to-buffer (comcons)
       (let ((key (car comcons))
	   (fun (cdr comcons)))
	  (if (eq fun 'Input/ Characters)	;doublequote input chars
	      (setq key (apply 'catenate
		        (append '("." """")
			      (mapcar
			        '(lambda (x)
				       (cond ((= x (CtoI """"))
					    """""")
					   (t (ItoC x))))
			        (exploden key))
			      '("""")))))
	  (if (> (+ 1 (stringlength key) (cur-hpos)) comment-column)
	      (new-line)
	      (insert-string "     ")
	      else (insert-string " "))
	  (insert-string key)))

(defun macedit-find-beginning-of-macdef ()
       (go-to-beginning-of-line)
       (do-forever
         (if (looking-at "macro")(stop-doing))
         (if (firstlinep)(display-error "No macro definition found"))
         (prev-line)))

(defun macedit-scan-atom ()
       (macedit-skip-over-whitespace)
       (cond ((at-end-of-buffer) nil)
	   ((looking-at ".""")
	    (forward-char)
	    (cons 'input-chars (macedit-scan-atom)))
	   ((at '/" )(macedit-scan-quoted-string))
	   ((at '+) (forward-char)
		  (macedit-scan-number))
	   ((at '-) (forward-char)
		  (- (macedit-scan-number)))
	   ((macedit-digitp (curchar))
	    (macedit-scan-number))
	   (t (with-mark begin
		       (skip-to-whitespace)
		       (intern (make_atom (point-mark-to-string begin)))))))

(defun macedit-digitp (x)
       (or (numberp x)(setq x (CtoI x)))
       (and (> x (1- (CtoI "0")))(< x (1- (CtoI "9")))(- x (CtoI "0"))))

(defun macedit-scan-number ()
       (cond ((and (not (at-end-of-buffer))
	         (macedit-digitp (curchar)))
	    (do ((acc 0)
	         (dp (macedit-digitp (curchar))
		   (and (not (at-end-of-buffer))
		        (macedit-digitp (curchar)))))
	        ((null dp) acc)
	        (setq acc (+ (* 10. acc) dp))
	        (forward-char)))
	   (t 1)))			;nothing, defaults to 1

(defun macedit-scan-quoted-string ()
       (do ((s ""))(nil)
         (forward-char)
         (with-mark bos
		(if (forward-search """")
		    (if-at '/"
			 (backward-char)
			 (setq s (catenate s (point-mark-to-string bos) """" ))
			 (forward-char)
			 else
			 (backward-char)
			 (setq s (catenate s (point-mark-to-string  bos)))
			 (forward-char)
			 (release-mark bos)
			 (return s))
		    else
		    (go-to-mark bos)
		    (release-mark bos)
		    (display-error "Unbalanced string")))))

(defun macedit-skip-over-whitespace ()
       (do-forever
         (skip-over-whitespace)
         (if (not (looking-at "/*"))(stop-doing))
         (do-times 2 (forward-char))
         (if (not (forward-search "*/"))
	   (display-error "Unbalanced comment."))))

(defun macedit-produce-macro-definition ()
       (prog (macname keyname mlist)
	   (macedit-find-beginning-of-macdef)
	   (or (eq (macedit-scan-atom) 'macro)
	       (return '(nil . "Mangled macro definition")))
	   (setq macname (macedit-scan-atom))
	   (if (memq macname '(nil end-macro on))
	       (return '(nil . "Bad or empty macro definition")))
	   (macedit-skip-over-whitespace)
	   (if (looking-at "on")		;Key given
	       (macedit-scan-atom)
	       (setq keyname (macedit-scan-atom)))
	   (do ((x nil (nconc (macedit-scan-commands) x)))
	       ((memq (car x) '(macend error))
	        (setq mlist x)))
	   (if (eq (car mlist) 'error)(return (cadr mlist)))
	   (if (not (eq macname (macedit-scan-atom)))
	       (return '(nil . "Macro end does not match beginning")))
	   (return (list macname keyname (nreverse mlist)))))


(defun macedit-scan-commands ()
       (if (or (not (boundp 'macedit-whats-escape-today-vbl))
	     (null macedit-whats-escape-today-vbl))
	 (setq macedit-whats-escape-today-vbl
	       (cadr (parse-key-description
		     (find-key-in-buf 'escape macedit-source-buffer)))))
					;Feelthy magic.
       (let ((atom (macedit-scan-atom)))
	  (cond ((eq atom nil)(list 'error "Macro ran off end."))
	        ((eq atom 'end-macro)(list 'macend))
	        ((numberp atom)(nreverse (exploden (decimal-rep atom))))
	        ((symbolp atom)
	         (if (and (> (stringlength atom) 5)
		        (samepnamep (substr atom 1 5) "meta-"))
		   (+ 200 (cadr (parse-key-description (substr atom 6))))
		   else
		   (setq atom (parse-key-description atom))
		   (cond ((= (car atom) 1)	;escape char
			(list (cadr atom)
			      (cons 'toplevel-char macedit-whats-escape-today-vbl)))
		         ((caddr atom)	;prefix char
			(list (cadr atom)(cons 'toplevel-char (caddr atom))))
		         (t (list (cons 'toplevel-char (cadr atom))))))) ;no pfx, no esc
	        ((stringp atom)
	         (mapcar
		 '(lambda (x)(cons 'toplevel-char x))
		 (nreverse (exploden atom))))
	        ((and (not (atom atom))(eq (car atom) 'input-chars))
	         (nreverse (exploden (cdr atom))))
	        (t (break macedit-scan-commands t)))))

(defprop emacro macro-edit-mode suffix-mode)
(defun macro-edit-mode ()
       (setq current-buffer-mode 'Macro/ Edit)
       (establish-local-var 'macedit-source-buffer current-buffer)
       (mapc '(lambda (x)(set-key (car x)(cadr x)))
	   '((ESC-^A	macedit-find-beginning-of-macdef)
	     (ESC-^B	macedit-backward-term)
	     (ESC-^C	macedit-compile-to-lisp)
	     (ESC-^E	macedit-find-end-of-macdef)
	     (ESC-^F	macedit-forward-term)
	     (ESC-^H	macedit-mark-whole-macro)
	     (ESC-^K	macedit-kill-term)
	     (ESC-^N	macedit-forward-macdef)
	     (ESC-^P	macedit-backward-macdef)
	     (ESC-^S	macedit-state-keyboard-macro)
	     (ESC-^Z	macedit-take-up-definition)))
       (setq comment-prefix "/*" comment-column 51.))

	   
	 
(defun macedit-state-keyboard-macro ()
       (let ((k (key-prompt "Macro Key: ")))
	  (let ((f (save-excursion-buffer
		   (go-to-buffer macedit-source-buffer)
		   (get-key-binding k))))
	       (let ((l (get f 'editor-macro)))
		  (if (null l)
		      (display-error " " (get-key-name k)
				 " is not a macro."))
		  (go-to-end-of-buffer)
		  (macedit-display-to-buffer f l (get-key-name k))
		  (new-line)))))


(defun macedit-take-up-definition ()
       (macedit-find-beginning-of-macdef)
       (let ((mac (macedit-produce-macro-definition)))
	  (if (car mac)
	      (putprop (car mac)(caddr mac) 'editor-macro)
	      (if (cadr mac)(set-perm-key (cadr mac)(car mac)))
	      else
	      (display-error-noabort (cdr mac)))))

(defun load-these-macros ()
       (go-to-beginning-of-buffer)
       (do-forever
         (macedit-skip-over-whitespace)
         (if (at-end-of-buffer)(stop-doing))
         (if (looking-at "macro")
	   (macedit-take-up-definition)
	   else
	   (display-error "Bad format in macro file"))))


(defun load-macrofile (filepath)
       (save-excursion-buffer
         (load-macrofile- filepath)))

(defun load-macrofile- (filepath)
       (let ((thatbuf current-buffer))
	  (find-file-subr filepath)
	  (macro-edit-mode)
	  (setq macedit-source-buffer thatbuf)
	  (load-these-macros)
	  (go-to-beginning-of-buffer)))

(defun edit-macrofile ()
       (load-macrofile- (trim-minibuf-response "Edit Macro File: " NL)))

(defun edit-macros ()
       (let ((thatbuf current-buffer))
	  (go-to-or-create-buffer 'emacs-macros)
	  (if (empty-buffer-p current-buffer)
	      (insert-string "/* Emacs macros ")
	      (with-mark m
		       (insert-string  (date_time_af_))
		       (go-to-mark m)
		       (insert-string
		         (prog2 0 (macedit-scan-quoted-string)
			      (go-to-mark m)
			      (without-saving (kill-to-end-of-line)))))
	      (insert-string " */")
	      (do-times 2 (new-line))
	      (macro-edit-mode)
	      else
	      (go-to-end-of-buffer))
	  (setq macedit-source-buffer thatbuf)
	  (save-excursion (macedit-display-all-macros-to-buffer))))

;;;
;;;	Crufty lispmode-like functions
;;;
(defprop macedit-forward-term t argwants)
(defun macedit-forward-term ()
       (macedit-skip-over-whitespace)
       (if (not (at-end-of-buffer))
	 (macedit-scan-atom)))

(defprop macedit-forward-macdef t argwants)
(defun macedit-forward-macdef ()
       (if (and (bolp)(looking-at "macro"))
	 (macedit-scan-atom))
       (do-forever
         (macedit-skip-over-whitespace)
         (if (at-end-of-buffer)(stop-doing))
         (if (and (bolp)(looking-at "macro"))
	   (stop-doing))
         (macedit-scan-atom)))

(defun macedit-find-end-of-macdef ()
       (macedit-find-beginning-of-macdef)
       (do-forever
         (if (eq (macedit-scan-atom) 'end-macro)
	   (macedit-scan-atom)
	   (go-to-end-of-line)
	   (stop-doing))
         (if (at-end-of-buffer)(stop-doing))))

(defun macedit-mark-whole-macro ()
       (macedit-find-beginning-of-macdef)
       (set-the-mark)
       (macedit-find-end-of-macdef))

(defprop macedit-kill-term forward kills)
(defprop macedit-kill-term t argwants)
(defun macedit-kill-term ()
       (with-mark m
	        (macedit-forward-term)
	        (wipe-point-mark m)))

(defun macedit-skip-back-whitespace ()
       (do-forever
         (skip-back-whitespace)
         (if (at-beginning-of-buffer)(stop-doing))
         (if-back-at '//
		 (if (one-back-is-a '*)
		     (if (not (reverse-search "/*"))
		         (display-error "Unbalanced comment."))
		     else (stop-doing))
		 else (stop-doing))))

(defprop macedit-backward-term t argwannts)
(defun macedit-backward-term ()
       (macedit-skip-back-whitespace)
       (if-back-at '/" (macedit-skip-back-quoted-string)
	         else (skip-back-to-whitespace)))

(defun macedit-skip-back-quoted-string ()
       (do-forever
         (backward-char)
         (if (not (reverse-search """"))
	   (display-error "Unbalanced string."))
         (if-back-at '/" nil else (stop-doing)))
       (if-back-at '/. (backward-char)))


(defun macedit-backward-macdef ()
       (if (firstlinep)(go-to-beginning-of-line)
	 else
	 (if (and (bolp)(looking-at "macro"))
	     (backward-char))
	 (macedit-find-beginning-of-macdef)))


;;;
;;;	Here's some new ground ...
;;;	Automatic Lisp-program writing.
;;;	BSG 2/18/79

(defun macedit-compile-to-lisp ()
       (macedit-find-beginning-of-macdef)
       (let ((mac (macedit-produce-macro-definition)))
	  (if (null (car mac))
	      (display-error "Syntax error: " (cdr mac)))
	  (let ((interp
		(save-excursion-buffer
		  (go-to-buffer macedit-source-buffer)
		  (kmacro-display-interpret (caddr mac)))))
	       (go-to-or-create-buffer
	         (intern (make_atom
		         (catenate macedit-source-buffer ".e-macros.lisp"))))
	       (if (empty-buffer-p current-buffer)(lisp-mode)
		 (macomp-output-to-buffer '(%include e-macros))
		 (insert-string ";;; e-macros.incl.lisp is found in ")
		 (insert-string include-dir)
		 (do-times 2 (new-line))
		 else
		 (go-to-end-of-buffer))
	       (if (cadr mac)
		 (macomp-output-to-buffer
		   (list 'set-perm-key (get_pname (cadr mac))
		         (list 'quote (car mac)))))
	       (macomp-output-to-buffer
	         (macomp-compile-to-expr (car mac) interp))
	       (new-line)
	       (begin-defun))))

(define-autoload-lib emacs-macro-compile
		 macomp-output-to-buffer macomp-compile-to-expr)
