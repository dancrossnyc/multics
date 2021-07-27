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
;;;	Bufed pro bufonibus.
;;;	BSG 4/14/79
;;; Modified: 4 December 1983 - B. Margolin - fix misspelling in
;;;	    message printed by bufed-examine.
;;;

(%include e-macros)

(declare
  (special bufed-goback-buf bufed-wopt bufed-kill-list known-buflist
	 two-window-mode))

(declare
  (*expr buffer-kill create-new-window-and-stay-here delete-window
         get-buffer-state go-to-hpos lruify-current-window lruify-window
         save-same-file window-adjust-lower window-adjust-upper))

(defun edit-buffers ()(bufed))
(defun bufed ()
       (prog (tbufnam origbuf origmark)
	   (setq origbuf current-buffer)
	   (setq tbufnam 'BUFEDIT)
	   (if numarg (find-buffer-in-window tbufnam)
	       else (go-to-or-create-buffer tbufnam))
	   (register-local-var 'bufed-goback-buf)
	   (setq bufed-goback-buf origbuf)
	   (register-local-var 'bufed-wopt)
	   (setq bufed-wopt numarg numarg nil)
	   (register-local-var 'bufed-kill-list)
	   (setq bufed-kill-list nil)
	   (bufedit-mode)
	   (go-to-beginning-of-buffer)
	   (setq read-only-flag nil)
	   (do ((bufl (delq tbufnam (subst nil nil known-buflist))
		    (cdr bufl))
	        (buf))
	       ((null bufl))
	       (setq buf (car bufl))
	       (or (line-is-blank)(without-saving (kill-to-end-of-line)))
	       (and (eq buf previous-buffer) (setq origmark (set-mark)))
	       (cond ((eq buf bufed-goback-buf)(insert-char ">"))
		   (t (insert-char SPACE)))
	       (cond ((get-buffer-state buf 'buffer-modified-flag)
		    (insert-char "*"))
		   (t (insert-char SPACE)))
	       (do-times 2 (insert-char SPACE))
	       (insert-string buf)
	       (cond ((get-buffer-state buf 'fpathname)
		    (format-to-col 25.)
		    (insert-string (get-buffer-state buf 'fpathname))))
	       (if (lastlinep)(new-line) else (next-line)))
	   (backward-char)
	   (without-saving (with-mark m (go-to-end-of-buffer)(wipe-point-mark m)))
	   (go-to-mark origmark)
	   (release-mark origmark)
	   (setq read-only-flag t buffer-modified-flag nil)
	   (select-buffer-window current-buffer 'cursize)))


(defun bufedit-mode ()
       (if (empty-buffer-p current-buffer)
	 (setq current-buffer-mode 'Buffer/ Edit)
	 (mapc '(lambda (x)(set-key (car x)(cadr x)))
	       '(
	         (w	edit-windows)	(W	edit-windows)
	         (q	bufed-quit)	(Q	bufed-quit)
	         (e	bufed-examine)	(E	bufed-examine)
	         (d	bufed-kill)	(D	bufed-kill)
	         (k	bufed-kill)	(K	bufed-kill)
	         (g	bufed-go)		(G	bufed-go)
	         (u	bufed-undelete)	(U	bufed-undelete)
	         (p	bufed-prev)	(P	bufed-prev)
	         (n	bufed-next)	(N	bufed-next)
	         (f	bufed-find)	(F	bufed-find)
	         (s	bufed-save)	(S	bufed-save)
	         (^X^Q	bufed-quit)))))

(defun bufed-prev ()
       (if (firstlinep)(go-to-end-of-buffer)
	 (go-to-beginning-of-line)
	 else (prev-line)))

(defun bufed-next ()
       (if (lastlinep)(go-to-beginning-of-buffer) else (next-line)))

(defun bufed-validate-target (targ)
       (if (memq targ bufed-kill-list)
	 (display-error "Buffer " targ " to be deleted.  Can't go there."))
       (if (not (memq targ known-buflist))
	 (display-error "Buffer " targ " no longer exists.  Choose another.")))

(defun bufed-go ()
       (let ((targ (bufed-get-bufnam))
	   (goback bufed-goback-buf))		;gonna get switched w bufs
	  (bufed-validate-target targ)
	  (bufed-check-deletions)
	  (set-buffer-self-destruct 'BUFEDIT)
	  (select-buffer-window targ nil)
	  (setq previous-buffer goback)))

(defun bufed-find ()
       (let ((buf (bufed-get-bufnam))
	   (goback bufed-goback-buf))
	  (bufed-validate-target buf)
	  (bufed-check-deletions)
	  (set-buffer-self-destruct 'BUFEDIT)
	  (find-buffer-in-window buf)
	  (setq previous-buffer goback)))


(defun bufed-quit ()
       (bufed-validate-target bufed-goback-buf)
       (bufed-check-deletions)
       (set-buffer-self-destruct 'BUFEDIT)
       (if bufed-wopt
	 (lruify-current-window)
	 (find-buffer-in-window bufed-goback-buf)
	 else
	 (select-buffer-window bufed-goback-buf nil)))

(defun bufed-examine ()
       (if two-window-mode
	 (let ((bub current-buffer)
	       (targ (bufed-get-bufnam)))
	      (let ((wf (buffer-on-display-in-window targ)))
		 (if wf
		     (display-error-remark
		       "Buffer " targ " on display in window "
		       (decimal-rep wf))
		     else
		     (find-buffer-in-window targ)
		     (setq wf (buffer-on-display-in-window targ))
		     (display-error-remark
		       "Buffer " targ " on display in window "
		       (decimal-rep wf))
		     (find-buffer-in-window bub)
		     (lruify-window wf))))
	 else
	 (go-to-buffer (bufed-get-bufnam))
	 (display-error-noabort "^XB CR to get back to Buffer Edit.")))

(defun bufed-kill ()
       (go-to-hpos 2)
       (if-at 'X (go-to-beginning-of-line)
	    else
	    (without-modifying (delete-char)
			   (insert-char 'X))
	    (setq bufed-kill-list (cons (bufed-get-bufnam) bufed-kill-list))
	    (if  (lastlinep)(go-to-beginning-of-line)
	         else (next-line))))

(defun bufed-check-deletions ()
       (if bufed-kill-list
	 (init-local-displays)
	 (mapc 'local-display-generator-nnl
	       '("Buffers to Kill:" "----------------" ""))
	 (mapc 'local-display-generator-nnl bufed-kill-list)
	 (end-local-displays)
	 (if (yesp "Go ahead and kill these buffers? ")
	     (mapc 'buffer-kill bufed-kill-list))
	 (setq bufed-kill-list nil)))

(defun bufed-undelete ()
       (go-to-hpos 2)
       (if-at 'X
	    (without-modifying (delete-char)
			   (insert-string " "))
	    (setq bufed-kill-list (delq (bufed-get-bufnam) bufed-kill-list))
	    (if (lastlinep)(go-to-beginning-of-line)
	        else (next-line))))

(defun bufed-get-bufnam ()
       (go-to-hpos 4.)
       (prog2 0
	    (make_atom
	      (with-mark b
		       (if (go-to-hpos 25.)
			 (if (forward-search-in-line ">")
			     (backward-char)
			     else
			     (go-to-end-of-line))
			 (skip-back-whitespace))
		       (point-mark-to-string b)))
	    (go-to-beginning-of-line)))

(defun bufed-save ()
       (save-excursion-buffer
         (go-to-buffer (bufed-get-bufnam))
         (save-same-file))
       (go-to-hpos 1)
       (if-at '*
	    (without-modifying
	      (delete-char) (insert-char SPACE)))
       (if (lastlinep) (go-to-beginning-of-line)
	 else (next-line)))
;;;
;;;
;;;	Window editor   BSG 4/14/79
;;;


(declare (special selected-window nuwindows))
(defun edit-windows ()
       (let ((ona numarg)(numarg nil))
	  (if ona (find-buffer-in-window 'WINDOWSTAT))
	  (wstat-edit))
       (select-buffer-window current-buffer 'cursize))

(defun wstat-edit ()
       (go-to-or-create-buffer 'WINDOWSTAT)
       (wstat-mode)
       (select-buffer-window
         current-buffer
         (if (buffer-on-display-in-window current-buffer)
	   nuwindows
	   else (1+ nuwindows)))
       (wstat-create-display)
;       (redisplay)
       (lruify-current-window))

(defun wstat-create-display ()
       (setq read-only-flag nil buffer-modified-flag t)	;suppr modified msg
       (go-to-beginning-of-buffer)
       (do i 1 (1+ i)(> i nuwindows)
	 (without-saving (kill-to-end-of-line))
	 (insert-string (decimal-rep i))
	 (if (= i selected-window)(insert-string "*"))
	 (format-to-col 4)
	 (let ((info (window-info i)))
	      (insert-string (decimal-rep (cadr info)))	;internal #
	 (format-to-col 10.)
	 (insert-string (decimal-rep (caar info)))   ;startline
	      (format-to-col 15.)
	      (insert-string (decimal-rep (cdar info)))	;nlines
	      (format-to-col 20.)
	      (insert-string (caddr info))	;buffer
	      (format-to-col 40.)
	      (if (null (cadddr info))
		(insert-string "<<EMPTY>>")
		else
		(insert-string
		  (substr (cadddr info) 1
			(min 10. (1- (stringlength (cadddr info))))))))
	 (if (lastlinep)(new-line) else (next-line)))
       (rubout-char)
       (without-saving (with-mark m (go-to-end-of-buffer)(wipe-point-mark m)))
       (setq buffer-modified-flag nil read-only-flag t)
       (go-to-beginning-of-buffer))

(defun wstat-mode ()
       (if (empty-buffer-p current-buffer)
	 (mapc '(lambda (x)(set-key (car x)(cadr x)))
	       '((b	edit-buffers)
	         (c	wstat-create-window)
	         (/3	wstat-create-window)
	         (g	wstat-go-window)
	         (f	wstat-go-window)
	         (^	wstat-push-up-top)
	         (v	wstat-push-down-bottom)
	         (u	wstat-pull-up-bottom)
	         (a	wstat-pull-down-top)
	         (k	wstat-kill-window)
	         (d	wstat-kill-window)
	         (n	wstat-next)
	         (p	wstat-prev)))
	 (setq current-buffer-mode 'Window/ Edit)))

(defun wstat-create-window ()
       (create-new-window-and-stay-here)
       (wstat-create-display)
       (go-to-end-of-buffer)
       (go-to-beginning-of-line))

(defprop wstat-next t argwants)(defprop wstat-prev t argwants)
(defun wstat-next ()(if (lastlinep)(go-to-beginning-of-buffer)else (next-line)))
(defun wstat-prev ()(if (firstlinep)(go-to-end-of-buffer)(go-to-beginning-of-line)
		    else (prev-line)))

(defun wstat-go-window ()
	  (set-buffer-self-destruct 'WINDOWSTAT)
	  (select-window (wstat-collect-wnum)))

(defun wstat-kill-window ()
       (delete-window (wstat-collect-wnum))
       (save-excursion (wstat-create-display)))

(defun wstat-collect-wnum ()
       (prog2 (go-to-beginning-of-line)
	    (let ((ibase 10.))
	         (readlist (explodec
			 (with-mark m (forward-word)
				  (point-mark-to-string m)))))
	    (go-to-beginning-of-line)))

(defun wstat-push-up-top ()
       (let ((howmuch (or numarg 1))
	   (u (wstat-collect-wnum)))
	  (if (= u 1)(display-error "The top window has no topline!"))
	  (if (< (- (cdar (window-info (1- u))) howmuch) 3)
	      (display-error "Attempt to make upstairs window too small."))
	  (window-adjust-upper u (- howmuch)))
       (save-excursion (wstat-create-display)))

(defun wstat-push-down-bottom ()
       (let ((howmuch (or numarg 1))
	   (u (wstat-collect-wnum)))
	  (if (= u nuwindows)
	      (display-error "The bottom window has no bottomline!"))
	  (if (< (- (cdar (window-info (1+ u))) howmuch) 3)
	      (display-error "Attempt to make downstairs window too small."))
	  (window-adjust-lower u howmuch))
       (save-excursion (wstat-create-display)))

(defun wstat-pull-down-top ()
       (let ((howmuch (or numarg 1))
	  (u (wstat-collect-wnum)))
	  (if (= u 1)(display-error "The top window has no topline!"))
	  (if (< (- (cdar (window-info u)) howmuch) 3)
	      (display-error "Attempt to make this window too small."))
	  (window-adjust-upper u howmuch))
       (save-excursion (wstat-create-display)))

(defun wstat-pull-up-bottom ()
       (let ((howmuch (or numarg 1))
	   (u (wstat-collect-wnum)))
	  (if (= u nuwindows)
	      (display-error "The bottom window has no bottomline!"))
	  (if (< (- (cdar (window-info u)) howmuch) 3)
	      (display-error "Attempt to make this window too small."))
	  (window-adjust-lower u (- howmuch)))
       (save-excursion (wstat-create-display)))
