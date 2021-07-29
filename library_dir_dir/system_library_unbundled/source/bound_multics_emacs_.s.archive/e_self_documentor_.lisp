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
;;;	Self-documentation System
;;;	 BSG 9/30/78 - 10/07/78
;;;	 Ut veritatem ipsam suam ipse dicet.

;;;	 Hacked 10/5/79 by BSG for defcom macrology.
;;;	 Hacked March 1981 by CWH to clean up describe/explain-command.

(declare (genprefix /!e_sd_))
(%include e-macros)
(declare (*expr center-line e_info_vfilesman_$get_recp e_info_vfilesman_$open
	      e_info_vfilesman_$seek e_info_vfilesman_$update
	      e_lap_$gsubstr exch-point-mark get-key-binding get-key-name
	      key-prompt mark-same-line-p runoff-fill-paragraph 
	      runoff-fill-region untabify))
(declare (special site-dir
	 doc-pack-open-mode doc-pack-curfun doc-pack-mark
	 doc-pack-rec-status fill-prefix tty-no-upmotionp))


(setq doc-pack-open-mode nil)


;;; Read documentation for given function from vfile
(defun get-doc (fun)
       (or (let ((d (get fun 'documentation)))
	       (if d
		 (setq doc-pack-rec-status nil)
		 (insert-string d))
	       d)
	 (progn
	   (or doc-pack-open-mode (doc-pack-open 'read))
	   (if (not (= 0 (e_info_vfilesman_$seek fun)))
	       (if (get fun 'editor-macro)
		 (insert-string
		   (catenate
		     "$$$ is a keyboard macro.  Type $$extended-command$ "
		     "show-macro " fun " CR to display its definition."))
		 else
		 (insert-string
		   (catenate "No documentation for " fun " found.")))
	       else
	       (let ((recp-result (e_info_vfilesman_$get_recp)))
		  (if (not (= 0 (caddr recp-result)))
		      else
		      (insert-string (e_lap_$gsubstr
				   (cons (car recp-result)
				         (cadr recp-result))
				   0 (cadr recp-result)))))))))


;;; Open the vfile for given mode
(defun doc-pack-open (mode)
       (let ((code (e_info_vfilesman_$open site-dir
				   (cond ((eq mode 'update) 1)
				         (t 0)))))
	  (if (not (= 0 code))
	      (display-com-error code "Cannot open the the info file for " mode "."))))


;;; Update documentation of current function
(defcom update-cmd-doc
        (go-to-end-of-buffer)
        (e_info_vfilesman_$update (point-mark-to-string doc-pack-mark)
			    (cond ((eq doc-pack-rec-status nil)
				 (display-error
				   "Record state inconsistent"))
				((eq doc-pack-rec-status 'new) 1)
				(t 0)))
        (minibuffer-print "Updated " doc-pack-curfun)
        (let ((code (e_info_vfilesman_$seek doc-pack-curfun)))
	   (if (not (= code 0))
	       (setq doc-pack-curfun 'Ze/ Garbage doc-pack-rec-status nil)
	       (display-com-error code "Problem updating vfile.")))
        (setq buffer-modified-flag nil
	    doc-pack-rec-status 'old))	;buffer now in file


;;; Setup to edit the documentation vfile
(defcom edit-emacs-cmd-doc
        &documentation "Enters Emacs command documentation update
         mode.  Requires write access to the info vfile."
       (or (eq doc-pack-open-mode 'update)
	 (doc-pack-open 'update))
       (go-to-or-create-buffer 'Command/ Documentation)
       (set-key '^X^S 'update-cmd-doc)
       (set-key '^X^A 'update-cmd-doc-from-old)
       (setq current-buffer-mode 'Doc/ Update)
       (without-saving (destroy-buffer-contents)))



;;; Edit documentation of a particular function
(defcom edit-cmd-doc
        &arguments ((command-name &prompt "Extended command to document: "))
        &doc "Fetches for editing the command documentation for
        an emacs extended command"
        (setq doc-pack-curfun command-name)
        (edit-emacs-cmd-doc)
        (insert-string command-name)
        (do-times 2 (new-line))
        (register-local-var 'doc-pack-mark)
        (setq doc-pack-mark (set-mark))
        (setq doc-pack-rec-status
	    (cond ((= 0 (e_info_vfilesman_$seek command-name))
		 (get-doc command-name)
		 'old)
		(t
		 (minibuffer-print "Not yet documented... Input the new doc.")
		 'new)))
       (and tty-no-upmotionp (display-buffer-as-printout)))


;;; Read documentation from old info segment
(defun get-old-style-doc (charrep)
       (save-excursion-buffer
         (go-to-or-create-buffer 'emacs-document)
         (if (empty-buffer-p 'emacs-document)
	   (read-in-file (catenate env-dir ">" "editor.info")))
         (go-to-beginning-of-buffer)
         (if (samepnamep (substr charrep 1 3) "esc")
	   (setq charrep (catenate "ESC" (substr charrep 4))))
         (do-forever
	 (if (looking-at charrep)
	     (skip-to-whitespace)
	     (skip-over-whitespace)
	     (with-mark m
		      (next-line)
		      (do-forever
		        (if (at-white-char)
			  (next-line)
			  else (stop-doing)))
		      (return (point-mark-to-string m))))    ;unwp release s mark
	 (if (lastlinep)
	     (setq charrep nil) (return ""))
	 (next-line))))


;;; Update documentation of a key's binding
(defun update-cmd-doc-from-old ()
       (let ((kp3 (key-prompt "Update Key Doc: ")))
	  (let ((symbol (get-key-name kp3))
	        (cmd (get-key-binding kp3)))
	       (edit-cmd-doc cmd)
	       (if numarg
		 (with-mark m
			  (insert-string (get-old-style-doc symbol))
			  (with-mark n
				   (go-to-mark m)
				   (do-forever
				     (if-at TAB (delete-char))
				     (if (mark-on-current-line-p n)
				         (stop-doing))
				     (next-line))))))))


;;; Replace all command names with key binding (if any)
(defun replace-substitutable-command-names (curcmd buf)
       (do-forever
         (if (not (forward-search "$$"))(stop-doing))
         (do-times 2 (rubout-char))
         (with-mark start
		(forward-search "$")
		(rubout-char)
		(let ((sym (make_atom (point-mark-to-string start))))
		     (without-saving (wipe-point-mark start))
		     (if (nullstringp sym)(insert-string curcmd)
		         else
		         (let ((val (find-key-in-buf sym buf)))
			    (if val (insert-string val)
			        else (insert-string sym))))))))


;; "description" is of the form "^S", "^X^S", or "esc-X describe".  It
;;  is substituted into the command documentation at appropriate places.
;; "symbol" is the lisp symbol the command is associated with, and is used
;;   to find the command documentation.
;; "first-line" is the first line of text to appear in the local display.

(defun describe-internal (description symbol first-line)
  (let ((original-buffer current-buffer))
    (display-as-printout
      (remprop current-buffer 'temporary-buffer)
      (setq buffer-modified-flag t)
      (insert-string first-line)
      (do-times 2 (new-line))
      (insert-command-doc symbol description original-buffer)
      (putprop current-buffer t 'temporary-buffer))))


;;; Describe an extended command
(defcom describe
        &arguments ((symbol &symbol &prompt "Extended command to describe: "))
        (let ((description (get-extcommed-name symbol current-buffer)))
	(describe-internal description symbol description)))


;;; Print documentation of a key's binding
(defun explain-command ()
  (let ((key (key-prompt "Explain Key: ")))
    (let ((symbol (get-key-binding key))
	(description (get-key-name key)))
      (describe-internal description symbol
		     (catenate description "		" symbol)))))


;;; Get "key" name of an extended command
(defun get-extcommed-name (cmd buf)
       (let ((excname (find-key-in-buf 'extended-command buf)))
	  (if (nullstringp cmd) excname
	      else (catenate excname " " cmd))))


;;; Insert documentation of a function into buffer
(defun insert-command-doc (cmd key-name origbuf)
       (with-mark
         m
         (get-doc cmd)
         (go-to-mark m)
         (replace-substitutable-command-names key-name origbuf)
         (go-to-mark m)
         (set-the-mark)
         (go-to-end-of-buffer)
         (region-fill-by-paragraphs)))


;;; Get key's binding in specified buffer
(defun get-cmd-symbol-in-buf (key buf)
       (save-excursion-buffer
         (go-to-or-create-buffer buf)
         (get-key-binding key)))


;;; Find key in buffer with given binding
(defun find-key-in-buf (cmd buf)
       (save-excursion-buffer
         (go-to-or-create-buffer buf)
         (catch
	 (progn
	   (map-over-emacs-commands
	     (function (lambda (ktps cname ss)
			   (if (eq ss cname)
			       (throw ktps find-key-))))
	     cmd)
	   nil)
	 find-key-)))


;;; Describe a key
(defun describe-key ()
       (if numarg (show-command-name)
	 else (explain-command)))


;;; Show name of binding of a key
(defun show-command-name ()
       (let ((k (key-prompt "Show Key Function: ")))
	  (let ((kn (get-key-name k))
	        (cmd (get-key-binding k)))
	       (if (memq cmd '(nil undefined-command))
		 (minibuffer-print kn " is not defined in this buffer.")
		 else
		 (cond ((getl cmd '(subr expr))
		        (minibuffer-print kn " = " cmd))
		       ((get cmd 'editor-macro)
		        (minibuffer-print kn " = " cmd " (keyboard macro)"))
		       (t (minibuffer-print kn " = " cmd " (unimplemented)")))))))


;;; Fill region a paragraph at a time

(defun region-fill-by-paragraphs ()
       (with-the-mark-last
         m				;loop in order
         (go-to-beginning-of-line)
         (if (mark-on-current-line-p m)
	   (insert-string fill-prefix)
	   else
	   (do-forever
	     (do-forever			;find beginning of para
	       (if (mark-on-current-line-p m)(stop-doing))
	       (if (at-white-char)(next-line)
		 else (stop-doing)))
	     (if (or (mark-on-current-line-p m)
		   (point>markp m))
	         (stop-doing))
	     (with-mark first-good-stuff
		      (insert-string fill-prefix)
		      (do-forever
		        (if (mark-on-current-line-p m)(stop-doing))
		        (if (line-is-blank)(stop-doing)
			  else (next-line)))
		      (if (line-is-blank)(prev-line))
		      (go-to-end-of-line)
		      (move-mark der-wahrer-mark first-good-stuff)
		      (without-saving (runoff-fill-region))
		      (if (or (point>markp m)
			    (mark-on-current-line-p m))
			(release-mark first-good-stuff)
			(stop-doing)))))))


;;; Document all keys and extended commands defined in buffer
(defcom document-buffer-commands
        &documentation "Creates a document similar to fundamental-mode.info
        describing all commands and bindings, suitable for dprinting."
        (prog (env was-mode buf)
	    (setq was-mode current-buffer-mode buf current-buffer)
	    (set (setq env (gensym)) nil)
	    (map-over-emacs-commands
	      (function (lambda (symbol suspect arg)
			    (cond ((memq suspect '(self-insert read-meta-argument)))
				(t (set arg (cons (cons symbol suspect)
					        (symeval arg)))))))
	      env)
	    (go-to-or-create-buffer
	      (make_atom (catenate  current-buffer ".doc")))
	    (setq buffer-modified-flag t)
	    (destroy-buffer-contents)
	    (insert-string "Multics Emacs Commands")
	    (insert-string " (")
	    (insert-string was-mode)
	    (insert-string " mode) ")
	    (insert-string (date))
	    (do-times 2 (new-line))
	    (insert-string TAB)
	    (insert-string "K__e_y_s _a_n_d _t_h_e_i_r _b_i_n_d_i_n_g_s")
	    (new-line)
	    (insert-string (catenate TAB TAB))
	    (insert-string
	      (catenate "(Extended (" (get-extcommed-name "" buf) ") commands are listed at the end.)" ))
	    (do-times 2 (new-line))
	    (document-emacs-functions-to-buffer
	      (symeval env) buf 1)
	    (let ((extcomsym (make_atom (catenate was-mode ".ext-commands")))
		(fill-prefix ""))
	         (if (boundp extcomsym)
		   (do-times 4 (new-line))
		   (insert-string (catenate TAB TAB TAB))
		   (insert-string "E__x_t_e_n_d_e_d C__o_m_m_a_n_d_s")
		   (do-times 2 (new-line))
		   (insert-string
		     (catenate
		       "Type " (get-extcommed-name "" buf)
		       " followed by the command name, and a carriage return"
		       " to invoke these commands."))
		   (let ((numarg 1))(runoff-fill-paragraph))
		   (do-times 2 (new-line))
		   (document-emacs-extcomms-to-buffer
		     (set extcomsym (sort (symeval extcomsym) 'alphalessp))
		     buf 1 )))))


;;; Put documentation of given extended commands into buffer
(defun document-emacs-extcomms-to-buffer (clist buf parm)
       (setq fill-prefix "   ")
       (let ((excn (get-extcommed-name "" buf)))
	  (do l clist (cdr l)(null l)
	      (let ((cmd (car l))
		  (exname (catenate excn " " (car l))))
		 (insert-string exname)
		 (do-times 2 (new-line))
		 (delete-white-sides)
		 (let ((numarg parm))
		      (insert-command-doc cmd exname buf))
		 (do-times 2 (new-line))
		 (delete-white-sides)))))
		     

;;; Put documentation of given commands into buffer
(defun document-emacs-functions-to-buffer (clist buf parm)
       (setq fill-prefix "   ")
       (do l (sortcar clist 'alphalessp)(cdr l)(null l)
	 (let ((key (caar l))
	       (cmd (cdar l)))
	      (insert-string key)
	      (format-to-col 20.)
	      (insert-string cmd)
	      (do-times 2 (new-line))
	      (delete-white-sides)
	      (let ((numarg parm))
		 (insert-command-doc cmd key buf))
	      (do-times 2 (new-line))
	      (delete-white-sides))))


;;; Apropos first written 5/24/78 by BSG and archy.
(defcom apropos
        &arguments ((string &prompt "String to match for apropos commands: "))
        (prog (env was-mode was-buf extcom-matches)
	    (setq was-mode current-buffer-mode was-buf current-buffer)
	    (setq env (ncons string))
	    (map-over-emacs-commands
	      (function (lambda (symbol suspect arg)
			    (cond ((memq suspect '(self-insert read-meta-argument)))
				((not (= 0 (index suspect (car arg))))
				 (rplacd arg (cons (cons symbol suspect)
					         (cdr arg)))))))
	      env)
	    (setq extcom-matches
		(sort (mapcan
		        (function (lambda (x)
				      (if (not (= 0 (index x string)))
					(list x))))
		        (symeval  'Fundamental/.ext-commands))
		      'alphalessp))		;TEMP KLUDGE!!!
	    (if (not (or extcom-matches (cdr env)))
	        (display-error "apropos:  No matches for " string))
	    (save-excursion-buffer
	      (go-to-or-create-buffer 'apropros)
	      (setq buffer-modified-flag t)
	      (destroy-buffer-contents)
	      (insert-string "Apropos """)
	      (insert-string string)
	      (insert-string """")
	      (insert-string " (")
	      (insert-string was-mode)
	      (insert-string " mode)")
	      (do-times 2 (new-line))
	      (list-emacs-functions-to-buffer (cdr env))
	      (if extcom-matches
		(new-line)
		(insert-string "Extended Commands:")
		(do-times 2 (new-line))
		(let ((exname (get-extcommed-name "" was-buf)))
		     (mapc '(lambda (x)
				(insert-string (catenate
					       exname " " x))
				(new-line))
			 extcom-matches)))
	      (display-buffer-as-printout)
	      (putprop current-buffer t 'temporary-buffer)))
        (end-local-displays)))


;;; Make a chart of all key bindings in this buffer
(defcom make-wall-chart
        (prog (env was-mode)
	    (setq was-mode current-buffer-mode)
	    (set (setq env (gensym)) nil)
	    (map-over-emacs-commands
	      '(lambda (symbol suspect arg)
		     (cond ((memq suspect '(self-insert read-meta-argument)))
			 (t (set arg (cons (cons symbol suspect)
				         (symeval arg))))))
	      env)
	    (go-to-or-create-buffer 'wall-chart)
	    (setq fill-column 133.)
	    (setq buffer-modified-flag t)
	    (destroy-buffer-contents)
	    (insert-string "Multics Emacs Wall Chart")
	    (insert-string " (")
	    (insert-string was-mode)
	    (insert-string " mode)  ")
	    (insert-string (date))
	    (center-line)
	    (do-times 3 (new-line))
	    (with-mark bob
		     (list-emacs-functions-to-buffer (symeval env))
		     (new-line)
		     (n-way-columnate-region 3 bob 50.))
	    (go-to-beginning-of-buffer)
	    (setq buffer-modified-flag nil)))

;;; List given keys' bindings to buffer
(defun list-emacs-functions-to-buffer (list)
       (do ((l (sortcar list 'alphalessp) (cdr l)))
	 ((null l))
	 (insert-string (printable (caar l)))
	 (format-to-col 10.)
	 (insert-string (cdar l))
	 (cond ((not (getl (cdar l) '(expr subr autoload)))
	        (format-to-col 30.)
	        (insert-string (cond ((get (cdar l) 'editor-macro)
				"(keyboard macro)")
			         (t "(unimplemented)")))))
	 (new-line)))

;;;
;;;   Columnate stuff BSG 10/31/79
;;;

(declare (special FF tab-equivalent))

(defun n-way-columnate-region (n mark pagel)
       (if (point>markp mark)(exch-point-mark mark))
       (untabify tab-equivalent)
       (let ((maxl 0)
	   (nlines 0)
	   (mlist nil))
	  (unwind-protect
	    (progn
	      (save-excursion
	        (do-forever
		(if (mark-on-current-line-p mark)(stop-doing))
		(if (= 0 (\ nlines pagel))(setq mlist (cons (set-mark) mlist)))
		(setq nlines (1+ nlines))
		(go-to-end-of-line)
		(skip-back-whitespace-in-line)
		(setq maxl (max maxl (cur-hpos)))
		(next-line)))
	      (save-excursion
	        (n-way-columnate-generate n mark pagel
				    (1+ maxl)
				    nlines
				    (reverse mlist)))
	      (without-saving (wipe-point-mark mark)))
	    (mapc 'release-mark mlist))))

(defun n-way-columnate-generate (n mark pagel maxl nlines mlist)
       (go-to-end-of-buffer)
       (do-forever
         (do lno 0 (1+ lno)(= lno pagel)
	   (new-line)
	   (do ((depth 0 (1+ depth))
	        (l mlist (cdr l)))
	       ((= depth n))
	       (if (and (car l)(not (mark-same-line-p mark (car l))))
		 (setq nlines (1- nlines))
		 (whitespace-to-hpos (* depth maxl))
		 (insert-string
		   (save-excursion
		     (go-to-mark (car l))
		     (go-to-end-of-line)
		     (skip-back-whitespace-in-line)
		     (prog2 0
			  (point-mark-to-string (car l))
			  (if (not (lastlinep))
			      (next-line)
			      (set-mark-here (car l)))))))
	       (delete-white-sides))
	   (if (< nlines 1)(stop-doing)))
         (new-line)
         (insert-string FF)
         (do i n (1- i)(= i 0)(setq mlist (cdr mlist)))
         (if (< nlines 1)(stop-doing))))

