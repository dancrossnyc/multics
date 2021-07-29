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
;;;	LDEBUG mode cause i needed it
;;;	BSG 2/24/79
;;;	Some features by RWK 9/79
;;;	Lisp trace features by BSG 10/6/79
;;;	Register-option forms commented out and moved to e_option_defaults_,
;;;	Barmar 1/19/84

(%include e-macros)
(declare (*lexpr ldebug-ioa)(genprefix /!ldb_))

(declare (special ldebug-closure ldebug-break-index ldebug-buf errset
	        ldebug-breaklist ldebug-cur-bkpt ldebug-trace-indent
	        ldebug-cur-bkpte ldebug-level e-lisp-error-mode))

(declare (*expr backward-sexp begin-defun down-list-level e_lap_$get-x7 
	      eval-top-level-form forward-sexp kill-sexp lisp-mode))

(defvar ldebug-mode-hook nil)

(setq ldebug-break-index 0
      ldebug-breaklist nil
      ldebug-cur-bkpte nil
      ldebug-level 0)            ; Number of nested breaks   


;; User options

(declare (special ldebug-prinlevel ldebug-prinlength ldebug-base ldebug-ibase))

;;; (register-option 'ldebug-prinlevel 6.) ;moved to e_option_defaults_
;;; (register-option 'ldebug-prinlength 10.) ;moved to e_option_defaults_
;;; (register-option 'ldebug-base 8.) ;moved to e_option_defaults_
;;; (register-option 'ldebug-ibase 8.) ;moved to e_option_defaults_

(defprop ldebug
"Enables a mode to take advantage of emacs editing capabilities
while interacting with lisp.  You type in se-expressions with the
full emacs command set available, and type CR to send your last
expresison to lisp.  The output will be inserted into the buffer with
prinlevel and prinlength bound to the values of the options ldebug-prinlevel
and ldebug-prinlength (default 6. and 10.).  The symbol * will be set to
the result of the evaluation, as in the default lisp top-level.

Errors encountered will enter a break level of editing on the buffer.
esc-G will return to the top-level edit loop, exc-P to the previous level,
esc-L will list the breaks currently in effect, esc-R resets a break
esc-s will show source for a breakpoint, esc-T will print a backtrace.  Esc-^S will
show where the editor was at the time of the error."
 documentation)

(defun ldebug-mode ()
       (lisp-mode)
       (dont-notice-modified-buffer current-buffer)
       (mapc '(lambda (x)(set-key (car x)(cadr x)))
	   '((^M 		ldebug-eval-and-print-result)
	     (esc-G 	ldebug-return-to-emacs-top-level)
	     (esc-P 	ldebug-return)
	     (esc-L	ldebug-list-breaks)
	     (esc-R	ldebug-reset-break)
	     (esc-S	ldebug-show-bkpt-source)
	     (esc-T	ldebug-trace-stack)
	     (esc-^S	ldebug-display-where-editor-was)))
       (setq current-buffer-mode  'Lisp/ Debug)
       (if ldebug-mode-hook
	 (errset (funcall ldebug-mode-hook))))

(defun %% (bx)(ldebug-catch bx (e_lap_$get-x7)))

(defun ldebug-catch (bx cl)
       (let ((ldebug-closure cl)
	   (ldebug-buf current-buffer)
	   (ldebug-cur-bkpt bx)
	   (ldebug-cur-bkpte (ldebug-find-bkpte bx)))
	  (let ((state (car ldebug-cur-bkpte)))
	       (cond ((memq state '(dead benign)))
		   ((eq state 'live)(ldebug-yggdrasil 'curbkpt))
		   ((not (numberp state)))	; ???
		   ((< state 2)
		    (rplaca ldebug-cur-bkpte 'live)
		    (ldebug-yggdrasil 'curbkpt))
		   (t (rplaca ldebug-cur-bkpte (1- state)))))))

(defprop ldebug
"$$$ enters a buffer LDEBUG in ldebug-mode, to
do interactive debugging of lisp code.  See the
documentation for ldebug-mode for details."
 documentation)

(defun ldebug ()
       (let ((ldebug-buf current-buffer)
	   (*rset t)
	   (ldebug-trace-indent 0)
	   (ldebug-closure (e_lap_$get-x7))
	   (ldebug-cur-bkpte nil)
	   (e-lisp-error-mode 'ldebug-lisp-toplevel-error-handler))
	  (ldebug-yggdrasil 'ldebug)))
       
(defun ldebug-lisp-toplevel-error-handler (arg)
       (setq arg arg)
       (let ((ldebug-buf current-buffer)
	   (ldebug-level (1+ ldebug-level))
	   (ldebug-closure (cadddr (errframe nil)))
	   (* nil))
	  (ldebug-yggdrasil 'errbreak)))

(defun ldebug-in-breakp ()
       (or ldebug-cur-bkpte (display-error "No current break.")))

(defun within-LDEBUG/'s-buffer-window macro (x)
       `(let ((oldbuf current-buffer)
	    (oldfdw (buffer-on-display-in-window current-buffer)))
	   (find-buffer-in-window 'LDEBUG)
	   (prog2 0 (progn ,@(cdr x))
		(if (null oldfdw)
		    (go-to-buffer oldbuf)
		    else (find-buffer-in-window oldbuf)))))

(defun ldebug-yggdrasil (key)
       (within-LDEBUG/'s-buffer-window
         (if (empty-buffer-p current-buffer)
	   (new-line)
	   (ldebug-mode)
	   else
	   (go-to-end-of-buffer))
         (if (not (line-is-blank))(new-line))
         (if (eq key 'errbreak)
	   (errset
	     (ring-tty-bell)
	     (let ((f (caddr (errframe nil))))
		(new-line)
		(ldebug-ioa "Lisp breakpoint " (caddr f) " at level "
			  (decimal-rep ldebug-level) " in buffer "
			  ldebug-buf ":")
		(ldebug-ioa (car f)(maknam (explodec (cadr f)))))))
         (if (eq key 'trace-break)
	   (ring-tty-bell)
	   (ldebug-ioa "Entry breakpoint to function "
		     (cadr ldebug-cur-bkpte)))
         (if (eq key 'curbkpt)
	   (ring-tty-bell)
	   (ldebug-ioa  "Break " (decimal-rep ldebug-cur-bkpt)
		      " in " (cadr ldebug-cur-bkpte)))
         (let ((*rset t)
	     (ldebug-trace-indent ldebug-trace-indent))
	    (let ((val (catch (charlisten) gazongues-des-lispes)))
	         (if (eq val 'tres-grandes-gazongues)
		   (go-to-buffer ldebug-buf)
		   (command-quit)
		   else val)))))

(defprop ldebug-return-to-emacs-top-level
"Release the current level of LDEBUG mode, returning to the
previous level.  All executing code betweent the two levels
is aborted."
  documentation)

(defun ldebug-return-to-emacs-top-level  ()
       (ldebug-ioa "$g")
       (throw 'tres-grandes-gazongues gazongues-des-lispes))

(defun ldebug-ioa n
       (go-to-end-of-buffer)
       (insert-string (apply 'catenate (listify n)))
       (redisplay)
       (new-line))

(defprop ldebug-eval-and-print-result
  "Takes the contents of the current line, reads it as an s-expression,
and inserts the result into the buffer, with prinlength and prinlevel
bound according to the ldebug-prinlength and ldebug-prinlevel options.
The variable * is set to the result of the evaluation, as in the default
lisp top-level." documentation)

;Make this loser use backward-sexp to get entire sexpression!

(defun ldebug-eval-and-print-result ()
       (let ((string (e_lap_$trim
		   (let ((s (curline-as-string)))
		        (let ((sl (stringlength s)))
			   (and (samepnamep (substr s sl 1) NL)
			        (setq s (substr s 1 (1- sl)))))
		        s))))
	  (if (not (nullstringp string))
	      (let ((errset 'ldebug-lisp-toplevel-error-handler))
		 (ldebug-output-to-buffer
		   (let ((fail-act    'ldebug-lisp-toplevel-error-handler)
;		         (gc-daemon   'ldebug-lisp-toplevel-error-handler)
		         (pdl-overflow 'ldebug-lisp-toplevel-error-handler)
		         (wrng-type-arg 'ldebug-lisp-toplevel-error-handler)
		         (*rset-trap  'ldebug-lisp-toplevel-error-handler)
		         (unbnd-vrbl  'ldebug-lisp-toplevel-error-handler)
		         (undf-fnctn  'ldebug-lisp-toplevel-error-handler)
		         (unseen-go-tag 'ldebug-lisp-toplevel-error-handler)
		         (wrng-no-args 'ldebug-lisp-toplevel-error-handler)
		         (ibase ldebug-ibase))
		        (car (errset
			     (prog2 0
				  (setq * (eval (read-from-string string)))
				  (new-line)
				  (insert-string "=> ")) nil)))))
	      else (new-line))))

;ldebug-flush-whitespace deletes extra white-space off the both ends of
;a string for passing to readline.  Clobbers

(defun ldebug-flush-whitespace (string)
  (nreverse (ldebug-flush-whitespace-beginning
	    (nreverse (ldebug-flush-whitespace-beginning string)))))

(defun ldebug-flush-whitespace-beginning (string)
   (do ((string string (cdr string)))
       ((not (memq (car string) '(9. 10. 32.)))
        string)))

(defprop ldebug-return
"Restart the current breakpoint or Lisp error which entered
the LDEBUG buffer, restoring buffer, point, and window. If a
numeric argument is given, restart this (trace or code) break
that many times automatically (including this time).  If
a Lisp error is being restarted, return the current line's
Lisp value to the Lisp error breakpoint."
 documetation)

(defun ldebug-return ()
       (if numarg
	 (ldebug-in-breakp)
	 (ldebug-ioa "Set for " (decimal-rep numarg) " proceeds.")
	 (rplaca ldebug-cur-bkpte numarg))
       (throw (prog2 0
		 (if (not (line-is-blank))
		     (car (errset
			  (let ((ibase ldebug-ibase))
			       (read-from-string (curline-as-string)))))
		     else nil)
		 (ldebug-ioa "$p"))
	    gazongues-des-lispes))

;;;
;;;	Break format is (number . (state function buffer mark))
;;;

(defprop ldebug-set-break
"Set a breakpoint in the Lisp code pointed at by the cursor.
The break number, in decimal, is plaed in the break code,
which is placed in the current buffer. The function pointed at
is reevaluated. When the break is executed, LDEBUG will be entered."
 documentation)

(defun ldebug-set-break ()
       (setq *rset t)
       (let ((fn nil))
	  (save-excursion
	    (begin-defun)
	    (down-list-level)
	    (do-times 2 (forward-sexp))
	    (with-mark m
		     (backward-sexp)
		     (setq fn (point-mark-to-string m))))
	  (insert-string
	    (catenate
	      " (%% "
	      (decimal-rep (setq ldebug-break-index (1+ ldebug-break-index)))
	      ".)"))
	  (backward-sexp)
	  (setq ldebug-breaklist
	        (cons (cons ldebug-break-index
			(list 'live fn current-buffer (set-mark)))
		    ldebug-breaklist))	        
	  (save-excursion (eval-top-level-form))))

(defun ldebug-errset-trap (x)
       (setq x (caddr (errframe nil)))
       (find-buffer-in-window 'LDEBUG)
       (new-line)
       (ldebug-ioa  "<<ERROR>>: "  (car x))
       (ldebug-ioa "     " (maknam (explodec (cdr x))))
       (command-quit))

(defprop ldebug-trace-stack
  "Insert into the buffer a traceback of the stack." documentation)

(defun ldebug-trace-stack ()
       (ldebug-ioa "--STACK TRACE--")
       (do x (evalframe nil)(evalframe (cadddr x))(null x)
	 (if (< (cadddr x) ldebug-closure)
	     (ldebug-output-to-buffer (caddr x))
	     (redisplay)))			;Show while ye grinds.
       (ldebug-ioa "--END TRACE--")
       (new-line))

(defprop ldebug-show-bkpt-source
   "Show the source for the current LDEBUG code breakpoint.
If a numeric argument is given, show the source for that breakpoint,
by break number."
   documentation)

(defun ldebug-show-bkpt-source ()
       (if numarg (ldebug-display-bkpt-source (ldebug-find-bkpte numarg))
	 else
	 (ldebug-in-breakp)
	 (if (eq (cadddr ldebug-cur-bkpte) '*trace)
	     (display-error "Trace breaks have no source."))
	 (ldebug-display-bkpt-source ldebug-cur-bkpte)))

(defun ldebug-display-bkpt-source (brk)
       (find-buffer-in-window (caddr brk))
       (go-to-mark (cadddr brk)))

(defprop ldebug-reset-break
"With no numeric argument, reset the current (active) LDEBUG
code or trace entry break.  With a numeric argument, reset
the code break of that break number." documentation)

(defun ldebug-reset-break ()
       (if numarg (ldebug-reset-bkpte numarg (ldebug-find-bkpte numarg))
	 else
	 (ldebug-in-breakp)
	 (if (eq (cadddr ldebug-cur-bkpte) '*trace)
	     (let ((fn (cadr ldebug-cur-bkpte)))
		(if (caar (errset (eval (list 'untrace fn))))
		    (rplaca ldebug-cur-bkpte 'dead)
		    (ldebug-ioa "Reset entry break to function " fn)))
	     else
	     (ldebug-reset-bkpte ldebug-cur-bkpt ldebug-cur-bkpte))))

(defun ldebug-reset-bkpte (bx bkpte)
	 (rplaca bkpte 'dead)
	 (save-excursion-buffer
	   (go-to-buffer (caddr bkpte))
	   (go-to-mark (cadddr bkpte))
	   (backward-char)
	   (kill-sexp)
	   (eval-top-level-form))
	 (minibuffer-print "Reset break " (decimal-rep bx)))

(defun ldebug-find-bkpte (no)
       (or (cdr (assoc no ldebug-breaklist))
	 (display-error "Breakpoint " (decimal-rep no) " somehow got lost.")))

(defprop ldebug-list-breaks
"Insert into the LDEBUG buffer a list of all active
breakpoints: their number, function, status, and buffer."
  documentation)

(defun ldebug-list-breaks ()
       (if (null ldebug-breaklist)(display-error "No active breaks.")
	 else
	 (ldebug-ioa "BREAK LIST")
	 (ldebug-ioa "#     Function       Status   Buffer")
	 (do l (setq ldebug-breaklist
		   (sort ldebug-breaklist
		         '(lambda (x y)(< (car x)(car y)))))
	     (cdr l)
	     (null l)
	     (let ((n (caar l))(brk (cdar l)))
		(if (not (eq (car brk) 'dead))
		    (insert-string (decimal-rep n))
		    (format-to-col 6.)
		    (insert-string (cadr brk))
		    (format-to-col 21.)
		    (insert-string (maknam (explodec (car brk))))
		    (format-to-col 30.)
		    (insert-string (caddr brk))
		    (if (eq brk ldebug-cur-bkpte)
		        (format-to-col 50.)
		        (insert-string "<<<"))
		    (redisplay)
		    (new-line))))
	 (ldebug-ioa "END BREAK LIST")))

(defprop ldebug-display-where-editor-was
"Select the buffer (and window, if that buffer is on display),
where Emacs was when the current breakpoint was taken.  The cursor
will be moved to the place where point was when the break was taken.
If point is moved, it will remain moved when the break is restarted."
 documentation)

(defun ldebug-display-where-editor-was ()
       (let ((m (save-excursion-buffer
	        (go-to-buffer ldebug-buf)
	        (set-mark))))
	  (find-buffer-in-window ldebug-buf)
	  (go-to-mark m)
	  (release-mark m)))


;print the desired lisp form into the buffer, with right base, prinlevel,
;etc.

(defun ldebug-output-to-buffer (form)
   (insert-string (maknam
		(let ((prinlevel ldebug-prinlevel)
		      (prinlength ldebug-prinlength)
		      (base ldebug-base))
		     (explode form))))
   (redisplay)
   (new-line))


;;;
;;;	Trace Hackery
;;;	BSG 10/6/79
;;;



(%include e-macros)

(declare (special trace-indent-incr trace-indent-max trace-ok-flag
	        ldebug-prinlength ldebug-prinlevel rdis-suppress-redisplay))

(setq ldebug-trace-indent 0)

(defun ldebug-trace-printer (arg)
       (if trace-ok-flag
	 (let ((trace-ok-flag nil))
	      (save-excursion-buffer
	        (go-to-or-create-buffer 'LDEBUG)
	        (go-to-end-of-buffer)
	        (if (empty-buffer-p current-buffer)(ldebug-mode))
	        (ldebug-trace-real-printer
		(car arg)(cadr arg)(caddr arg)(cadddr arg)(cddddr arg))
	        (if (not (buffer-on-display-in-window 'LDEBUG))
		  (local-display-current-line)))
	      (if (buffer-on-display-in-window 'LDEBUG)
		(within-LDEBUG/'s-buffer-window (redisplay))))))

(defun ldebug-trace-real-printer (recurlev type fn arg stuff)
       (setq ldebug-trace-indent (max 0 ldebug-trace-indent))
       (new-line)
       (and (eq type 'exit)(setq ldebug-trace-indent (- ldebug-trace-indent trace-indent-incr)))
       (whitespace-to-hpos (max 0 (min trace-indent-max ldebug-trace-indent))) 
       (and (eq type 'enter)(setq ldebug-trace-indent (+ ldebug-trace-indent trace-indent-incr)))
       (insert-string "(")
       (insert-string (decimal-rep recurlev))
       (insert-string " ")
       (insert-string type)
       (insert-string " ")
       (insert-string fn)
       (insert-string " ")
       (ldebug-trace-insert-lisp-string arg)
       (mapc '(lambda (x)(insert-string " ")(ldebug-trace-insert-lisp-string x))
	   stuff)
       (insert-string ")"))))

(defun ldebug-trace-insert-lisp-string (x)
       (let ((prinlength ldebug-prinlength)
	   (prinlevel ldebug-prinlevel)
	   (base ldebug-base))
	  (insert-string (maknam (explode x)))))


(defun ldebug-trace-break (fname)
       (let ((ldebug-closure (e_lap_$get-x7))
	   (ldebug-buf current-buffer)
	   (ldebug-cur-bkpt '*trace)
	   (ldebug-cur-bkpte (or (get fname 'ldebug-trace-break)
			     (putprop fname
				    (list 'live fname '*trace '*trace)
				    'ldebug-trace-break))))
	  (let ((state (car ldebug-cur-bkpte))) 
	       (cond ((memq state '(dead benign)))
		   ((eq state 'live)(ldebug-yggdrasil 'trace-break))
		   ((not (numberp state)))	; ???
		   ((< state 2)
		    (rplaca ldebug-cur-bkpte 'live)
		    (ldebug-yggdrasil 'curbkpt))
		   (t (rplaca ldebug-cur-bkpte (1- state)))))))

