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
;;;	The default bindings for all characters in Multics EMACS.
;;;


;;; HISTORY COMMENTS:
;;;  1) change(84-01-23,Margolin), approve(), audit(),
;;;     install(86-08-20,MR12.0-1136):
;;;     pre-hcom history:
;;;               Extracted from e_.lisp, 7/27/78 by bsg & archy
     
;;;               Changed to use per-process erase
;;;                    and kill chars 11/27/78 by Richard S. Lamson
;;;               ^Z as prefix char, 4/16/79 by BSG
;;;               Added ^Z^F and ESC-~, 7/24/79 by GMP
;;;               Modified:  August-September 1979 by GMP for new dispatcher
;;;                    and new commands
;;;               Modified:  17 April 1981 for esc-T Richard Soley
;;;               Modified:   7 May   1981 for object-mode-find-file Richard Soley
;;;               Modified:  31 March 1982 for ^\ Richard Soley
;;;       Modified:  26 August 1982 to remove MCR-mode, add some more
;;;                          Fundamental/.ext-commands.  B. Margolin
;;;               Modified:  30 August 1982 to rename emacs-ITS-searches to
;;;                          emacs-extended-searches and add more autoloads
;;;                          from it.  B. Margolin
;;;               Modified:  29 November 1983 to add forward-char-command and
;;;                          backward-char-command as the default bindings of
;;;                          ^F and ^B.  B. Margolin
;;;               Modified:  23 January 1984 to bind permit-setting-esc-number.
;;;  2) change(84-12-25,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     to slashify #'s.
;;;  3) change(86-03-18,LJAdams), approve(86-03-18,MCR7361),
;;;     audit(86-04-17,Margolin), install(86-08-20,MR12.0-1136):
;;;     Added emacs-history-comment.
;;;                                                      END HISTORY COMMENTS


;;;
;;;	Character function bindings.
;;;

(let ((permit-setting-esc-number t))
     (mapc '(lambda (x) (set-key (car x) (cadr x)))

	 '((\177 rubout-char)

	   (ESC escape)
	   (^@ set-or-pop-the-mark)
	   (^A go-to-beginning-of-line)
	   (^B backward-char-command)
	   (^C re-execute-command)
	   (^D delete-char)
	   (^E go-to-end-of-line)
	   (^F forward-char-command)
	   (^G command-prompt-abort)
	   (^J noop)
	   (^K kill-lines)
	   (^L redisplay-command)
	   (^M new-line)			;carriage return
	   (^N next-line-command)
	   (^O open-space)
	   (^P prev-line-command)
	   (^Q quote-char)
	   (^R reverse-string-search)
	   (^S string-search)
	   (^T twiddle-chars)
	   (^U multiplier)
	   (^V next-screen)
	   (^W wipe-region)
					;^X  is a char prefix
	   (^Y yank)
					;^Z  is a char prefix
	   (^\ undo-prefix)
	   (^_ help-on-tap)

	   (ESC-CR cret-and-indent-relative)
	   (ESC-SPACE complete-command)
	   (ESC-% query-replace)
	   (ESC-/# rubout-word)
	   (ESC-\177 rubout-word)
	   (ESC-< go-to-beginning-of-buffer)
	   (ESC-> go-to-end-of-buffer)
	   (ESC-/; indent-for-comment)
	   (ESC-/? describe-key)
	   (ESC-// regexp-search)
	   (ESC-[ beginning-of-paragraph)
	   (ESC-\ delete-white-sides)
	   (ESC-] end-of-paragraph)
	   (ESC-_ underline-word)
	   (ESC-^ delete-line-indentation)
	   (ESC-~ unmodify-buffer)
	   (ESC-+ read-meta-argument)
	   (ESC-- read-meta-argument)
	   (ESC-0 read-meta-argument)
	   (ESC-1 read-meta-argument)
	   (ESC-2 read-meta-argument)
	   (ESC-3 read-meta-argument)
	   (ESC-4 read-meta-argument)
	   (ESC-5 read-meta-argument)
	   (ESC-6 read-meta-argument)
	   (ESC-7 read-meta-argument)
	   (ESC-8 read-meta-argument)
	   (ESC-9 read-meta-argument)
	   (ESC-A backward-sentence)
	   (ESC-B backward-word)
	   (ESC-C capitalize-initial-word)
	   (ESC-D delete-word)
	   (ESC-E forward-sentence)
	   (ESC-F forward-word)
	   (ESC-G go-to-line-number)
	   (ESC-H mark-paragraph)
	   (ESC-I tab-to-previous-columns)
	   (ESC-K kill-to-end-of-sentence)
	   (ESC-L lower-case-word)
	   (ESC-M skip-over-indentation)
	   (ESC-N down-comment-line)
	   (ESC-P prev-comment-line)
	   (ESC-Q runoff-fill-paragraph)
	   (ESC-R move-to-screen-edge)
	   (ESC-S center-line)
	   (ESC-T twiddle-words)
	   (ESC-U upper-case-word)
	   (ESC-V prev-screen)
	   (ESC-W copy-region)
	   (ESC-X extended-command)
	   (ESC-Y wipe-this-and-yank-previous)
	   (ESC-ESC eval-lisp-line)
	   (ESC-^B balance-parens-backward)
	   (ESC-^F balance-parens-forward)
	   (ESC-^G ignore-prefix)
	   (ESC-^I indent-to-fill-prefix)
	   (ESC-^O split-line)
	   (ESC-^V page-other-window)
	   (ESC-^W merge-last-kills-with-next)
	   (ESC-^Y yank-minibuf)

	   (^X-ESC escape-dont-exit-minibuf)
	   (^X/# kill-backward-sentence)
	   (^X/( begin-macro-collection)
	   (^X/) end-macro-collection)
	   (^X* show-last-or-current-macro)
	   (^X/. set-fill-prefix)
	   (^X/; set-comment-column)
	   (^X= linecounter)
	   (^X0 remove-window)
	   (^X1 expand-window-to-whole-screen)
	   (^X2 create-new-window-and-go-there)
	   (^X3 create-new-window-and-stay-here)
	   (^X4 select-another-window)
	   (^XB select-buffer)
	   (^XD edit-dir)
	   (^XE execute-last-editor-macro)
	   (^XF set-fill-column)
	   (^XG get-variable)
	   (^XH mark-whole-buffer)
	   (^XI insert-file)
	   (^XK kill-buffer)
	   (^XM send-mail)
	   (^XO select-other-window)
	   (^XQ macro-query)
	   (^XR rmail)
	   (^XS global-print)
	   (^XV view-lines)
	   (^XW multi-word-search)
	   (^XX put-variable)
	   (^X_ underline-region)
	   (^X^B list-buffers)
	   (^X^C quit-the-editor)
	   (^X^E comout-command)
	   (^X^F find-file)
	   (^X^I indent-rigidly)
	   (^X^L lower-case-region)
	   (^X^M eval-multics-command-line)
	   (^X^O delete-blank-lines)
	   (^X^R read-file)
	   (^X^S save-same-file)
	   (^X^T toggle-redisplay)
	   (^X^U upper-case-region)
	   (^X^W write-file)
	   (^X^X exchange-point-and-mark)
	   (^X\177 kill-backward-sentence)

	   (^ZF object-mode-find-file)
	   (^Z_ remove-underlining-from-word)
	   (^Z^@ set-named-mark)
	   (^Z^B edit-buffers)
	   (^Z^F get-filename)
	   (^Z^L redisplay-this-line)
	   (^Z^W edit-windows)
	   (^Z^V scroll-current-window)
	   (^Z^Z signalquit)
	   (^Z/; kill-comment)
	   (^ZG  go-to-named-mark)
	   )))


(mapc '(lambda (x)
	     (set-key x 'self-insert))
      (append (explodec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	    (explodec "0123456789")
	    (explodec "!""$%&'()*[=]|_:;/<->{}.,+^?~`@#\")
	    (list TAB SPACE (ascii 010))))


(mapc '(lambda (x)
	     (putprop x 'repeat 'argwants))
      '(self-insert delete-char forward-char forward-char-command
		forward-word backward-word
		delete-word rubout-word
		forward-sentence backward-sentence kill-to-end-of-sentence
		kill-backward-sentence
		beginning-of-paragraph end-of-paragraph
		open-space backward-char backward-char-command
		rubout-char
		forward-sexp backward-sexp kill-sexp))

;;; THE FOLLOWING TWO mapc's MUST BE KEPT ACCURATE
;;; WITH THE ABOVE LIST OF argwants FUNTIONS
(mapc '(lambda (x)
	     (putprop (car x) (cadr x) 'negative-arg-function)
	     (putprop (cadr x) (car x) 'negative-arg-function))
      '((delete-char rubout-char)
        (forward-char backward-char)
        (forward-char-command backward-char-command)
        (forward-word backward-word)
        (delete-word rubout-word)
        (forward-sentence backward-sentence)
        (kill-to-end-of-sentence kill-backward-sentence)
        (beginning-of-paragraph end-of-paragraph)
        (forward-sexp backward-sexp)))

(mapc '(lambda (x)
	     (putprop x 'bad-negative-argument 'negative-arg-function))
      '(self-insert open-space kill-sexp))


(define-autoload-lib emacs-dir-edit dired-mode edit-dir)
(define-autoload-lib emacs_pl1_mode_ pl1-mode electric-pl1-mode)
(define-autoload-lib emacs-compilations locate-next-error build-error-list
		 exit-error-scan-mode conditional-new-line compile-buffer
		 set-compiler set-compile-options)
(define-autoload-lib emacs-fortran-mode fortran-mode)
(define-autoload-lib emacs_rmail_ rmail send-mail)
(define-autoload-lib emacs-meter-redisplay mrds)
(define-autoload-lib emacs-console-messages accept-msgs accept-messages-path
		 accept-messages)
(define-autoload-lib emacs-lisp-mode lisp-mode)
(define-autoload-lib emacs-alm-mode alm-mode electric-alm-mode)
(define-autoload-lib emacs-text-mode text-mode runoff-mode compose-mode)
(define-autoload-lib emacs-buffer-edit edit-buffers edit-windows)
(define-autoload-lib emacs-extended-searches set-search-mode
		 character-search reverse-character-search
		 ITS-string-search reverse-ITS-string-search
		 incremental-search reverse-incremental-search
		 regexp-search reverse-regexp-search
		 character-search reverse-character-search query-replace
		 global-regexp-print)
(define-autoload-lib emacs-macro-edit edit-macros macro-edit-mode load-macrofile)
(define-autoload-lib emacs-lisp-debug-mode ldebug ldebug-trace-printer ldebug-trace-break)
(define-autoload-lib emacs-overwrite-mode overwrite-mode)
(define-autoload-lib emacs-object-mode object-mode-find-file object-mode)
(define-autoload-lib emacs-completions complete-command)
(define-autoload-lib emacs-history-comment add-history-comment add-hcom)

(defprop emacro macro-edit-mode suffix-mode)
(defprop cds pl1-mode suffix-mode)
(defprop rd pl1-mode suffix-mode)
(defprop lap lisp-mode suffix-mode)

(setq Fundamental/.ext-commands
  '(replace speedtype speedtypeoff setab fillon filloff lvars apropos
    describe make-wall-chart pl1-mode electric-pl1-mode set-screen-size
    set-comment-prefix accept-messages opt loadlib loadfile alm-mode lisp-mode
    accept-messages-path
    set-key set-permanent-key fundamental-mode show-macro save-macro
    set-search-mode runoff-fill-region fortran-mode reset-screen-size
    set-minibuffer-size reset-minibuffer-size ldebug edit-macros
    set-compiler set-compile-options
    list-named-marks
    fill-mode signalquit quit-the-editor alm-mode query-replace
    kill-contents-of-line
    add-history-comment
      ))

(setq trace-printer 'ldebug-trace-printer trace-break-fun 'ldebug-trace-break)


