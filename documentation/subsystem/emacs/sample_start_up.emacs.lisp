;;;
;;;   Sample start_up.emacs, Lisp source
;;;   Must be compiled to get start_up.emacs
;;;   Last modified 10/16/80

(declare (special rmail-mode-hook my-personal-name))   ;Global variables

(defun Jones/'s-start-up ()			;Compilation =>  efficiency
       (opt 'find-file-set-modes 'on)		;foo.pl1 => pl1 mode, etc.
       (setq rmail-mode-hook 'Jones-rmail-mode-hook)
					;Like to read msgs backwards
       (accept-messages)			;Accept console messages
       (opt 'suppress-backspace-display 'on)	; _\010A => _A
       (setq my-personal-name "Harvey B. Jones"))	;For RMAIL

(defun Jones-rmail-mode-hook ()(setq rmail-msgx rmail-msgcount))

(Jones/'s-start-up)
