;;;Shell
(setq shell-file-name "/bin/bash")

(shell)

(rename-buffer "bbb-shell")

(shell)

(rename-buffer "aaa-shell")

;;;Font
;(set-default-font "9x15")
;(set-default-font "courier new-12")
(set-default-font "Lucida Console 12")

(set-background-color "black")

(set-foreground-color "white")

(set-cursor-color "red")

;;;Keys
;;; VIM # forward search current word
(defun isearch-cur-word (fun)
  "ISearch current word use function FUN."
  (let ((cur-word (current-word)))
    (if (not cur-word)
        (message "No word under cursor.")
      (call-interactively fun)
      (isearch-yank-string cur-word))))
 
(defun isearch-forward-cur-word (&optional backward)
  "`isearch-forward' current word."
  (interactive "P")
  (let ((fun (if backward 'isearch-backward 'isearch-forward)))
    (isearch-cur-word fun)))

;;; Vim % match paren
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))


(global-set-key [f8] 'kill-buffer)
(global-set-key [Esc] 'exit-minibuffer)
(global-set-key [f12] 'switch-to-buffer)
(global-set-key [f11] 'distraction-mode) 
(global-set-key [(control a)] 'mark-whole-buffer)
(global-set-key [(control k)] 'kill-whole-line)
(global-set-key [(control u)] 'undo)
(global-set-key [(meta l)] 'list-buffers)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [f9] 'indent-region)
(global-set-key [(control ?])] 'find-tag)
(global-set-key [(control t)] 'pop-tag-mark)
(fset 'yes-or-no-p 'y-or-n-p)
(hl-line-mode t)
(ido-mode t)
(setq cua-mode t)
(setq x-select-enable-clipboard t)
(setq ido-save-directory-list-file nil)

;;;Programing
(require 'git-emacs)

(global-set-key "%" 'match-paren)

;;;C-func setting
;; For some reason 1) c-backward-syntactic-ws is a macro and 2)  under Emacs 22
;; bytecode cannot call (unexpanded) macros at run time:
(eval-when-compile (require 'cc-defs))

;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\" of the Google
C++ Style Guide for the case where the previous line ends with an open
parenthese.

\"Current C expression\", as per the Google Style Guide and as clarified by
subsequent discussions,
means the whole expression regardless of the number of nested parentheses, but
excluding non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    ;; We are making a reasonable assumption that if there is a control
    ;; structure to indent past, it has to be at the beginning of the line.
    (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
        (goto-char (match-end 1)))
    (vector (+ 4 (current-column)))))

(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0))))
  "Google C/C++ Programming Style")

(defun google-set-c-style ()
  "Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t))

(defun google-make-newline-indent ()
  "Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))

(provide 'google-c-style)
;;; google-c-style.el ends here

(defconst linux-c-style
  `((c-mode)
    (c-set-style . "K&R")
    (tab-width . 8)
    (indent-tabs-mode . t)
    (c-basic-offset . 8))
  "Linux Kernel C/C++ Programming Style")

(defun linux-set-c-style ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Kernel" linux-c-style t))


(defun get-end-of-line ()
  "Get the position of the end of the current line."
  (save-excursion
    (let ((junk (end-of-line)))
      (point))))

(defface ifdef-highlight-face1
  '((((type tty pc) (class color))
     (:background "turquoise3"))
    (((class color) (background light))
     (:background "paleturquoise"))
    (((class color) (background dark))
     (:background "paleturquoise4"))
    (t (:underline t)))
  "The face of the out most #if...#endif block.")

(defface ifdef-highlight-face4
  '((((type tty pc) (class color))
     (:background "pink3"))
    (((class color) (background light))
     (:background "pink"))
    (((class color) (background dark))
     (:background "pink4"))
    (t (:underline t)))
  "The face of the in most #if...#endif block.")

(defface ifdef-highlight-face3
  '((((type tty pc) (class color))
     (:background "yellow3"))
    (((class color) (background light))
     (:background "yellow"))
    (((class color) (background dark))
     (:background "yellow4"))
    (t (:underline t)))
  "The face of the 3rd level #if...#endif block.")

(defface ifdef-highlight-face2
  '((((type tty pc) (class color))
     (:background "paleGreen1"))
    (((class color) (background light))
     (:background "paleGreen2"))
    (((class color) (background dark))
     (:background "paleGreen3"))
    (t (:underline t)))
  "The face of the 2nd level #if...#endif block.")

(defvar ifdef-highlight-face1 'ifdef-highlight-face1)
(defvar ifdef-highlight-face2 'ifdef-highlight-face2)
(defvar ifdef-highlight-face3 'ifdef-highlight-face3)
(defvar ifdef-highlight-face4 'ifdef-highlight-face4)

;; put the faces in a hash table, only 4 colors 
(defvar face-table (make-hash-table :test 'eql :size 16))
(puthash 1 ifdef-highlight-face1 face-table)
(puthash 2 ifdef-highlight-face2 face-table)
(puthash 3 ifdef-highlight-face3 face-table)
(puthash 4 ifdef-highlight-face4 face-table)

;; the overlay list
(defvar ifdef-overlay-list nil)
(defvar ifdef-marked-flag nil)

(defun mark-line (level begin end)
  "Mark the region from BEGIN to END with the LEVELth face."
  (let ((ov (make-overlay begin end))
        (face (gethash level face-table)))
    (when face
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 0)
      (push ov ifdef-overlay-list))))

(defun mark-ifdef3 (start end other)
  "Mark the block with delimiter START and END.
OTHERS is intermediate mark, which can be nil. "
  (make-variable-buffer-local 'ifdef-marked-flag)
  (make-variable-buffer-local 'ifdef-overlay-list)
  (if ifdef-marked-flag          ; if already marked, remove the marks
      (ifdef-remove-marks)
    (save-excursion
      (let ((nest 0)
            (continue-flag t))
        (goto-char (point-min))
        (while continue-flag
          (when (re-search-forward  start
                                    (get-end-of-line) t 1) ; find START delimiter
            (setq nest (1+ nest))
            (if (> nest 0)
                (mark-line nest (match-beginning 0) (match-end 0))))

          (if other                     ; if other is not nil
              (when (re-search-forward  other
                                        (get-end-of-line) t 1) ; find a #else or #elif
                (if (> nest 0)
                    (mark-line nest (match-beginning 0) (match-end 0)))))

          (when (re-search-forward end
                                   (get-end-of-line) t 1) ; find END delimiter
            (if (> nest 0)
                (mark-line nest (match-beginning 0) (match-end 0)))
            (setq nest (1- nest))
            (if (<= nest 0)       ; found the out most START delimiter
                (setq nest 0)))
          (if (= 1 (forward-line 1))  ; we reach the end of file, exit
              (setq continue-flag nil))))
      (setq ifdef-marked-flag t))))


(defun ifdef-remove-marks ()
  (dolist (ov ifdef-overlay-list)
    (delete-overlay ov)
    (setq ifdef-overlay-list nil)
    (setq ifdef-marked-flag nil)))  ; reset the flag


(defun mark-blocks ()
  "Mark blocks delimited by { and }. "
  (interactive)
  (mark-ifdef3 "{" "}" nil))

(defun mark-if-makefile ()
  "Mark if ... else ... endif in GNU makefile."
  (interactive)
  (mark-ifdef3 "^[ 	]*if.*$"  "^[ 	]*endif.*$"  "^[ 	]*el.*$"))

(defun mark-ifdef ()
  "Mark if ... else ... endif in GNU makefile."
  (interactive)
  (mark-ifdef3 "^[ 	]*#[ 	]*if.*$"  "^[ 	]*#[ 	]*endif.*$"  "^[ 	]*#[ 	]*el.*$"))

  
;;;; add (require 'ifdef) in your .emacs file
(provide 'ifdef)
(require 'ifdef)
(add-hook 'c-mode-common-hook 'linux-set-c-style)
(add-hook 'c-mode-common-hook 'mark-ifdef)


;;;Windows
(when (fboundp 'winner-mode) 
(winner-mode) 
(windmove-default-keybindings)) 

(setq frame-title-format "cuibixiong@%b") 

(setq inhibit-startup-message t)

(setq make-backup-files nil)

(setq auto-save-mode nil)

(setq column-number-mode t)

(setq display-time-24hr-format t)

(setq display-time-day-and-date t)

(global-cwarn-mode 1)

(ansi-color-for-comint-mode-on)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq kill-ring-max 200)

(defun distraction-mode ()
(interactive)
(x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

;;;File
(defun dos2unix ()
  "dos2unix on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "unix2dos on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))


;(defun make-some-files-read-only ()
;  "when file opened is of a certain mode, make it read only"
;  (when (memq major-mode '(c++-mode tcl-mode text-mode python-mode))
;    (toggle-read-only 1)))
;(add-hook 'find-file-hooks 'make-some-files-read-only)


;;;Setting
(setq default-directory "~/")
(setq my-shebang-patterns
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
	    "^#!/usr/.*/sh"
	    "^#!/usr/.*/bash"
	    "^#!/bin/sh"
	    "^#!/.*/perl"
	    "^#!/.*/awk"
	    "^#!/.*/sed"
	    "^#!/bin/bash"))
(add-hook
 'after-save-hook
 (lambda ()
   (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
       (progn
	 ;; This puts message in *Message* twice, but minibuffer
	 ;; output looks better.
	 (message (concat "Wrote " (buffer-file-name)))
	 (save-excursion
	   (goto-char (point-min))
	   ;; Always checks every pattern even after
	   ;; match.  Inefficient but easy.
	   (dolist (my-shebang-pat my-shebang-patterns)
	     (if (looking-at my-shebang-pat)
		 (if (= (shell-command
			 (concat "chmod u+x " (buffer-file-name)))
			0)
		     (message (concat
			       "Wrote and made executable "
			       (buffer-file-name))))))))
     ;; This puts message in *Message* twice, but minibuffer output
     ;; looks better.
     (message (concat "Wrote " (buffer-file-name))))))



;;;Remote
(require 'drkm-fav)
(setq drkm-fav:favourite-directories-alist
  '(("home"  . "/mtk81232@mtkslt002:~"))))
