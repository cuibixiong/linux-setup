;;;Shell
(shell)
(rename-buffer "ccc-shell")
(shell)
(rename-buffer "bbb-shell")
(shell)
(rename-buffer "aaa-shell")

;;;Font
(set-default-font "9x15")
(set-background-color "black")
(set-foreground-color "white")

;;;Misc
(setq frame-title-format "Bill@%b") 
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-mode nil)
(setq column-number-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq default-fill-column 80)
(global-cwarn-mode 1)
(ansi-color-for-comint-mode-on)
(global-set-key "%" 'match-paren)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq kill-ring-max 200)

(defun dos2unix ()
  "dos2unix on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "unix2dos on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun switch-to-shell ()
  (interactive)
  (let ((buffer (get-buffer-create "aaa-shell")))
    (switch-to-buffer buffer)
    (unless (equal major-mode 'lisp-interaction-mode)
      (lisp-interaction-mode))))
(global-set-key [(control 1)] 'switch-to-shell)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
  ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))



(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
			    (lambda (proc change)
			      (when (string-match "\\(finished\\|exited\\)" change)
				(kill-buffer (process-buffer proc))))))))
(add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)
(add-hook 'term-mode-hook 'kill-buffer-when-shell-command-exit)
(defun kill-buffer-when-compile-success (process)
  "Close current buffer when `shell-command' exit."
  (set-process-sentinel process
			(lambda (proc change)
			  (when (string-match "finished" change)
			    (delete-windows-on (process-buffer proc))))))
(add-hook 'compilation-start-hook 'kill-buffer-when-compile-success)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key [f8] 'kill-buffer)
(global-set-key [Esc] 'exit-minibuffer)
(global-set-key [f12] 'switch-to-buffer)

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

;;;windows
(global-set-key [f11] 'distraction-mode) 
(defun distraction-mode ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
  )
(global-set-key [f10] 'split-window-vertically)

;;;Files
(global-set-key [(meta o)] 'find-file)

;;;Edit
(setq cua-mode t)
(setq x-select-enable-clipboard t)
(global-set-key [(control a)] 'mark-whole-buffer)
(global-set-key [(control q)] 'toggle-read-only)
(global-set-key [(control r)] 'query-replace)
(global-set-key [(control u)] 'undo)
(global-set-key [(meta l)] 'list-buffers)
(global-set-key [(meta g)] 'goto-line)

;;;Code
(global-set-key [f9] 'indent-region)

;;;TAGS
(global-set-key [(control ?])] 'find-tag)
(global-set-key [(control t)] 'pop-tag-mark)


;;; google-c-style.el --- Google's C/C++ style for c-mode

;; Keywords: c, tools

;; google-c-style.el is Copyright (C) 2008 Google Inc. All Rights Reserved.
;;
;; It is free software; you can redistribute it and/or modify it under the
;; terms of either:
;;
;; a) the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version, or
;;
;; b) the "Artistic License".

;;; Commentary:

;; Provides the google C/C++ coding style. You may wish to add
;; `google-set-c-style' to your `c-mode-common-hook' after requiring this
;; file. For example:
;;
;;    (add-hook 'c-mode-common-hook 'google-set-c-style)
;;
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;;
;;    (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;; Code:

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



;;
;; Copyright (C) 2009 The Android Open Source Project
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; Variables to customize and common functions for the Android build
;; support in Emacs.
;; There should be no interactive function in this module.
;;
;; You need to have a proper buildspec.mk file in your root directory
;; for this module to work (see $TOP/build/buildspec.mk.default).
;; If the path the product's files/image uses an a product alias, you
;; need to add a mapping in `android-product-alias-map'. For instance
;; if TARGET_PRODUCT is foo but the build directory is out/target/product/bar,
;; you need to add a mapping Target:foo -> Alias:bar
;;

;;; Code:

(defgroup android nil
  "Support for android development in Emacs."
  :prefix "android-"                    ; Currently unused.
  :tag    "Android"
  :group  'tools)

;;;###autoload
(defcustom android-compilation-jobs 2
  "Number of jobs used to do a compilation (-j option of make)."
  :type 'integer
  :group 'android)

;;;###autoload
(defcustom android-compilation-no-buildenv-warning t
  "If not nil, suppress warnings from the build env (Makefile,
bash) from the compilation output since they interfere with
`next-error'."
  :type 'boolean
  :group 'android)

;;;###autoload
(defcustom android-product-alias-map nil
  "Alist between product targets (declared in buildspec.mk) and actual
 product build directory used by `android-product'.

For instance if TARGET_PRODUCT is 'foo' but the build directory
 is 'out/target/product/bar', you need to add a mapping Target:foo -> Alias:bar."
  :type '(repeat (list (string :tag "Target")
                       (string :tag "Alias")))
  :group 'android)

(defconst android-output-buffer-name "*Android Output*"
  "Name of the default buffer for the output of the commands.
There is only one instance of such a buffer.")

(defun android-find-build-tree-root ()
  "Ascend the current path until the root of the android build tree is found.
Similarly to the shell functions in envsetup.sh, for the root both ./Makefile
and ./build/core/envsetup.mk are exiting files.
Return the root of the build tree.  Signal an error if not found."
  (let ((default-directory default-directory))
    (while (and (> (length default-directory) 2)
                (not (file-exists-p (concat default-directory
                                            "Makefile")))
                (not (file-exists-p (concat default-directory
                                            "build/core/envsetup.mk"))))
      (setq default-directory
            (substring default-directory 0
                       (string-match "[^/]+/$" default-directory))))
    (if (> (length default-directory) 2)
        default-directory
      (error "Not in a valid android tree"))))

(defun android-project-p ()
  "Return nil if not in an android build tree."
  (condition-case nil
      (android-find-build-tree-root)
    (error nil)))

(defun android-host ()
  "Return the <system>-<arch> string (e.g linux-x86).
Only linux and darwin on x86 architectures are supported."
  (or (string-match "x86" system-configuration)
      (string-match "i386" system-configuration)
      (error "Unknown arch"))
  (or (and (string-match "darwin" system-configuration) "darwin-x86")
      (and (string-match "linux" system-configuration) "linux-x86")
      (error "Unknown system")))

(defun android-product ()
  "Return the product built according to the buildspec.mk.
You must have buildspec.mk file in the top directory.

Additional product aliases can be listed in `android-product-alias-map'
if the product actually built is different from the one listed
in buildspec.mk"
  (save-excursion
    (let* ((buildspec (concat (android-find-build-tree-root) "buildspec.mk"))
           (product (with-current-buffer (find-file-noselect buildspec)
                      (goto-char (point-min))
                      (search-forward "TARGET_PRODUCT:=")
                      (buffer-substring-no-properties (point)
                                                      (scan-sexps (point) 1))))
           (alias (assoc product android-product-alias-map)))
					; Post processing, adjust the names.
      (if (not alias)
          product
        (nth 1 alias)))))

(defun android-product-path ()
  "Return the full path to the product directory.

Additional product aliases can be added in `android-product-alias-map'
if the product actually built is different from the one listed
in buildspec.mk"
  (let ((path (concat (android-find-build-tree-root) "out/target/product/"
                      (android-product))))
    (when (not (file-exists-p path))
      (error (format "%s does not exist. If product %s maps to another one,
add an entry to android-product-map." path (android-product))))
    path))

(defun android-find-host-bin (binary)
  "Return the full path to the host BINARY.
Binaries don't depend on the device, just on the host type.
Try first to locate BINARY in the out/host tree.  Fallback using
the shell exec PATH setup."
  (if (android-project-p)
      (let ((path (concat (android-find-build-tree-root) "out/host/"
                          (android-host) "/bin/" binary)))
        (if (file-exists-p path)
            path
          (error (concat binary " is missing."))))
    (executable-find binary)))

(defun android-adb ()
  "Return the path to the adb executable.
If not in the build tree use the PATH env variable."
  (android-find-host-bin "adb"))

(defun android-fastboot ()
  "Return the path to the fastboot executable.
If not in the build tree use the PATH env variable."
					; For fastboot -p is the name of the product, *not* the full path to
					; its directory like adb requests sometimes.
  (concat (android-find-host-bin "fastboot") " -p " (android-product)))

(defun android-adb-command (command &optional product)
  "Execute 'adb COMMAND'.
If the optional PRODUCT is not nil, -p (android-product-path) is used
when adb is invoked."
  (when (get-buffer android-output-buffer-name)
    (with-current-buffer android-output-buffer-name
      (erase-buffer)))
  (if product
      (shell-command (concat (android-adb) " -p " (android-product-path)
                             " " command)
                     android-output-buffer-name)
    (shell-command (concat (android-adb) " " command)
                   android-output-buffer-name)))

(defun android-adb-shell-command (command)
  "Execute 'adb shell COMMAND'."
  (android-adb-command (concat " shell " command)
                       android-output-buffer-name))

(provide 'android-common)

;;; android-common.el ends here

;;; android-compile.el --- Compile the Android source tree.
;;
;; Copyright (C) 2009 The Android Open Source Project
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; Helper functions to compile Android file within emacs.
;; This module ignores 'build/envsetup.sh' and any enviroment set by the
;; 'lunch' shell function.
;; Instead it relies solely on 'buildspec.mk', remember that when you
;; switch configuration.
;;
;; The only interactive function is 'android-compile'.
;; In your .emacs load this file (e.g (require 'android-compile)) then:
;;
;;   (add-hook 'c++-mode-hook 'android-compile)
;;   (add-hook 'java-mode-hook 'android-compile)
;; and/or
;;   (global-set-key [f9] 'android-compile)
;;
;;
;; TODO: Maybe we could cache the result of the compile function in
;; buffer local vars.

;;; Code:

(require 'compile)
(require 'android-common)

;; No need to be customized.
(defvar android-compile-ignore-re
  "\\(^\\(\\sw\\|[/_]\\)+\\(Makefile\\|\\.mk\\):[0-9]+:.*warning\\)\\|\\(^/bin/bash\\)"
  "RE to match line to suppress during a compilation.
During the compilation process line matching the above will be
suppressed if `android-compilation-no-buildenv-warning' is non nil.")

(defun android-makefile-exists-p (directory)
  "Return t if an Android makefile exists in DIRECTORY."
					; Test for Android.mk first: more likely.
  (or (file-exists-p (concat directory "Android.mk"))
      (file-exists-p (concat directory "Makefile"))))

(defun android-find-makefile (topdir)
  "Ascend the current path until an Android makefile is found.
Makefiles are named Android.mk except in the root directory where
the file is named Makefile.
TOPDIR is the root directory of the build.
Return a list with 2 elements (MAKEFILE_PATH IS_ROOT_MAKEFILE).
MAKEFILE_PATH is the relative path of the makefile wrt TOPDIR.
Signal an error if no Makefile was found."
  ;; TODO: Could check that topdir is the start of default-directory.
  (unless (> (length topdir) 2)
    (error "Topdir invalid %s for current dir %s" topdir default-directory))
  (let ((default-directory default-directory)
        file)
    ;; Ascend the path.
    (while (and (> (length default-directory) (length topdir))
                (not (android-makefile-exists-p default-directory)))
      (setq default-directory
            (substring default-directory 0
                       (string-match "[^/]+/$" default-directory))))

    (when (not (android-makefile-exists-p default-directory))
      (error "Not in a valid android tree"))

    (if (string= default-directory topdir)
        (list "Makefile" t)
      ;; Remove the root dir at the start of the filename
      (setq default-directory (substring default-directory (length topdir) nil))
      (setq file (concat default-directory "Android.mk"))
      (list file nil))))

;; This filter is registered as a `compilation-filter-hook' and is
;; called when new data has been inserted in the compile buffer. Don't
;; assume that only one line has been inserted, typically more than
;; one has changed since the last call due to stdout buffering.
;;
;; We store in a buffer local variable `android-compile-context' a
;; list with 2 elements, the process and point position at the end of
;; the last invocation. The process is used to detect a new
;; compilation. The point position is used to limit our search.
;;
;; On entry (point) is at the end of the last block inserted.
(defun android-compile-filter ()
  "Filter to discard unwanted lines from the compilation buffer.

This filter is registered as a `compilation-filter-hook' and is
called when new data has been inserted in the compile buffer.

Has effect only if `android-compilation-no-buildenv-warning' is
not nil."
  ;; Currently we are looking only for compilation warnings from the
  ;; build env. Move this test lower, near the while loop if we
  ;; support more than one category of regexp.
  (when android-compilation-no-buildenv-warning

    ;; Check if android-compile-context does not exist or if the
    ;; process has changed: new compilation.
    (let ((proc (get-buffer-process (current-buffer))))
      (unless (and (local-variable-p 'android-compile-context)
                   (eq proc (cadr android-compile-context)))
        (setq android-compile-context (list (point-min) proc))
        (make-local-variable 'android-compile-context)))

    (let ((beg (car android-compile-context))
          (end (point)))
      (save-excursion
        (goto-char beg)
        ;; Need to go back at the beginning of the line before we
        ;; start the search: because of the buffering, the previous
        ;; block inserted may have ended in the middle of the
        ;; expression we are trying to match. As result we missed it
        ;; last time and we would miss it again if we started just
        ;; where we left of. By processing the line from the start we
        ;; are catching that case.
        (forward-line 0)
        (while (search-forward-regexp android-compile-ignore-re end t)
          ;; Nuke the line
          (let ((bol (point-at-bol)))
            (forward-line 1)
            (delete-region bol (point)))))
      ;; Remember the new end for next time around.
      (setcar android-compile-context (point)))))

(defun android-compile ()
  "Elisp equivalent of mm shell function.
Walk up the path until a makefile is found and build it.
You need to have a proper buildspec.mk in your top dir.

Use `android-compilation-jobs' to control the number of jobs used
in a compilation."
  (interactive)
  (if (android-project-p)
      (let* ((topdir (android-find-build-tree-root))
             (makefile (android-find-makefile topdir))
             (options
              (concat " -j " (number-to-string android-compilation-jobs))))
        (unless (file-exists-p (concat topdir "buildspec.mk"))
          (error "buildspec.mk missing in %s." topdir))
        ;; Add-hook do not re-add if already present. The compile
        ;; filter hooks run after the comint cleanup (^M).
        (add-hook 'compilation-filter-hook 'android-compile-filter)
        (set (make-local-variable 'compile-command)
             (if (cadr makefile)
                 ;; The root Makefile is not invoked using ONE_SHOT_MAKEFILE.
                 (concat "make -C " topdir options) ; Build the whole image.
               (concat "ONE_SHOT_MAKEFILE=" (car makefile)
                       " make -C " topdir options " files ")))
        (if (interactive-p)
            (call-interactively 'compile)))))

(provide 'android-compile)

;;; android-compile.el ends here

;;; android-host.el --- Module to use host binaries from an Android dev tree.
;;
;; Copyright (C) 2009 The Android Open Source Project
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; This module defines interactive functions to send the most common
;; commands to a device.
;;
;; Currently only one device is supported.
;;
;; In your .emacs load this file (e.g (require 'android-host)) then
;; you can either create new shortcuts e.g:
;;
;;   (global-set-key [f8] 'android-adb-sync)
;;
;; or rely on autocompletion M-x and-sync will expand to
;; M-x  android-adb-sync
;;
;; By default the following key bindings are active:
;; C-x a a android-adb-root
;; C-x a r android-adb-remount
;; C-x a s android-adb-sync
;; C-x a b android-adb-shell-reboot-bootloader
;; C-x a f android-fastboot-flashall
;;
;; android-fastboot-flashall is still work in progress, check the
;; associated buffer (*Android Output*) for errors when you use it.

;;; Code:

(require 'android-common)

(defvar android-host-command-map (make-sparse-keymap))

(defun android-host-key-prefix-set (var val)
  "Bind the keys shortcuts to the functions.i"
  ;; TODO: This should go in a minor mode keymap instead of
  ;; messing with the global one.
  (define-key global-map (read-kbd-macro val) android-host-command-map)
  (custom-set-default var val))

(let ((map android-host-command-map))
  (define-key map (kbd "a") 'android-adb-root)
  (define-key map (kbd "r") 'android-adb-remount)
  (define-key map (kbd "s") 'android-adb-sync)
  (define-key map (kbd "b") 'android-adb-shell-reboot-bootloader)
  (define-key map (kbd "f") 'android-fastboot-flashall))

(defcustom android-host-key-prefix "C-x a"
  "Prefix keystrokes for Android commands."
  :group 'android
  :type 'string
  :set 'android-host-key-prefix-set)

(defun android-adb-remount ()
  "Execute 'adb remount'."
  (interactive)
  (android-adb-command "remount"))

(defun android-adb-root ()
  "Execute 'adb root'."
  (interactive)
  (android-adb-command "root"))

(defun android-adb-shell-reboot-bootloader ()
  "Execute 'adb shell reboot bootloader'."
  (interactive)
  (android-adb-shell-command "reboot bootloader"))

(defun android-adb-sync ()
  "Execute 'adb sync'."
  (interactive)
  ;; Always force root and remount, this way sync always works even on
  ;; a device that has just rebooted or that runs a userdebug build.
  (android-adb-root)
  (android-adb-remount)
  (android-adb-command "sync" 'p))

(defun android-fastboot-sentinel (process event)
  "Called when the fastboot process is done."
  ;; TODO: Should barf if the last lines are not:
  ;;   OKAY
  ;;   rebooting...
  (princ
   (format "Process: %s had the event `%s'" process event)))

(defun android-fastboot-flashall (arg)
  "Execute 'fastboot -p <product> flashall'.

With no ARG, don't wipe the user data.
With ARG, wipe the user data."
  (interactive "P")
  (when (get-buffer android-output-buffer-name)
    (with-current-buffer android-output-buffer-name
      (erase-buffer)))
  (let ((proc
         (if arg
             (start-process-shell-command
              "fastboot"
              android-output-buffer-name
              (concat (android-fastboot) " flashall -w"))
           (start-process-shell-command
            "fastboot" android-output-buffer-name
            (concat (android-fastboot) " flashall")))))
    (set-process-sentinel proc 'android-fastboot-sentinel)))


(provide 'android-host)
;;; android-host.el ends here

;;; Tabbar.el --- Display a tab bar in the header line

;; Copyright (C) 2003, 2004, 2005 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 25 February 2003
;; Keywords: convenience
;; Revision: $Id: tabbar.el,v 1.7 2010/11/22 23:30 m00natic Exp $

(defconst tabbar-version "2.0")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides the Tabbar global minor mode to display a tab
;; bar in the header line of Emacs 21 and later versions.  You can use
;; the mouse to click on a tab and select it.  Also, three buttons are
;; displayed on the left side of the tab bar in this order: the
;; "home", "scroll left", and "scroll right" buttons.  The "home"
;; button is a general purpose button used to change something on the
;; tab bar.  The scroll left and scroll right buttons are used to
;; scroll tabs horizontally.  Tabs can be divided up into groups to
;; maintain several sets of tabs at the same time (see also the
;; chapter "Core" below for more details on tab grouping).  Only one
;; group is displayed on the tab bar, and the "home" button, for
;; example, can be used to navigate through the different groups, to
;; show different tab bars.
;;
;; In a graphic environment, using the mouse is probably the preferred
;; way to work with the tab bar.  However, you can also use the tab
;; bar when Emacs is running on a terminal, so it is possible to use
;; commands to press special buttons, or to navigate cyclically
;; through tabs.
;;
;; These commands, and default keyboard shortcuts, are provided:
;;
;; `tabbar-mode'
;;     Toggle the Tabbar global minor mode.  When enabled a tab bar is
;;     displayed in the header line.
;;
;; `tabbar-local-mode'         (C-c <C-f10>)
;;     Toggle the Tabbar-Local minor mode.  Provided the global minor
;;     mode is turned on, the tab bar becomes local in the current
;;     buffer when the local minor mode is enabled.  This permits to
;;     see the tab bar in a buffer where the header line is already
;;     used by another mode (like `Info-mode' for example).
;;
;; `tabbar-mwheel-mode'
;;     Toggle the Tabbar-Mwheel global minor mode.  When enabled you
;;     can use the mouse wheel to navigate through tabs of groups.
;;
;; `tabbar-press-home'         (C-c <C-home>)
;; `tabbar-press-scroll-left'  (C-c <C-prior>)
;; `tabbar-press-scroll-right' (C-c <C-next>)
;;     Simulate a mouse-1 click on respectively the "home", "scroll
;;     left", and "scroll right" buttons.  A numeric prefix argument
;;     value of 2, or 3, respectively simulates a mouse-2, or mouse-3
;;     click.
;;
;; `tabbar-backward'           (C-c <C-left>)
;; `tabbar-forward'            (C-c <C-right>)
;;     Are the basic commands to navigate cyclically through tabs or
;;     groups of tabs.  The cycle is controlled by the
;;     `tabbar-cycle-scope' option.  The default is to navigate
;;     through all tabs across all existing groups of tabs.  You can
;;     change the default behavior to navigate only through the tabs
;;     visible on the tab bar, or through groups of tabs only.  Or use
;;     the more specialized commands below.
;;
;; `tabbar-backward-tab'
;; `tabbar-forward-tab'
;;     Navigate through the tabs visible on the tab bar.
;;
;; `tabbar-backward-group'     (C-c <C-up>)
;; `tabbar-forward-group'      (C-c <C-down>)
;;     Navigate through existing groups of tabs.
;;
;;
;; Core
;; ----
;;
;; The content of the tab bar is represented by an internal data
;; structure: a tab set.  A tab set is a collection (group) of tabs,
;; identified by an unique name.  In a tab set, at any time, one and
;; only one tab is designated as selected within the tab set.
;;
;; A tab is a simple data structure giving the value of the tab, and a
;; reference to its tab set container.  A tab value can be any Lisp
;; object.  Each tab object is guaranteed to be unique.
;;
;; A tab set is displayed on the tab bar through a "view" defined by
;; the index of the leftmost tab shown.  Thus, it is possible to
;; scroll the tab bar horizontally by changing the start index of the
;; tab set view.
;;
;; The visual representation of a tab bar is a list of valid
;; `header-line-format' template elements, one for each special
;; button, and for each tab found into a tab set "view".  When the
;; visual representation of a tab is required, the function specified
;; in the variable `tabbar-tab-label-function' is called to obtain it.
;; The visual representation of a special button is obtained by
;; calling the function specified in `tabbar-button-label-function',
;; which is passed a button name among `home', `scroll-left', or
;; `scroll-right'.  There are also options and faces to customize the
;; appearance of buttons and tabs (see the code for more details).
;;
;; When the mouse is over a tab, the function specified in
;; `tabbar-help-on-tab-function' is called, which is passed the tab
;; and should return a help string to display.  When a tab is
;; selected, the function specified in `tabbar-select-tab-function' is
;; called, which is passed the tab and the event received.
;;
;; Similarly, to control the behavior of the special buttons, the
;; following variables are available, for respectively the `home',
;; `scroll-left' and `scroll-right' value of `<button>':
;;
;; `tabbar-<button>-function'
;;    Function called when <button> is selected.  The function is
;;    passed the mouse event received.
;;
;; `tabbar-<button>-help-function'
;;    Function called with no arguments to obtain a help string
;;    displayed when the mouse is over <button>.
;;
;; To increase performance, each tab set automatically maintains its
;; visual representation in a cache.  As far as possible, the cache is
;; used to display the tab set, and refreshed only when necessary.
;;
;; Several tab sets can be maintained at the same time.  Only one is
;; displayed on the tab bar, it is obtained by calling the function
;; specified in the variable `tabbar-current-tabset-function'.
;;
;; A special tab set is maintained, that contains the list of the
;; currently selected tabs in the existing tab sets.  This tab set is
;; useful to show the existing tab sets in a tab bar, and switch
;; between them easily.  The function `tabbar-get-tabsets-tabset'
;; returns this special tab set.
;;
;;
;; Buffer tabs
;; -----------
;;
;; The default tab bar implementation provided displays buffers in
;; dedicated tabs.  Selecting a tab, switch (mouse-1), or pop
;; (mouse-2), to the buffer it contains.
;;
;; The list of buffers put in tabs is provided by the function
;; specified in the variable `tabbar-buffer-list-function'.  The
;; default function: `tabbar-buffer-list', excludes buffers whose name
;; starts with a space, when they are not visiting a file.
;;
;; Buffers are organized in groups, each one represented by a tab set.
;; A buffer can have no group, or belong to more than one group.  The
;; function specified by the variable `tabbar-buffer-groups-function'
;; is called for each buffer to obtain the groups it belongs to.  The
;; default function provided: `tabbar-buffer-groups' organizes buffers
;; depending on their major mode (see that function for details).
;;
;; The "home" button toggles display of buffer groups on the tab bar,
;; allowing to easily show another buffer group by clicking on the
;; associated tab.
;;
;; Known problems:
;;
;; Bug item #858306 at <http://sf.net/tracker/?group_id=79309>:
;; tabbar-mode crashes GNU Emacs 21.3 on MS-Windows 98/95.
;;

;;; History:
;;

;;; Code:

;;; Options
;;
(defgroup tabbar nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom tabbar-cycle-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'tabbar
  :type '(choice :tag "Cycle through..."
                 (const :tag "Visible Tabs Only" tabs)
                 (const :tag "Tab Groups Only" groups)
                 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom tabbar-auto-scroll-flag t
  "*Non-nil means to automatically scroll the tab bar.
That is, when a tab is selected outside of the tab bar visible area,
the tab bar is scrolled horizontally so the selected tab becomes
visible."
  :group 'tabbar
  :type 'boolean)

(defvar tabbar-inhibit-functions '(tabbar-default-inhibit-function)
  "List of functions to be called before displaying the tab bar.
Those functions are called one by one, with no arguments, until one of
them returns a non-nil value, and thus, prevents to display the tab
bar.")

(defvar tabbar-current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar tabbar-tab-label-function nil
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string.")

(defvar tabbar-select-tab-function nil
  "Function that select a tab.
The function is passed a mouse event and a tab, and should make it the
selected tab.")

(defvar tabbar-help-on-tab-function nil
  "Function to obtain a help string for a tab.
The help string is displayed when the mouse is onto the button.  The
function is passed the tab and should return a help string or nil for
none.")

(defvar tabbar-button-label-function nil
  "Function that obtains a button label displayed on the tab bar.
The function is passed a button name should return a propertized
string to display.")

(defvar tabbar-home-function nil
  "Function called when clicking on the tab bar home button.
The function is passed the mouse event received.")

(defvar tabbar-home-help-function nil
  "Function to obtain a help string for the tab bar home button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

(defvar tabbar-scroll-left-function 'tabbar-scroll-left
  "Function that scrolls tabs on left.
The function is passed the mouse event received when clicking on the
scroll left button.  It should scroll the current tab set.")

(defvar tabbar-scroll-left-help-function 'tabbar-scroll-left-help
  "Function to obtain a help string for the scroll left button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

(defvar tabbar-scroll-right-function 'tabbar-scroll-right
  "Function that scrolls tabs on right.
The function is passed the mouse event received when clicking on the
scroll right button.  It should scroll the current tab set.")

(defvar tabbar-scroll-right-help-function 'tabbar-scroll-right-help
  "Function to obtain a help string for the scroll right button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

;;; Misc.
;;
(eval-and-compile
  (defalias 'tabbar-display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

(defsubst tabbar-click-p (event)
  "Return non-nil if EVENT is a mouse click event."
  (memq 'click (event-modifiers event)))

(defun tabbar-shorten (str width)
  "Return a shortened string from STR that fits in the given display WIDTH.
WIDTH is specified in terms of character display width in the current
buffer; see also `char-width'.  If STR display width is greater than
WIDTH, STR is truncated and an ellipsis string \"...\" is inserted at
end or in the middle of the returned string, depending on available
room."
  (let* ((n  (length str))
         (sw (string-width str))
         (el "...")
         (ew (string-width el))
         (w  0)
         (i  0))
    (cond
     ;; STR fit in WIDTH, return it.
     ((<= sw width)
      str)
     ;; There isn't enough room for the ellipsis, STR is just
     ;; truncated to fit in WIDTH.
     ((<= width ew)
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (substring str 0 i))
     ;; There isn't enough room to insert the ellipsis in the middle
     ;; of the truncated string, so put the ellipsis at end.
     ((zerop (setq sw (/ (- width ew) 2)))
      (setq width (- width ew))
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (concat (substring str 0 i) el))
     ;; Put the ellipsis in the middle of the truncated string.
     (t
      (while (< w sw)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (setq w (+ w ew))
      (while (< w width)
        (setq n (1- n)
              w (+ w (char-width (aref str n)))))
      (concat (substring str 0 i) el (substring str n)))
     )))

;;; Tab and tab set
;;
(defsubst tabbar-make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst tabbar-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst tabbar-tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar tabbar-tabsets nil
  "The tab sets store.")

(defvar tabbar-tabsets-tabset nil
  "The special tab set of existing tab sets.")

(defvar tabbar-current-tabset nil
  "The tab set currently displayed on the tab bar.")
(make-variable-buffer-local 'tabbar-current-tabset)

(defvar tabbar-init-hook nil
  "Hook run after tab bar data has been initialized.
You should use this hook to initialize dependent data.")

(defsubst tabbar-init-tabsets-store ()
  "Initialize the tab set store."
  (setq tabbar-tabsets (make-vector 31 0)
        tabbar-tabsets-tabset (make-symbol "tabbar-tabsets-tabset"))
  (put tabbar-tabsets-tabset 'start 0)
  (run-hooks 'tabbar-init-hook))

(defvar tabbar-quit-hook nil
  "Hook run after tab bar data has been freed.
You should use this hook to reset dependent data.")

(defsubst tabbar-free-tabsets-store ()
  "Free the tab set store."
  (setq tabbar-tabsets nil
        tabbar-tabsets-tabset nil)
  (run-hooks 'tabbar-quit-hook))

;; Define an "hygienic" function free of side effect between its local
;; variables and those of the callee.
(eval-and-compile
  (defalias 'tabbar-map-tabsets
    (let ((function (make-symbol "function"))
          (result   (make-symbol "result"))
          (tabset   (make-symbol "tabset")))
      `(lambda (,function)
         "Apply FUNCTION to each tab set, and make a list of the results.
The result is a list just as long as the number of existing tab sets."
         (let (,result)
           (if tabbar-tabsets
	       (mapatoms
		#'(lambda (,tabset)
		    (push (funcall ,function ,tabset) ,result))
		tabbar-tabsets))
           ,result)))))

(defun tabbar-make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name tabbar-tabsets))
         (tabs (mapcar #'(lambda (object)
                           (tabbar-make-tab object tabset))
                       objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst tabbar-get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name tabbar-tabsets))

(defsubst tabbar-delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset tabbar-tabsets))

(defsubst tabbar-tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst tabbar-tab-values (tabset)
  "Return the list of tab values in TABSET."
  (mapcar 'tabbar-tab-value (tabbar-tabs tabset)))

(defsubst tabbar-get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (tabbar-tabs tabset)))

(defsubst tabbar-member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (tabbar-tab-tabset tab) tabset)
      (memq tab (tabbar-tabs tabset))))

(defsubst tabbar-template (tabset)
  "Return the cached visual representation of TABSET.
That is, a `header-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst tabbar-set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `header-line-format' template, or nil to
cleanup the cache."
  (put tabset 'template template))

(defsubst tabbar-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst tabbar-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (tabbar-tab-value (tabbar-selected-tab tabset)))

(defsubst tabbar-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (tabbar-selected-tab tabset)))

(defvar tabbar--track-selected nil)

(defsubst tabbar-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (tabbar-member tab tabset)
    (unless (tabbar-selected-p tab tabset)
      (tabbar-set-template tabset nil)
      (setq tabbar--track-selected tabbar-auto-scroll-flag))
    (put tabset 'select tab)))

(defsubst tabbar-select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (tabbar-select-tab (tabbar-get-tab object tabset) tabset))

(defsubst tabbar-start (tabset)
  "Return the index of the first visible tab in TABSET."
  (get tabset 'start))

(defsubst tabbar-view (tabset)
  "Return the list of visible tabs in TABSET.
That is, the sub-list of tabs starting at the first visible one."
  (nthcdr (tabbar-start tabset) (tabbar-tabs tabset)))

(defun tabbar-add-tab (tabset object &optional append)
  "Add to TABSET a tab with value OBJECT if there isn't one there yet.
If the tab is added, it is added at the beginning of the tab list,
unless the optional argument APPEND is non-nil, in which case it is
added at the end."
  (let ((tabs (tabbar-tabs tabset)))
    (if (tabbar-get-tab object tabset)
        tabs
      (let ((tab (tabbar-make-tab object tabset)))
        (tabbar-set-template tabset nil)
        (set tabset (if append
                        (append tabs (list tab))
                      (cons tab tabs)))))))

(defun tabbar-delete-tab (tab)
  "Remove TAB from its tab set."
  (let* ((tabset (tabbar-tab-tabset tab))
         (tabs   (tabbar-tabs tabset))
         (sel    (eq tab (tabbar-selected-tab tabset)))
         (next   (and sel (cdr (memq tab tabs)))))
    (tabbar-set-template tabset nil)
    (setq tabs (delq tab tabs))
    ;; When the selected tab is deleted, select the next one, if
    ;; available, or the last one otherwise.
    (and sel (tabbar-select-tab (car (or next (last tabs))) tabset))
    (set tabset tabs)))

(defun tabbar-scroll (tabset count)
  "Scroll the visible tabs in TABSET of COUNT units.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (tabbar-start tabset) count))
                    (1- (length (tabbar-tabs tabset))))))
    (when (/= start (tabbar-start tabset))
      (tabbar-set-template tabset nil)
      (put tabset 'start start))))

(defun tabbar-tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* (last (tabs (tabbar-tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
            tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

(defun tabbar-current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`tabbar-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (and update tabbar-current-tabset-function
       (setq tabbar-current-tabset
             (funcall tabbar-current-tabset-function)))
  tabbar-current-tabset)

(defun tabbar-get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tab sets."
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab))
  (tabbar-scroll tabbar-tabsets-tabset 0)
  (tabbar-set-template tabbar-tabsets-tabset nil)
  tabbar-tabsets-tabset)

;;; Faces
;;
(defface tabbar-default
  '(
    ;;(((class color grayscale) (background light))
    ;; :inherit variable-pitch
    ;; :height 0.8
    ;; :foreground "gray50"
    ;; :background "grey75"
    ;; )
    (((class color grayscale) (background dark))
     :inherit variable-pitch
     :height 0.8
     :foreground "grey75"
     :background "gray50"
     )
    (((class mono) (background light))
     :inherit variable-pitch
     :height 0.8
     :foreground "black"
     :background "white"
     )
    (((class mono) (background dark))
     :inherit variable-pitch
     :height 0.8
     :foreground "white"
     :background "black"
     )
    (t
     :inherit variable-pitch
     :height 0.8
     :foreground "gray50"
     :background "gray75"
     ))
  "Default face used in the tab bar."
  :group 'tabbar)

(defface tabbar-unselected
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-selected
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style pressed-button)
     :foreground "blue"
     ))
  "Face used for the selected tab."
  :group 'tabbar)

(defface tabbar-highlight
  '((t
     :underline t
     ))
  "Face used to highlight a tab during mouse-overs."
  :group 'tabbar)

(defface tabbar-separator
  '((t
     :inherit tabbar-default
     ))
  "Face used for separators between tabs."
  :group 'tabbar)

(defface tabbar-button
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     ))
  "Face used for tab bar buttons."
  :group 'tabbar)

(defface tabbar-button-highlight
  '((t
     :inherit tabbar-default
     ))
  "Face used to highlight a button during mouse-overs."
  :group 'tabbar)

(defcustom tabbar-background-color nil
  "*Background color of the tab bar.
By default, use the background color specified for the
`tabbar-default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'tabbar
  :type '(choice (const :tag "Default" nil)
                 (color)))

(defsubst tabbar-background-color ()
  "Return the background color of the tab bar."
  (or tabbar-background-color
      (let* ((face 'tabbar-default)
             (color (face-background face)))
        (while (null color)
          (or (facep (setq face (face-attribute face :inherit)))
              (setq face 'default))
          (setq color (face-background face)))
        color)))

;;; Buttons and separator look and feel
;;
(defconst tabbar-button-widget
  '(cons
    (cons :tag "Enabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    (cons :tag "Disabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    )
  "Widget for editing a tab bar button.
A button is specified as a pair (ENABLED-BUTTON . DISABLED-BUTTON),
where ENABLED-BUTTON and DISABLED-BUTTON specify the value used when
the button is respectively enabled and disabled.  Each button value is
a pair (STRING . IMAGE) where STRING is a string value, and IMAGE a
list of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING.
If only the ENABLED-BUTTON image is provided, a DISABLED-BUTTON image
is derived from it.")

;;; Home button
;;
(defvar tabbar-home-button-value nil
  "Value of the home button.")

(defconst tabbar-home-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0
6 0 255 255 255 255 255 255 255 255 255 255 9 130 9 255 255 255 255
255 255 255 255 255 255 26 130 26 255 255 255 255 255 255 255 0 9 26
41 130 41 26 9 0 255 255 255 255 5 145 140 135 130 125 120 115 5 255
255 255 255 0 9 26 41 130 41 26 9 0 255 255 255 255 255 255 255 26 130
26 255 255 255 255 255 255 255 255 255 255 9 130 9 255 255 255 255 255
255 255 255 255 255 0 6 0 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255
"))
  "Default image for the enabled home button.")

(defconst tabbar-home-button-disabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 0 0 1 2 3 2 1 0 0 255 255 255 255 0 132 128 123 119 114 110
106 0 255 255 255 255 0 0 1 2 3 2 1 0 0 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255
"))
  "Default image for the disabled home button.")

(defcustom tabbar-home-button
  (cons (cons "[o]" tabbar-home-button-enabled-image)
        (cons "[x]" tabbar-home-button-disabled-image))
  "The home button.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-home-button-value nil)))

;;; Scroll left button
;;
(defvar tabbar-scroll-left-button-value nil
  "Value of the scroll left button.")

(defconst tabbar-scroll-left-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 128 16 48 255 255 255 255 255 255 255
255 144 28 86 128 0 255 255 255 255 255 255 160 44 92 159 135 113 0
255 255 255 255 160 44 97 165 144 129 120 117 0 255 255 176 44 98 175
174 146 127 126 127 128 0 255 255 0 160 184 156 143 136 134 135 137
138 0 255 255 176 32 67 144 146 144 145 146 148 149 0 255 255 255 255
160 42 75 140 154 158 159 160 0 255 255 255 255 255 255 160 40 74 154
170 171 0 255 255 255 255 255 255 255 255 160 41 82 163 0 255 255 255
255 255 255 255 255 255 255 160 32 48 255 255 255 255 255 255 255 255
255 255 255 255 255 255
"))
  "Default image for the enabled scroll left button.
A disabled button image will be automatically build from it.")

(defcustom tabbar-scroll-left-button
  (cons (cons " <" tabbar-scroll-left-button-enabled-image)
        (cons " =" nil))
  "The scroll left button.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-scroll-left-button-value nil)))

;;; Scroll right button
;;
(defvar tabbar-scroll-right-button-value nil
  "Value of the scroll right button.")

(defconst tabbar-scroll-right-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
48 32 160 255 255 255 255 255 255 255 255 255 255 44 161 71 32 160 255
255 255 255 255 255 255 255 36 157 163 145 62 32 160 255 255 255 255
255 255 30 128 133 137 142 124 50 32 160 255 255 255 255 29 120 121
124 126 126 124 105 42 32 176 255 255 31 126 127 128 128 128 128 126
124 89 32 255 255 33 134 135 136 137 137 138 119 49 32 176 255 255 34
143 144 145 146 128 54 32 160 255 255 255 255 36 152 153 134 57 32 160
255 255 255 255 255 255 38 141 60 32 160 255 255 255 255 255 255 255
255 48 32 160 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255
"))
  "Default image for the enabled scroll right button.
A disabled button image will be automatically build from it.")

(defcustom tabbar-scroll-right-button
  (cons (cons " >" tabbar-scroll-right-button-enabled-image)
        (cons " =" nil))
  "The scroll right button.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-scroll-right-button-value nil)))

;;; Separator
;;
(defconst tabbar-separator-widget
  '(cons (choice (string)
                 (number :tag "Space width" 0.2))
         (repeat :tag "Image"
                 :extra-offset 2
                 (restricted-sexp :tag "Spec"
                                  :match-alternatives (listp))))
  "Widget for editing a tab bar separator.
A separator is specified as a pair (STRING-OR-WIDTH . IMAGE) where
STRING-OR-WIDTH is a string value or a space width, and IMAGE a list
of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING-OR-WIDTH.
The value (\"\"), or (0) hide separators.")

(defvar tabbar-separator-value nil
  "Value of the separator used between tabs.")

(defcustom tabbar-separator (list 0.2)
  "Separator used between tabs.
The variable `tabbar-separator-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-separator-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of separator value.
          (setq tabbar-separator-value nil)))

;;; Images
;;
(defcustom tabbar-use-images t
  "*Non-nil means to try to use images in tab bar.
That is for buttons and separators."
  :group 'tabbar
  :type 'boolean
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of all buttons and separator values.
          (setq tabbar-separator-value nil
                tabbar-home-button-value nil
                tabbar-scroll-left-button-value nil
                tabbar-scroll-right-button-value nil)))

(defsubst tabbar-find-image (specs)
  "Find an image, choosing one of a list of image specifications.
SPECS is a list of image specifications.  See also `find-image'."
  (when (and tabbar-use-images (display-images-p))
    (condition-case nil
        (find-image specs)
      (error nil))))

(defsubst tabbar-disable-image (image)
  "From IMAGE, return a new image which looks disabled."
  (setq image (copy-sequence image))
  (setcdr image (plist-put (cdr image) :conversion 'disabled))
  image)

(defsubst tabbar-normalize-image (image &optional margin)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image."
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (setq plist (plist-put plist :mask '(heuristic t))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

;;; Button keymaps and callbacks
;;
(defun tabbar-make-mouse-keymap (callback)
  "Return a keymap that call CALLBACK on mouse events.
CALLBACK is passed the received mouse event."
  (let ((keymap (make-sparse-keymap)))
    ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line mouse-1] callback)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-2] callback)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-3] callback)
    keymap))

(defsubst tabbar-make-mouse-event (&optional type)
  "Return a mouse click event.
Optional argument TYPE is a mouse-click event or one of the
symbols `mouse-1', `mouse-2' or `mouse-3'.
The default is `mouse-1'."
  (if (tabbar-click-p type)
      type
    (list (or (memq type '(mouse-2 mouse-3)) 'mouse-1)
          (or (event-start nil) ;; Emacs 21.4
              (list (selected-window) (point) '(0 . 0) 0)))))

;;; Buttons
;;
(defconst tabbar-default-button-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-button-callback)
  "Default keymap of a button.")

(defun tabbar-help-on-button (window object position)
  "Return a help string or nil for none, for the button under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the button label under the mouse.
POSITION is the position in that label.
Call `tabbar-NAME-help-function' where NAME is the button name
associated to OBJECT."
  (let* ((name (get-text-property position 'tabbar-button object))
         (funvar (and name
                      (intern-soft (format "tabbar-%s-help-function"
                                           name)))))
    (and (symbol-value funvar)
         (funcall (symbol-value funvar)))))

(defsubst tabbar-click-on-button (name &optional type)
  "Handle a mouse click event on button NAME.
Call `tabbar-select-NAME-function' with the received, or simulated
mouse click event.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let ((funvar (intern-soft (format "tabbar-%s-function" name))))
    (when (symbol-value funvar)
      (funcall (symbol-value funvar) (tabbar-make-mouse-event type))
      (tabbar-display-update))))

(defun tabbar-select-button-callback (event)
  "Handle a mouse EVENT on a button.
Pass mouse click events on a button to `tabbar-click-on-button'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-button
       (get-text-property (cdr target) 'tabbar-button (car target))
       event))))

(defun tabbar-make-button-keymap (name)
  "Return a keymap to handle mouse click events on button NAME."
  (if (fboundp 'posn-string)
      tabbar-default-button-keymap
    (let ((event (make-symbol "event")))
      (tabbar-make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tabbar-click-p ,event)
               (tabbar-click-on-button ',name ,event)))))))

;;; Button callbacks
;;
(defun tabbar-scroll-left (event)
  "On mouse EVENT, scroll current tab set on left."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) -1)))

(defun tabbar-scroll-left-help ()
  "Help string shown when mouse is over the scroll left button."
  "mouse-1: scroll tabs left.")

(defun tabbar-scroll-right (event)
  "On mouse EVENT, scroll current tab set on right."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) 1)))

(defun tabbar-scroll-right-help ()
  "Help string shown when mouse is over the scroll right button."
  "mouse-1: scroll tabs right.")

;;; Tabs
;;
(defconst tabbar-default-tab-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-tab-callback)
  "Default keymap of a tab.")

(defun tabbar-help-on-tab (window object position)
  "Return a help string or nil for none, for the tab under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the tab label under the mouse.
POSITION is the position in that label.
Call `tabbar-help-on-tab-function' with the associated tab."
  (when tabbar-help-on-tab-function
    (let ((tab (get-text-property position 'tabbar-tab object)))
      (funcall tabbar-help-on-tab-function tab))))

(defsubst tabbar-click-on-tab (tab &optional type)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (when tabbar-select-tab-function
    (funcall tabbar-select-tab-function
             (tabbar-make-mouse-event type) tab)
    (tabbar-display-update)))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-tab
       (get-text-property (cdr target) 'tabbar-tab (car target))
       event))))

(defun tabbar-make-tab-keymap (tab)
  "Return a keymap to handle mouse click events on TAB."
  (if (fboundp 'posn-string)
      tabbar-default-tab-keymap
    (let ((event (make-symbol "event")))
      (tabbar-make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tabbar-click-p ,event)
               (tabbar-click-on-tab ',tab ,event)))))))

;;; Tab bar construction
;;
(defun tabbar-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-NAME-button'."
  (let* ((btn (symbol-value
               (intern-soft (format "tabbar-%s-button" name))))
         (on  (tabbar-find-image (cdar btn)))
         (off (and on (tabbar-find-image (cddr btn)))))
    (when on
      (tabbar-normalize-image on 1)
      (if off
          (tabbar-normalize-image off 1)
        ;; If there is no disabled button image, derive one from the
        ;; button enabled image.
        (setq off (tabbar-disable-image on))))
    (cons
     (propertize (or (caar btn) " ") 'display on)
     (propertize (or (cadr btn) " ") 'display off))))

(defun tabbar-line-button (name)
  "Return the display representation of button NAME.
That is, a propertized string used as an `header-line-format' template
element."
  (let ((label (if tabbar-button-label-function
                   (funcall tabbar-button-label-function name)
                 (cons name name))))
    ;; Cache the display value of the enabled/disabled buttons in
    ;; variables `tabbar-NAME-button-value'.
    (set (intern (format "tabbar-%s-button-value"  name))
         (cons
          (propertize (car label)
                      'tabbar-button name
                      'face 'tabbar-button
                      'mouse-face 'tabbar-button-highlight
                      'pointer 'hand
                      'local-map (tabbar-make-button-keymap name)
                      'help-echo 'tabbar-help-on-button)
          (propertize (cdr label)
                      'face 'tabbar-button
                      'pointer 'arrow)))))

(defun tabbar-line-separator ()
  "Return the display representation of a tab bar separator.
That is, a propertized string used as an `header-line-format' template
element."
  (let ((image (tabbar-find-image (cdr tabbar-separator))))
    ;; Cache the separator display value in variable
    ;; `tabbar-separator-value'.
    (setq tabbar-separator-value
          (cond
           (image
            (propertize " "
                        'face 'tabbar-separator
                        'pointer 'arrow
                        'display (tabbar-normalize-image image)))
           ((numberp (car tabbar-separator))
            (propertize " "
                        'face 'tabbar-separator
                        'pointer 'arrow
                        'display (list 'space
                                       :width (car tabbar-separator))))
           ((propertize (or (car tabbar-separator) " ")
                        'face 'tabbar-separator
                        'pointer 'arrow))))
    ))

(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list
   (if tabbar-home-function
       (car tabbar-home-button-value)
     (cdr tabbar-home-button-value))
   (if (> (tabbar-start tabset) 0)
       (car tabbar-scroll-left-button-value)
     (cdr tabbar-scroll-left-button-value))
   (if (< (tabbar-start tabset)
          (1- (length (tabbar-tabs tabset))))
       (car tabbar-scroll-right-button-value)
     (cdr tabbar-scroll-right-button-value))
   tabbar-separator-value))

(defsubst tabbar-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (concat (propertize
           (if tabbar-tab-label-function
               (funcall tabbar-tab-label-function tab)
             tab)
           'tabbar-tab tab
           'local-map (tabbar-make-tab-keymap tab)
           'help-echo 'tabbar-help-on-tab
           'mouse-face 'tabbar-highlight
           'face (if (tabbar-selected-p tab (tabbar-current-tabset))
                     'tabbar-selected
                   'tabbar-unselected)
           'pointer 'hand)
          tabbar-separator-value))

(defun tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
         atsel elts)
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
              atsel (eq (car tabs) sel)
              tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (tabbar-line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (tabbar-set-template
     tabset
     (list (tabbar-line-buttons tabset)
           (nreverse elts)
           (propertize "%-"
                       'face (list :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow)))
    ))

(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window if any of the
`tabbar-inhibit-functions' return non-nil."
  (cond
   ((run-hook-with-args-until-success 'tabbar-inhibit-functions)
    ;; Don't show the tab bar.
    (setq header-line-format nil))
   ((tabbar-current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (tabbar-template tabbar-current-tabset)
        (tabbar-line-format tabbar-current-tabset)))))

(defconst tabbar-header-line-format '(:eval (tabbar-line))
  "The tab bar header line format.")

(defun tabbar-default-inhibit-function ()
  "Inhibit display of the tab bar in specified windows.
That is dedicated windows, and `checkdoc' status windows."
  (or (window-dedicated-p (selected-window))
      (member (buffer-name)
              (list " *Checkdoc Status*"
                    (if (boundp 'ispell-choices-buffer)
                        ispell-choices-buffer
                      "*Choices*")))))

;;; Cyclic navigation through tabs
;;
(defun tabbar-cycle (&optional backward type)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `tabbar-cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead.
Optional argument TYPE is a mouse event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((tabset (tabbar-current-tabset t))
         (ttabset (tabbar-get-tabsets-tabset))
         ;; If navigation through groups is requested, and there is
         ;; only one group, navigate through visible tabs.
         (cycle (if (and (eq tabbar-cycle-scope 'groups)
                         (not (cdr (tabbar-tabs ttabset))))
                    'tabs
                  tabbar-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (tabbar-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
        (setq tab (tabbar-tab-next ttabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (tabbar-tabs ttabset)
                tab (car (if backward (last tabset) tabset))))
        )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tab (tabbar-tab-next ttabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (tabbar-tabs ttabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (tabbar-tabs (tabbar-tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (tabbar-click-on-tab tab type))))

;;;###autoload
(defun tabbar-backward ()
  "Select the previous available tab.
Depend on the setting of the option `tabbar-cycle-scope'."
  (interactive)
  (tabbar-cycle t))

;;;###autoload
(defun tabbar-forward ()
  "Select the next available tab.
Depend on the setting of the option `tabbar-cycle-scope'."
  (interactive)
  (tabbar-cycle))

;;;###autoload
(defun tabbar-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle t)))

;;;###autoload
(defun tabbar-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle)))

;;;###autoload
(defun tabbar-backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle t)))

;;;###autoload
(defun tabbar-forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle)))

;;; Button press commands
;;
(defsubst tabbar--mouse (number)
  "Return a mouse button symbol from NUMBER.
That is mouse-2, or mouse-3 when NUMBER is respectively 2, or 3.
Return mouse-1 otherwise."
  (cond ((eq number 2) 'mouse-2)
        ((eq number 3) 'mouse-3)
        ('mouse-1)))

;;;###autoload
(defun tabbar-press-home (&optional arg)
  "Press the tab bar home button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tabbar-click-on-button 'home (tabbar--mouse arg)))

;;;###autoload
(defun tabbar-press-scroll-left (&optional arg)
  "Press the tab bar scroll-left button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tabbar-click-on-button 'scroll-left (tabbar--mouse arg)))

;;;###autoload
(defun tabbar-press-scroll-right (&optional arg)
  "Press the tab bar scroll-right button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tabbar-click-on-button 'scroll-right (tabbar--mouse arg)))

;;; Mouse-wheel support
;;
(require 'mwheel)

;;; Compatibility
;;
(defconst tabbar--mwheel-up-event
  (symbol-value (if (boundp 'mouse-wheel-up-event)
                    'mouse-wheel-up-event
                  'mouse-wheel-up-button)))

(defconst tabbar--mwheel-down-event
  (symbol-value (if (boundp 'mouse-wheel-down-event)
                    'mouse-wheel-down-event
                  'mouse-wheel-down-button)))

(defsubst tabbar--mwheel-key (event-type)
  "Return a mouse wheel key symbol from EVENT-TYPE.
When EVENT-TYPE is a symbol return it.
When it is a button number, return symbol `mouse-<EVENT-TYPE>'."
  (if (symbolp event-type)
      event-type
    (intern (format "mouse-%s" event-type))))

(defsubst tabbar--mwheel-up-p (event)
  "Return non-nil if EVENT is a mouse-wheel up event."
  (let ((x (event-basic-type event)))
    (if (eq 'mouse-wheel x)
        (< (car (cdr (cdr event))) 0)   ;; Emacs 21.3
      ;; Emacs > 21.3
      (eq x tabbar--mwheel-up-event))))

;;; Basic commands
;;
;;;###autoload
(defun tabbar-mwheel-backward (event)
  "Select the previous available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward'."
  (interactive "@e")
  (tabbar-cycle t event))

;;;###autoload
(defun tabbar-mwheel-forward (event)
  "Select the next available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward'."
  (interactive "@e")
  (tabbar-cycle nil event))

;;;###autoload
(defun tabbar-mwheel-backward-group (event)
  "Go to selected tab in the previous available group.
If there is only one group, select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-group'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle t event)))

;;;###autoload
(defun tabbar-mwheel-forward-group (event)
  "Go to selected tab in the next available group.
If there is only one group, select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-group'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle nil event)))

;;;###autoload
(defun tabbar-mwheel-backward-tab (event)
  "Select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-tab'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle t event)))

;;;###autoload
(defun tabbar-mwheel-forward-tab (event)
  "Select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-tab'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle nil event)))

;;; Wrappers when there is only one generic mouse-wheel event
;;
;;;###autoload
(defun tabbar-mwheel-switch-tab (event)
  "Select the next or previous tab according to EVENT."
  (interactive "@e")
  (if (tabbar--mwheel-up-p event)
      (tabbar-mwheel-forward-tab event)
    (tabbar-mwheel-backward-tab event)))

;;;###autoload
(defun tabbar-mwheel-switch-group (event)
  "Select the next or previous group of tabs according to EVENT."
  (interactive "@e")
  (if (tabbar--mwheel-up-p event)
      (tabbar-mwheel-forward-group event)
    (tabbar-mwheel-backward-group event)))

;;; Minor modes
;;
(defsubst tabbar-mode-on-p ()
  "Return non-nil if Tabbar mode is on."
  (eq (default-value 'header-line-format)
      tabbar-header-line-format))

;;; Tabbar-Local mode
;;
(defvar tabbar--local-hlf nil)

;;;###autoload
(define-minor-mode tabbar-local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Tabbar mode is off."
  :group 'tabbar
  :global nil
  (unless (tabbar-mode-on-p)
    (error "Tabbar mode must be enabled"))
;;; ON
  (if tabbar-local-mode
      (if (and (local-variable-p 'header-line-format)
               header-line-format)
          ;; A local header line exists, hide it to show the tab bar.
          (progn
            ;; Fail in case of an inconsistency because another local
            ;; header line is already hidden.
            (when (local-variable-p 'tabbar--local-hlf)
              (error "Another local header line is already hidden"))
            (set (make-local-variable 'tabbar--local-hlf)
                 header-line-format)
            (kill-local-variable 'header-line-format))
        ;; Otherwise hide the tab bar in this buffer.
        (setq header-line-format nil))
;;; OFF
    (if (local-variable-p 'tabbar--local-hlf)
        ;; A local header line is hidden, show it again.
        (progn
          (setq header-line-format tabbar--local-hlf)
          (kill-local-variable 'tabbar--local-hlf))
      ;; The tab bar is locally hidden, show it again.
      (kill-local-variable 'header-line-format))))

;;; Tabbar mode
;;
(defvar tabbar-prefix-key [(control ?c)]
  "The common prefix key used in Tabbar mode.")

(defvar tabbar-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km [(control home)]  'tabbar-press-home)
    (define-key km [(control left)]  'tabbar-backward)
    (define-key km [(control right)] 'tabbar-forward)
    (define-key km [(control up)]    'tabbar-backward-group)
    (define-key km [(control down)]  'tabbar-forward-group)
    (define-key km [(control prior)] 'tabbar-press-scroll-left)
    (define-key km [(control next)]  'tabbar-press-scroll-right)
    (define-key km [(control f10)]   'tabbar-local-mode)
    km)
  "The key bindings provided in Tabbar mode.")

(defvar tabbar-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km tabbar-prefix-key tabbar-prefix-map)
    km)
  "Keymap to use in  Tabbar mode.")

(defvar tabbar--global-hlf nil)

;;;###autoload
(define-minor-mode tabbar-mode
  "Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mode-map}"
  :group 'tabbar
  :require 'tabbar
  :global t
  :keymap tabbar-mode-map
  (if tabbar-mode
;;; ON
      (unless (tabbar-mode-on-p)
        ;; Save current default value of `header-line-format'.
        (setq tabbar--global-hlf (default-value 'header-line-format))
        (tabbar-init-tabsets-store)
        (setq-default header-line-format tabbar-header-line-format)
	(if (fboundp 'tabbar-define-access-keys) (tabbar-define-access-keys)))
;;; OFF
    (when (tabbar-mode-on-p)
      ;; Turn off Tabbar-Local mode globally.
      (mapc #'(lambda (b)
                (condition-case nil
                    (with-current-buffer b
                      (and tabbar-local-mode
                           (tabbar-local-mode -1)))
                  (error nil)))
            (buffer-list))
      ;; Restore previous `header-line-format'.
      (setq-default header-line-format tabbar--global-hlf)
      (tabbar-free-tabsets-store))
    ))

;;; Tabbar-Mwheel mode
;;
(defvar tabbar-mwheel-mode-map
  (let ((km (make-sparse-keymap)))
    (if (get 'mouse-wheel 'event-symbol-elements)
        ;; Use one generic mouse wheel event
        (define-key km [A-mouse-wheel]
          'tabbar-mwheel-switch-group)
      ;; Use separate up/down mouse wheel events
      (let ((up   (tabbar--mwheel-key tabbar--mwheel-up-event))
            (down (tabbar--mwheel-key tabbar--mwheel-down-event)))
        (define-key km `[header-line ,down]
          'tabbar-mwheel-backward-group)
        (define-key km `[header-line ,up]
          'tabbar-mwheel-forward-group)
        (define-key km `[header-line (control ,down)]
          'tabbar-mwheel-backward-tab)
        (define-key km `[header-line (control ,up)]
          'tabbar-mwheel-forward-tab)
        (define-key km `[header-line (shift ,down)]
          'tabbar-mwheel-backward)
        (define-key km `[header-line (shift ,up)]
          'tabbar-mwheel-forward)
        ))
    km)
  "Keymap to use in Tabbar-Mwheel mode.")

;;;###autoload
(define-minor-mode tabbar-mwheel-mode
  "Toggle use of the mouse wheel to navigate through tabs or groups.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mwheel-mode-map}"
  :group 'tabbar
  :require 'tabbar
  :global t
  :keymap tabbar-mwheel-mode-map
  (when tabbar-mwheel-mode
    (unless (and mouse-wheel-mode tabbar-mode)
      (tabbar-mwheel-mode -1))))

(defun tabbar-mwheel-follow ()
  "Toggle Tabbar-Mwheel following Tabbar and Mouse-Wheel modes."
  (tabbar-mwheel-mode (if (and mouse-wheel-mode tabbar-mode) 1 -1)))

(add-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
(add-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

;;; Buffer tabs
;;
(defgroup tabbar-buffer nil
  "Display buffers in the tab bar."
  :group 'tabbar)

(defcustom tabbar-buffer-home-button
  (cons (cons "[+]" tabbar-home-button-enabled-image)
        (cons "[-]" tabbar-home-button-disabled-image))
  "The home button displayed when showing buffer tabs.
The enabled button value is displayed when showing tabs for groups of
buffers, and the disabled button value is displayed when showing
buffer tabs.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar-buffer
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-home-button-value nil)))

(defvar tabbar-buffer-list-function 'tabbar-buffer-list
  "Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers.")

(defvar tabbar-buffer-groups-function 'tabbar-buffer-groups
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group.")

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(defun tabbar-buffer-mode-derived-p (mode parents)
  "Return non-nil if MODE derives from a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
          (setq derived t)
        (setq mode (get mode 'derived-mode-parent))))
    derived))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*"))
     "Common"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

;;; Group buffers in tab sets.
;;
(defvar tabbar--buffers nil)

(defun tabbar-buffer-update-groups ()
  "Update tab sets from groups of existing buffers.
Return the the first group where the current buffer is."
  (let ((bl (sort
             (mapcar
	      ;; for each buffer, create list: buffer, buffer name, groups-list
	      ;; sort on buffer name; store to bl (buffer list)
              #'(lambda (b)
                  (with-current-buffer b
                    (list (current-buffer)
                          (buffer-name)
                          (if tabbar-buffer-groups-function
                              (funcall tabbar-buffer-groups-function)
                            '("Common")))))
              (and tabbar-buffer-list-function
                   (funcall tabbar-buffer-list-function)))
             #'(lambda (e1 e2)
                 (string-lessp (nth 1 e1) (nth 1 e2))))))
    ;; If the cache has changed, update the tab sets.
    (unless (equal bl tabbar--buffers)
      ;; Add new buffers, or update changed ones.
      (dolist (e bl) ;; loop through buffer list
        (dolist (g (nth 2 e)) ;; for each member of groups-list for current buffer
          (let ((tabset (tabbar-get-tabset g))) ;; get group from group name
            (if tabset ;; if group exists
		;; check if current buffer is same as any cached buffer
		;; (search buffer list for matching buffer)
                (unless (equal e (assq (car e) tabbar--buffers)) ;; if not,...
                  ;; This is a new buffer, or a previously existing
                  ;; buffer that has been renamed, or moved to another
                  ;; group.  Update the tab set, and the display.
                  (tabbar-add-tab tabset (car e) t) ;; add to end of tabset
                  (tabbar-set-template tabset nil))
	      ;;if tabset doesn't exist, make a new tabset with this buffer
              (tabbar-make-tabset g (car e))))))
      ;; Remove tabs for buffers not found in cache or moved to other
      ;; groups, and remove empty tabsets.
      (mapc 'tabbar-delete-tabset ;; delete each tabset named in following list:
            (tabbar-map-tabsets ;; apply following function to each tabset:
             #'(lambda (tabset)
                 (dolist (tab (tabbar-tabs tabset)) ;; for each tab in tabset
                   (let ((e (assq (tabbar-tab-value tab) bl))) ;; get buffer
                     (or (and e (memq tabset ;; skip if buffer exists and tabset is a member of groups-list for this buffer
                                      (mapcar 'tabbar-get-tabset
                                              (nth 2 e))))
                         (tabbar-delete-tab tab)))) ;; else remove tab from this set
                 ;; Return empty tab sets
                 (unless (tabbar-tabs tabset)
                   tabset)))) ;; return list of tabsets, replacing non-empties with nil
      ;; The new cache becomes the current one.
      (setq tabbar--buffers bl)))
  ;; Return the first group the current buffer belongs to.
  (car (nth 2 (assq (current-buffer) tabbar--buffers))))

;;; Tab bar callbacks
;;
(defvar tabbar--buffer-show-groups nil)

(defsubst tabbar-buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq tabbar--buffer-show-groups flag
        ;; Redisplay the home button.
        tabbar-home-button-value nil))

(defun tabbar-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (tabbar-get-tabset (tabbar-buffer-update-groups))))
    (tabbar-select-tab-value (current-buffer) tabset)
    (when tabbar--buffer-show-groups
      (setq tabset (tabbar-get-tabsets-tabset))
      (tabbar-select-tab-value (current-buffer) tabset))
    tabset))

(defun tabbar-buffer-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-button-label'.
When NAME is 'home, return a different ENABLED button if showing tabs
or groups.  Call the function `tabbar-button-label' otherwise."
  (let ((lab (tabbar-button-label name)))
    (when (eq name 'home)
      (let* ((btn tabbar-buffer-home-button)
             (on  (tabbar-find-image (cdar btn)))
             (off (tabbar-find-image (cddr btn))))
        ;; When `tabbar-buffer-home-button' does not provide a value,
        ;; default to the enabled value of `tabbar-home-button'.
        (if on
            (tabbar-normalize-image on 1)
          (setq on (get-text-property 0 'display (car lab))))
        (if off
            (tabbar-normalize-image off 1)
          (setq off (get-text-property 0 'display (car lab))))
        (setcar lab
                (if tabbar--buffer-show-groups
                    (propertize (or (caar btn) (car lab)) 'display on)
                  (propertize (or (cadr btn) (car lab)) 'display off)))
        ))
    lab))

(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]" (tabbar-tab-tabset tab))
                  (format "%s" (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "mouse-1: switch to buffer %S\n\
mouse-2: pop to buffer, mouse-3: delete other windows"
            (buffer-name (tabbar-tab-value tab)))
    ))

(defun tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (pop-to-buffer buffer t))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)
    ))

(defun tabbar-buffer-click-on-home (event)
  "Handle a mouse click EVENT on the tab bar home button.
mouse-1, toggle the display of tabs for groups of buffers.
mouse-3, close the current buffer."
  (let ((mouse-button (event-basic-type event)))
    (cond
     ((eq mouse-button 'mouse-1)
      (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
     ((eq mouse-button 'mouse-3)
      (kill-buffer nil))
     )))

(defun tabbar-buffer-help-on-home ()
  "Return the help string shown when mouse is onto the toggle button."
  (concat
   (if tabbar--buffer-show-groups
       "mouse-1: show buffers in selected group"
     "mouse-1: show groups of buffers")
   ", mouse-3: close current buffer"))

(defun tabbar-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Tabbar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq header-line-format tabbar-header-line-format)
       (eq tabbar-current-tabset-function 'tabbar-buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (tabbar-tab-values (tabbar-current-tabset)))
             (b  (current-buffer))
             found sibling)
         (while (and bl (not found))
           (if (eq b (car bl))
               (setq found t)
             (setq sibling (car bl)))
           (setq bl (cdr bl)))
         (when (and (setq sibling (or (car bl) sibling))
                    (buffer-live-p sibling))
           ;; Move sibling buffer in front of the buffer list.
           (save-current-buffer
             (switch-to-buffer sibling))))))

					;(add-hook 'c-mode-common-hook 'google-set-c-style)
					;(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'linux-set-c-style)
(ido-mode)
(require 'xcscope)

(load-file "~/bin/cedet-1.0pre7/common/cedet.el")
(require 'cedet)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
(hl-line-mode t)