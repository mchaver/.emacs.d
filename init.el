;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Emacs configuration using use-package

;;; Code:

;; ============================================================================
;; Package Management
;; ============================================================================

(require 'package)

;; Use HTTPS for security
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Always install packages if not present

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun safe-add-to-load-path (dirname)
  "Add a dir to load path if it is readable, otherwise log to the *Messages* buffer."
  (if (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)
    (message "Unable to read the path: %s" dirname)))

(defun safe-load (filename)
  "Load a file if it readable, otherwise log to the *Messages* buffer."
  (if (file-readable-p filename)
      (load filename)
    (message "Unable to read the file: %s" filename)))

;; Load custom init files
(safe-add-to-load-path "~/.emacs.d/init")
(safe-add-to-load-path "~/.emacs.d/init/tla-mode")
(safe-load "~/.emacs.d/init/autopair")
(safe-load "~/.emacs.d/init/rgbds-mode.el")

;; ============================================================================
;; Basic Settings
;; ============================================================================

(setq user-full-name "J.H."
      user-mail-address "mchaver@gmail.com")

;; Environment PATH
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/usr/local/share/npm/bin:Users/mchaver/.cargo/bin:" (getenv "PATH")))

;; Disable splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Text selection and clipboard
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Show empty lines and end of file
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Case insensitive search
(setq case-fold-search t)

;; Column number mode
(setq column-number-mode t)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Turn off bell sound
(setq ring-bell-function 'ignore)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Continuous scrolling in doc-view
(setq doc-view-continuous t)

;; TeX input method
(setq default-input-method 'TeX)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; Performance settings to prevent crashes with large files
(setq read-process-output-max (* 1024 1024))  ; 1MB (default is 4KB)
(setq process-adaptive-read-buffering nil)     ; Disable adaptive buffering

;; Large file handling - disable features that slow down large files
(defvar large-file-threshold (* 5 1024 1024)  ; 5MB threshold
  "Size in bytes above which a file is considered large.")

(defun check-large-file ()
  "If a file is over a certain size, disable features that slow it down."
  (when (and buffer-file-name
             (> (buffer-size) large-file-threshold))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (message "Large file detected (%s bytes). Read-only mode enabled, undo disabled, syntax highlighting off."
             (buffer-size))))

(add-hook 'find-file-hook 'check-large-file)

;; Increase limits for long lines to prevent freezing
(setq-default bidi-display-reordering nil)  ; Disable bidirectional text (helps with long lines)
(setq bidi-inhibit-bpa t)                    ; Emacs 27+ - more bidi optimizations
(setq-default long-line-threshold 1000)      ; Warn about lines longer than this
(setq large-hscroll-threshold 1000)          ; Avoid slow scrolling with long lines

;; Increase max lisp eval depth to prevent errors with deeply nested structures
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; ============================================================================
;; Packages
;; ============================================================================

;; Zenburn theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Smex - M-x enhancement with search history
(use-package smex
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Ido - file system navigation
(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window)
  ;; Stop ido from suggesting when naming new file
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil))

;; YAML mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;; Markdown mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; JSON mode
(use-package json-mode
  :mode "\\.json\\'"
  :config
  ;; Safe JSON pretty-print that checks size first
  (defun safe-json-pretty-print-buffer ()
    "Pretty print JSON buffer with size check to prevent crashes."
    (interactive)
    (let ((size (buffer-size)))
      (if (> size (* 512 1024))  ; 512KB threshold
          (if (yes-or-no-p (format "Buffer is %d bytes. This may be slow or crash. Continue? " size))
              (json-pretty-print-buffer)
            (message "JSON pretty-print cancelled."))
        (json-pretty-print-buffer))))

  (defun safe-json-pretty-print (beg end)
    "Pretty print JSON region with size check to prevent crashes."
    (interactive "r")
    (let ((size (- end beg)))
      (if (> size (* 512 1024))  ; 512KB threshold
          (if (yes-or-no-p (format "Region is %d bytes. This may be slow or crash. Continue? " size))
              (json-pretty-print beg end)
            (message "JSON pretty-print cancelled."))
        (json-pretty-print beg end))))

  :bind (:map json-mode-map
              ("C-c C-f" . safe-json-pretty-print-buffer)))

;; Web mode for HTML
(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

;; JavaScript mode
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

;; JSX mode
(use-package rjsx-mode
  :mode "\\.jsx\\'")

;; Haskell mode - simple syntax highlighting
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  ;; Basic indentation settings
  (setq haskell-indentation-layout-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-starter-offset 2))

;; Rust mode
(use-package rust-mode
  :mode "\\.rs\\'")

;; Tuareg for OCaml
(use-package tuareg
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)))

;; Flycheck for syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Deft for note-taking
(use-package deft
  :commands deft
  :config
  (setq deft-directory "~/deft"
        deft-use-filename-as-title t
        deft-extension "org"
        deft-text-mode 'org-mode))

;; Ripgrep integration
(use-package rg
  :config
  (rg-enable-default-bindings))

;; Org mode
(use-package org
  :config
  (setq org-log-done t
        calendar-week-start-day 1)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)))

;; Auto-complete
(use-package auto-complete
  :config
  (ac-config-default))

;; AC Slime
(use-package ac-slime
  :after (auto-complete slime)
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))

;; Mwim - Move Where I Mean
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Deferred execution
(use-package deferred)

;; RGBDS mode (custom mode)
(with-eval-after-load 'rgbds-mode
  (require 'rgbds-mode))

;; ============================================================================
;; Custom Functions
;; ============================================================================

(defun insert-line-above ()
  "Insert an empty line above the current line. Position the cursor at its beginning."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun insert-before-line ()
  "Insert yanked text before the current line."
  (interactive)
  (let ((pos (point))
        (cur-max (point-max)))
    (beginning-of-line)
    (yank)
    (indent-according-to-mode)
    (newline-and-indent)
    (goto-char (+ pos (- (point-max) cur-max)))))

(defun newline-without-break-of-line ()
  "Move to the end of the line. Insert a new line."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                  (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun insert-date (prefix)
  "Insert the current date.
Without prefix-argument, use ISO format.
With one prefix-argument, use standard US format.
With two prefix arguments, write out the day and month name.
With three prefix arguments, use standard European format."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4))  "%m/%d/%Y")
                 ((equal prefix '(16)) "%A, %d %B %Y")
                 ((equal prefix '(64))  "%d.%m.%Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer and copy to kill ring."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
Does not push text to the kill-ring."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
Does not push text to the kill-ring."
  (interactive "p")
  (delete-word (- arg)))

(defun delete-line ()
  "Delete text from current position to end of line char.
Does not push text to the kill-ring."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun backward-delete-line ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to the kill-ring."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (require 'cl-lib)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-ido-buffers ()
  "Kill ido virtual buffers and clear recent file list."
  (interactive)
  (setq ido-virtual-buffers '())
  (setq recentf-list '()))

(defun copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path. Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the current or marked files.
If a buffer is not file and not dired, copy value of `default-directory'."
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory)
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath)))))

;; ============================================================================
;; Key Bindings
;; ============================================================================

;; Comment/uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Line manipulation
(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)
(global-set-key (kbd "<S-return>") 'smart-open-line)
(global-set-key (kbd "<C-S-return>") 'insert-line-above)
(global-set-key (kbd "C-<up>") 'move-line-up)
(global-set-key (kbd "C-<down>") 'move-line-down)

;; Window navigation
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x TAB") 'other-frame)

;; Window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Deletion without kill-ring
(global-set-key (kbd "C-S-k") 'backward-delete-line)
(global-set-key (kbd "M-k") 'delete-line)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)

;; Utility functions
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "M-\"") 'insert-pair)

;; Neotree
(global-set-key [f8] 'neotree-toggle)

;; ============================================================================
;; Mode-specific Settings
;; ============================================================================

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))

;; Gradle files
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;; ============================================================================
;; Hooks
;; ============================================================================

;; Remove killed buffers from ido history
(require 'cl-lib)
(add-hook 'kill-buffer-hook
          (lambda ()
            (setq buffer-name-history
                  (cl-delete (buffer-name) buffer-name-history :test 'string=))))

;; ============================================================================
;; UI Settings
;; ============================================================================

;; Font
;; Set height first, then set-frame-font to get the correct size
(set-face-attribute 'default nil :height 100)
(set-frame-font "DejaVu Sans Mono:size=14" nil t)

;; Disable mouse
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; ============================================================================
;; Custom Variables
;; ============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme yaml-mode web-mode use-package tuareg smex rust-mode rjsx-mode rg org mwim markdown-mode json-mode js2-mode haskell-mode flycheck deft deferred ac-slime))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ============================================================================
;; Performance - Reset GC threshold
;; ============================================================================

;; Reset garbage collection threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; Enable server mode for emacsclient
;; (server-start)

;; Enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(provide 'init)
;;; init.el ends here
