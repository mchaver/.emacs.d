;; -*- mode: elisp -*-

;; structure of an elisp function
;; (defun function-name (arguments...)
;;        "optional-documentation..."
;;        (interactive argument-passing-info)     ; optional
;;        body...)

(setq gc-cons-threshold (* 50 1000 1000))

;; Enable package.el for some packages that don't work with straight.el
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

;; turn off bell function
(setq ring-bell-function 'ignore)

;; helpful functions

(defun safe-add-to-load-path (dirname)
  "Add a dir to load path if it is readable, otherwise log to the *Message* buffer that it is not readable."
  (if (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)
    (message "Unable to read the path: %s" dirname)))

(defun safe-load (filename)
  "Load a file if it readable, otherwise log to the *Message* buffer that it is not readable."
  (if (file-readable-p filename)
      (load filename)
    (message "Unable to read the file: %s" filename)))

;; load init files
(safe-add-to-load-path "~/.emacs.d/init")
(safe-load "~/.emacs.d/init/verilog-mode.el")
(safe-load "~/.emacs.d/init/autopair")
(safe-load "~/.emacs.d/init/rgbds-mode.el")

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/usr/local/share/npm/bin:/Users/mchaver/.cargo/bin:" (getenv "PATH")))

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; Package declarations
;; Theme
(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))

;; Utilities
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package deferred :straight t)

;; Navigation and completion
(use-package helm
  :straight t
  :init
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-ff-auto-update-initial-value t)
  :config
  (helm-mode 1)
  (define-key helm-find-files-map (kbd "<C-backspace>") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-DEL") 'helm-find-files-up-one-level)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)))

(use-package projectile :straight t)

(use-package neotree
  :straight t
  :bind ("M-0" . neotree-toggle))

;; Editing
(use-package mwim :straight t)
(use-package auto-complete :straight t)
(use-package flycheck :straight t)
(use-package goto-chg
  :straight t
  :bind (("C-x C-/" . goto-last-change)
         ("C-x C-?" . goto-last-change-reverse)))

;; Search
(use-package deadgrep :straight t)
(use-package rg
  :straight nil  ;; Use package.el instead
  :ensure t
  :defer t
  :commands (rg rg-dwim rg-project))

;; Notes and organization
(use-package deft
  :straight t
  :config
  (setq deft-directory "~/deft"
        deft-use-filename-as-title t
        deft-extension "org"
        deft-text-mode 'org-mode))

;; Org-mode (use built-in version)
(use-package org
  :straight nil  ;; Use built-in org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (setq org-log-done t
        calendar-week-start-day 1))

;; Language modes
(use-package haskell-mode :straight t)
(use-package rust-mode :straight t)
(use-package lua-mode :straight t)
(use-package php-mode :straight t)

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2))

(use-package json-mode :straight t)

(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package markdown-mode :straight t)
(use-package web-mode :straight t)
(use-package groovy-mode :straight t)
(use-package nginx-mode :straight t)
(use-package fountain-mode :straight t)

;; Lisp development
(use-package ac-slime :straight t)

(setq user-full-name "James M.C. Haver II")
(setq user-mail-address "mchaver@gmail.com")

;; depends on mwim
(safe-load "~/.emacs.d/init/rgbds-mode.el")
(require 'rgbds-mode)

;; make C-s case insensitive
(setq case-fold-search t)

;; Disable the splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Clean up emacs appearance
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Show empty lines and end of file
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; column number mode

(setq column-number-mode t)

;; put backup files in ~/.emacs.d/backup

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t   ; Don't delink hardlinks
      version-control t     ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20  ; how many of the newest versions to keep
      kept-old-versions 5   ; how many of the old versions to keep
      )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" default))
 '(package-selected-packages
   '(zenburn-theme yaml-mode smex markdown-mode flycheck deft autopair ac-slime)))

;; js-mode

(defun js-custom ()
  "js-mode-hook"
  (setq-default indent-tabs-mode nil)
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; custom key settings

;; Scroll by 5 lines
(defun scroll-down-5 ()
  "Scroll down 5 lines."
  (interactive)
  (forward-line 5))

(defun scroll-up-5 ()
  "Scroll up 5 lines."
  (interactive)
  (forward-line -5))

(global-set-key (kbd "C-,") 'scroll-down-5)
(global-set-key (kbd "C-.") 'scroll-up-5)

(defun insert-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'insert-line-above)

;; insert text before line
(defun insert-before-line ()
  (interactive)
  (let ((pos (point))
	(cur-max (point-max)))
    (beginning-of-line)
    (yank)(indent-according-to-mode)
    (newline-and-indent)
    (goto-char (+ pos (- (point-max) cur-max)))))

(defun newline-without-break-of-line ()
  "Move to the end of the line. Insert a new line"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

(defun smart-open-line ()
  "Insert an empty line after the current line.
   Position the cursor at its beginning, according to the
   current mode"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)

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

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
(global-set-key [(control o)] 'other-window)


;; resize windows
;; S stands for shift
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; show parens
(show-paren-mode 1)
(setq show-paren-delay 0)


;; open window
;; (global-set-key (kbd "C-x C-n") 'new-frame)

;; toggle window
(global-set-key (kbd "C-x TAB") 'other-frame)

;; (setq org-agenda-files (list "~/org/work.org"
;; 			     "~/org/home.org"))


;; prolog
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
;; (setq prolog-system 'swi)
;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
;;                                 ("\\.m$" . mercury-mode))
;;                                auto-mode-alist))

;; gradle
;; (add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))


;; ;; ;; eslint
;; ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html

;; ;; use web-mode for .jsx files
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; ;; http://www.flycheck.org/manual/latest/index.html
;; (require 'flycheck)

;; ;; turn on flychecking globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))

;; ;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'web-mode)

;; ;; customize flycheck temp file prefix
;; (setq-default flycheck-temp-prefix ".flycheck")

;; ;; disable json-jsonlist checking for json files
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(json-jsonlist)))

;; ;; https://github.com/purcell/exec-path-from-shell
;; ;; only need exec-path-from-shell on OSX
;; ;; this hopefully sets up path and other vars better
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; ;; use local eslint from node_modules before global
;; ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))
;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; ;; for better jsx syntax-highlighting in web-mode
;; ;; - courtesy of Patrick @halbtuerke
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;     (let ((web-mode-enable-part-face nil))
;;       ad-do-it)
;;     ad-do-it))

(global-set-key (kbd "M-\"") 'insert-pair)

;;

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

;; insert current data
(defun insert-date (prefix)
    "Insert the current date. 
     Without prefix-argument, use ISO format.
     With one prefix-argument, use standard US format.
     With two prefix arguments, write out the day and month name.
     With three prefix arguments, use standard European format."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ; C-u C-c d
                   ((equal prefix '(4))  "%m/%d/%Y")
                   ; C-u C-u C-c d
                   ((equal prefix '(16)) "%A, %d %B %Y")
                   ; C-u C-u C-u C-c d
                   ((equal prefix '(64))  "%d.%m.%Y")))
          (system-time-locale "en_US"))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; use TeX
(setq default-input-method 'TeX)

(setq-default indent-tabs-mode nil)

;; pwd for emacs buffer
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;; This lets you scroll the whole document with mouse wheel(not just the current page).
(setq doc-view-continuous t)

;; Turn off the mouse

;; (global-unset-key (kbd "<down-mouse-1>"))
;; (global-unset-key (kbd "<mouse-1>"))
;; (global-unset-key (kbd "<down-mouse-3>"))
;; (global-unset-key (kbd "<mouse-3>"))

(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]  
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; replace C-k, delete without yank
;; (defun test1 ()
;;   (interactive)
;;   (delete-region (point) (line-end-position)))

;; (defun test2 ()
;;   (interactive)
;;   (delete-region (line-beginning-position) (line-end-position)))

;; (defun test2 ()
;;   (interactive)
;;   (delete-region (line-beginning-position) (line-end-position)))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word, does not push
   text to the kill-ring."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word, does 
   not push text to the kill-ring."
  (interactive "p")
  (delete-word (- arg)))

;; (defun delete-line ()
;;   "Delete text from current position to end of line char, does not push text 
;;    to the kill-ring."
;;   (interactive)
;;   (delete-region
;;    (point)
;;    (progn (end-of-line 1) (point)))
;;   (delete-char 1))

(defun delete-line ()
  "Delete text from current position to end of line char, does not push text 
   to the kill-ring."  
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

(global-set-key (kbd "C-S-k") 'backward-delete-line) ; Ctrl+Shift+k
(global-set-key (kbd "M-k") 'delete-line)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (cl-remove-if-not 'buffer-file-name (buffer-list)))))
;; (defun kill-all-buffers ()
;;   (interactive)
;;   (mapcar 'kill-buffer (buffer-list))
;;   (delete-other-windows))

;; go to column
(defun er-go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(global-set-key (kbd "M-g M-c") #'er-go-to-column)

(global-set-key (kbd "C-x C-c") nil)


;; default font size
;; on new macs, these fonts are tiny
(set-face-attribute 'default nil :height 140)

;; use mac command as meta key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)


;; lilypond
;; (require 'lilypond)
(setq load-path (append (list (expand-file-name "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp")) load-path))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.lytex$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; php
;; (add-hook 'php-mode-hook 'php-enable-default-coding-style)
;; (add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)
(setq js-indent-level 2)
(setq-default c-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; C-h m, documentation
;; M-x hel-mode
;; C-x C-f
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; keep this at the bottom
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 2 1000 1000))))
