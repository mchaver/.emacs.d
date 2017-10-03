;; -*- mode: elisp -*-

(require 'package)
(package-initialize)

;; load init files
(add-to-list 'load-path "~/.emacs.d/init")

(load "init-key-bindings")

(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; (setq package-archive-enable-alist '(("melpa" deft magit)))

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/usr/local/share/npm/bin" (getenv "PATH")))

(defvar mchaver/packages '(ac-slime
			   auto-complete
			   autopair
			   deft
			   flycheck
			   ghc
			   ido
			   magit
			   markdown-mode
			   marmalade
			   neotree
			   org
			   smex
			   yaml-mode
			   zenburn-theme)
  "Default packages")


(defun mchaver/packages-installed-p ()
  (cl-every 'package-installed-p mchaver/packages))

(unless (mchaver/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg mchaver/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq user-full-name "James M.C. Haver II")
(setq user-mail-address "mchaver@gmail.com")

;; neotree
(global-set-key [f8] 'neotree-toggle)

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

;; Smex settings
;; provides search history for M-x
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido
;; navigate the file system
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

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

;; YAML

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; theme

(load-theme 'zenburn t)

;; font size

(set-face-attribute 'default nil :height 100)

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
    (zenburn-theme yaml-mode smex marmalade markdown-mode magit flycheck deft autopair ac-slime)))
 )

;; js-mode

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; custom key settings

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

;; deft settings

(setq deft-directory "~/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

