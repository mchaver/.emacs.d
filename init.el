;; -*- mode: elisp -*-


;; structure of an elisp function
;; (defun function-name (arguments...)
;;        "optional-documentation..."
;;        (interactive argument-passing-info)     ; optional
;;        body...)


(require 'package)
(package-initialize)


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
(safe-add-to-load-path "~/.emacs.d/init/reason-mode")

;; (add-to-list 'load-path "~/.opam/4.02.3/share/emacs/site-lisp")
(safe-load "/home/james/.opam/4.02.3/share/emacs/site-lisp/tuareg-site-file.el")

(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; (setq package-archive-enable-alist '(("melpa" deft magit)))

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/usr/local/share/npm/bin:Users/mchaver/.cargo/bin:" (getenv "PATH")))

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
			   rust-mode
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

;; make C-s case insensitive
(setq case-fold-search t)

;; neotree
(global-set-key [f8] 'neotree-toggle)
;; this opens the dir for current file, but I don't like this behavior
;; (setq neo-smart-open t)

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
;; use current pane for newly opened file
(setq ido-default-file-method 'selected-window)
;; use current pane for newly switched buffer
(setq ido-default-buffer-method 'selected-window)
;; stop ido from suggesting when naming new file
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

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
    (zenburn-theme yaml-mode smex marmalade markdown-mode magit flycheck deft autopair ac-slime))))

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

;; deft settings

(setq deft-directory "~/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;; reason mode
(when (file-accessible-directory-p "~/.opam")
  (defun shell-cmd (cmd)
    "Returns the stdout output of a shell command or nil if the command returned
   an error"
    (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  (let* ((refmt-bin (or (shell-cmd "refmt ----where")
			(shell-cmd "which ~/.opam/4.02.3/bin/refmt")))
	 (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
			 (shell-cmd "which ~/.opam/4.02.3/bin/ocamlmerlin")))
	 (merlin-base-dir (shell-cmd "which ~/.opam/4.02.3/share/emacs/site-lisp")))
    ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      ;; (add-to-list 'load-path (merlin-base-dir))
      (safe-add-to-load-path "~/.opam/4.02.3/share/emacs/site-lisp")
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin)))

  (require 'reason-mode)
  (require 'merlin)
  (add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

  (setq merlin-ac-setup t))

;; open window
;; (global-set-key (kbd "C-x C-n") 'new-frame)

;; toggle window
(global-set-key (kbd "C-x TAB") 'other-frame)

;; org-mode settings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq calendar-week-start-day 1)
(setq org-agenda-files (list "~/work.org"))
