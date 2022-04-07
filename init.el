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
;; (safe-add-to-load-path "~/.emacs.d/init/reason-mode")
(safe-add-to-load-path "~/.emacs.d/init/tla-mode")
;; (safe-load "~/.emacs.d/init/verilog-mode.el")
(safe-load "~/.emacs.d/init/autopair")


(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
;;			 ("melpa" . "https://melpa.org/packages/")))

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/usr/local/share/npm/bin:Users/mchaver/.cargo/bin:" (getenv "PATH")))

(defvar mchaver/packages '(ac-slime
			   auto-complete
			   deft
                           exec-path-from-shell
			   flycheck
                           js2-mode
                           json-mode
                           haskell-mode
			   ido
			   markdown-mode
                           mwim
			   org
                           rg
                           rjsx-mode
			   rust-mode
			   smex
			   tuareg
			   web-mode
			   yaml-mode
			   zenburn-theme
			   deferred)
  "Default packages")

(defun mchaver/packages-installed-p ()
  (cl-every 'package-installed-p mchaver/packages))

(unless (mchaver/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg mchaver/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq user-full-name "J.H.")
(setq user-mail-address "mchaver@gmail.com")

;; load michelson-mode and alphanet if it is available
;; (safe-load "~/.emacs.d/init/michelson-mode.el")
;; (if file-readable-p "~/.emacs.d/init/michelson-mode.el"
;;   (if file-readable-p "~/alphanet.sh"
;;     ((setq michelson-client-command "~/alphanet.sh client")
;;      (setq michelson-alphanet t)
;;      )
;;     (message "Unable to read the file: ~/alphanet.net"))
;;p  (message "Unable to read the file: ~/.emacs.d/init/michelson-mode.el"))

;; depends on mwim
(safe-load "~/.emacs.d/init/rgbds-mode.el")
(require 'rgbds-mode)

(defun safe-load (filename)
  "Load a file if it readable, otherwise log to the *Message* buffer that it is not readable."
  (if (file-readable-p filename)
      (load filename)
    (message "Unable to read the file: %s" filename)))


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
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

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
    (zenburn-theme yaml-mode smex marmalade markdown-mode flycheck deft autopair ac-slime))))

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

;; (defun shell-cmd (cmd)
;;   "Returns the stdout output of a shell command or nil if the command returned
;;    an error"
;;   (car (ignore-errors (apply 'process-lines (split-string cmd)))))

;; (defun reason-cmd-where (cmd)
;;   (let ((where (shell-cmd cmd)))
;;     (if (not (string-equal "unknown flag ----where" where))
;;       where)))

;; (let* ((refmt-bin (shell-cmd "which bsrefmt"))
;;        (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
;;                        (shell-cmd "which ocamlmerlin")))
;;        (merlin-base-dir (when merlin-bin
;;                           (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
;;   ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
;;   (when merlin-bin
;;     (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
;;     (setq merlin-command merlin-bin))

;;   (when refmt-bin
;;     (setq refmt-command refmt-bin)))

;; (require 'reason-mode)
;; (require 'merlin)
;; (add-hook 'reason-mode-hook (lambda ()
;;                               (add-hook 'before-save-hook 'refmt-before-save)
;;                               (merlin-mode)))

;; (setq merlin-ac-setup t)

;; open window
;; (global-set-key (kbd "C-x C-n") 'new-frame)

;; toggle window
(global-set-key (kbd "C-x TAB") 'other-frame)

;; agda mode
;; (safe-load (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

;; org-mode settings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq calendar-week-start-day 1)
;; (setq org-agenda-files (list "~/org/work.org"
;; 			     "~/org/home.org"))


;; prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)  
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; gradle
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))


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
                (remove-if-not 'buffer-file-name (buffer-list)))))
;; (defun kill-all-buffers ()
;;   (interactive)
;;   (mapcar 'kill-buffer (buffer-list))
;;   (delete-other-windows))

(defun kill-ido-buffers ()
  "Kill ido buffers."  
  (interactive)
  (setq ido-virtual-buffers '())
  (setq recentf-list '()))

;; (setq ido-use-virtual-buffers nil)
;; M-x eval-expression RET (setq buffer-name-history '()) RET


(add-hook 'kill-buffer-hook
   (lambda ()
    (setq buffer-name-history
          (delete*
           (buffer-name)
           buffer-name-history :test 'string=))))

(set-frame-font "DejaVu Sans Mono:size=14" nil t)

;; setup rg for search
(rg-enable-default-bindings)

;; for emacs daemon
;; (server-start) 
(put 'erase-buffer 'disabled nil)


;; copy file path of current buffer
(defun copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path. Result is full path. If 
   `universal-argument' is called first, copy only the dir path.
   If in dired, copy the current or marked files.
   If a buffer is not file and not dired, copy value of `default-directory'."
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
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
         $fpath )))))
