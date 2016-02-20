;; -*- coding: utf-8 -*-

(global-set-key (kbd "M-|") 'split-window-horizontally)
(global-set-key (kbd "M--") 'split-window-vertically)

;; kill will add to the kill ring, delete does not.
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; magit

(global-set-key (kbd "C-x g") 'magit-status)
