;; -*- coding: utf-8 -*-


;; org default has two tags: TODO and Done
;; add INPROGRESS and UNFINISHED tag

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "INCOMPLETE" "DONE" "CANCELED" "DELEGATED" ))
      org-todo-keyword-faces '(("INPROGRESS" . "cyan")
			       ("INCOMPLETE" . "magenta")
			       ("CANCELED"   . "yellow")
			       ("DELEGATED"  . "orange")))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-show-log t)

(setq org-agend-files (list "~/journal/agenda.org"))

(setq org-startup-truncated nil)
