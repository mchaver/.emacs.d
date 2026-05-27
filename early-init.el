;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Emacs 27+ loads this before init.el and before package initialization.
;; It is the right place for things that should take effect as early as
;; possible: GC tuning during startup and UI elements (so they don't flash).

;; Raise the garbage-collection threshold for the duration of startup so we
;; don't pause to collect while loading packages. init.el resets this to a
;; saner value on `emacs-startup-hook'.
(setq gc-cons-threshold (* 50 1000 1000))

;; Don't let package.el auto-initialize; init.el manages packages explicitly
;; (straight.el + a manual `package-initialize').
(setq package-enable-at-startup nil)

;; Kill the tool bar and scroll bar before the first frame is drawn so they
;; never flash on screen. (The menu bar is left to init.el, which keeps it on
;; for macOS where it lives in the system bar and costs no screen space.)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
