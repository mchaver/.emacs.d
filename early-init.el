;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Emacs 27+ loads this before init.el and package initialization
;; Use it for UI elements to prevent them from flashing on startup

;; Clean up emacs appearance
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Increase garbage collection threshold during startup for better performance
;; Will be reset in init.el
(setq gc-cons-threshold (* 50 1000 1000))

;; Disable package.el in favor of use-package
(setq package-enable-at-startup nil)
