;;; early-init.el --- Emacs 27+ pre-init config -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Disable garbage collection in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Don't init packages
(setq package-enable-at-startup nil)

;; Don't display useless UI elements
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't resize the frame when font size changes
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
