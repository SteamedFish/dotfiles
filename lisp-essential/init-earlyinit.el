;;; init-earlyinit.el --- early init for emacs27+ -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error (concat "This config requires at least Emacs %s, "
             "but you are running Emacs %s")
      min-version emacs-version)))

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

;; Don't display useless messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      inhibit-default-init t)

;; So we can detect this having been loaded
(provide 'init-earlyinit)
