;;; init-earlyinit.el --- early init for emacs27+ -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(let ((min-version "28.0"))
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
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(when (and (require 'comp nil t)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p)
           (boundp 'native-comp-eln-load-path))
  (setq native-comp-deferred-compilation (not noninteractive))
  ;; Keep all native compilation output under my-data-dir.  The default
  ;; `eln-cache/' location is removed so new GUI sessions do not create it,
  ;; while system native-lisp directories remain available for builtins.
  (let ((target-cache (file-name-as-directory
                       (expand-file-name ".local/data/eln-cache"
                                         user-emacs-directory)))
        (user-cache-root (file-name-as-directory user-emacs-directory)))
    (setq native-comp-eln-load-path
          (delete-dups
           (cons target-cache
                 (delq nil
                       (mapcar
                        (lambda (path)
                          (unless (string-prefix-p
                                   user-cache-root
                                   (file-name-as-directory
                                    (expand-file-name path)))
                            path))
                        native-comp-eln-load-path)))))))

;; Don't resize the frame when font size changes
(setq frame-inhibit-implied-resize t)

(setq load-prefer-newer t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Don't display useless messages
(setq inhibit-startup-message           t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen            t
      inhibit-default-init              t
      inhibit-x-resources               t
      inhibit-splash-screen             t)

;; So we can detect this having been loaded
(provide 'init-earlyinit)
