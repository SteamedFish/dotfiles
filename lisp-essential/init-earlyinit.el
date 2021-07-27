
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

;; we don't need gc at startup
;; after startup, set it to 20MB
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-default-init t)

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error (concat "This config requires at least Emacs %s, "
             "but you are running Emacs %s")
      min-version emacs-version)))

;; So we can detect this having been loaded
(provide 'init-earlyinit)
