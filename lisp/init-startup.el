;;; init-startup.el --- startup configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; setups at startup time

;;; Code:

;; we don't need gc at startup
;; after startup, set it to 20MB
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-default-init t)

(provide 'init-startup)
;;; init-startup.el ends here
