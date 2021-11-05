;;; init-notifier.el --- config pass -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config notifier

;;; Code:

(leaf alert
  :url https://github.com/jwiegley/alert/
  :straight t
  :config
  (when IS-MAC
    (system-packages-ensure "terminal-notifier")
    (setq alert-default-style 'notifier))
  (when IS-LINUX
    (setq alert-default-style 'notifications)))

(provide 'init-notifier)
;;; init-notifier.el ends here
