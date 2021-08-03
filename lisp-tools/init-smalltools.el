;;; init-smalltools.el --- small tools that don't have a long config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  small tools that don't have a long config

;;; Code:

(leaf ssh-deploy
  :straight t
  :init
  (push '(ssh-deploy-on-explicit-save . t) safe-local-variable-values))

(leaf wakatime-mode
  :straight t
  :blackout t
  :global-minor-mode global-wakatime-mode)

(leaf rfc-mode
  :straight t
  :setq `((rfc-mode-directory . ,(concat my-data-dir "data/rfc/"))))

(leaf all-the-icons
  :straight t)

(provide 'init-smalltools)
;;; init-smalltools.el ends here
