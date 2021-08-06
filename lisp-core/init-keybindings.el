;;; init-keybindings.el --- configure basic keybindings -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  configure basic keybindings

;;; Code:

;; S-left S-right S-up S-down to move windows
(leaf windmove
  :tag "builtin"
  :config
  (windmove-default-keybindings)
  (windmove-display-default-keybindings)
  (windmove-delete-default-keybindings))

;; undo/redo stack for windows
(leaf winner
  :tag "builtin"
  :global-minor-mode t)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
