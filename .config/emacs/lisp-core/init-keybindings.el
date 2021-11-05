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

(leaf winum
  :url https://github.com/deb0ch/emacs-winum
  :doc "C-x w"
  :straight t
  :global-minor-mode winum-mode)

(leaf simple
  :tag "builtin"
  :doc "Mac like keybindings"
  :bind
  ("s-a"         . mark-whole-buffer)
  ("s-c"         . kill-ring-save)
  ("s-l"         . goto-line)
  ("s-v"         . yank)
  ("s-z"         . undo)
  ("s-<return>"  . toggle-frame-maximized))


(provide 'init-keybindings)
;;; init-keybindings.el ends here
