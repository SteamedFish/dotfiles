;;; init-evil.el --- setup evil -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup evil

;;; Code:

(leaf evil
  :url "https://github.com/emacs-evil/evil"
  :straight t
  :global-minor-mode t
  :pre-setq
  `((evil-want-keybinding . nil)))

(leaf evil-collection
  :url "https://github.com/emacs-evil/evil-collection"
  :straight t
  :hook (evil-mode-hook . evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
