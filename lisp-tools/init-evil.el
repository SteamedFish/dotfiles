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
  (evil-want-keybinding . nil)
  :custom
  (evil-search-module . 'evil-search)
  (evil-undo-system . 'undo-fu)
  (evil-split-window-below . t)
  (evil-vsplit-window-right . t))

(leaf evil-collection
  :url "https://github.com/emacs-evil/evil-collection"
  :straight t
  :hook (evil-mode-hook . evil-collection-init)
  :hook (magit-mode-hook . evil-collection-magit-setup)
  :hook (telega-load-hook . evil-collection-telega-setup))

(provide 'init-evil)
;;; init-evil.el ends here
