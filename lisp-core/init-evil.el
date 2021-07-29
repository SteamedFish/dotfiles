;;; init-evil.el --- setup evil -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup evil

;;; Code:

(leaf evil
  :straight t
  :hook (after-init-hook . evil-mode)
  :pre-setq
  `((evil-want-keybinding . nil)))

(leaf evil-collection
  :straight t
  :hook (evil-mode-hook . evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
