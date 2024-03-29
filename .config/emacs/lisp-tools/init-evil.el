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
  (evil-want-keybinding                . nil)
  (evil-disable-insert-state-bindings  . t)
  :bind
  (:ctl-x-map
   ("2"                                . evil-window-split)
   ("3"                                . evil-window-vsplit))
  :custom
  (evil-search-module                  . 'evil-search)
  (evil-undo-system                    . 'undo-fu)
  :setq
  (evil-split-window-below             . t)
  (evil-vsplit-window-right            . t))

(leaf evil-collection
  :url "https://github.com/emacs-evil/evil-collection"
  :straight t
  :setq
  (evil-collection-calendar-want-org-bindings . t)
  (evil-collection-outline-bind-tab-p         . nil)
  :config
  (evil-collection-init)
  (blackout 'evil-collection-unimpaired-mode))

(leaf evil-goggles
  :url https://github.com/edkolev/evil-goggles
  :doc "displays a visual hint when editing with evil"
  :straight t
  :disabled t
  :blackout t
  :global-minor-mode t)

(provide 'init-evil)
;;; init-evil.el ends here
