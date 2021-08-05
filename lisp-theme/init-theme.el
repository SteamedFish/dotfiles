;;; init-theme.el --- setup emacs theme -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs theme

;;; Code:

(leaf svg-tag-mode
  :straight (svg-tag-mode :host github :repo "rougier/svg-tag-mode"))

(leaf mini-frame
  :straight t)

(leaf nano
  :straight (nano :host github :repo "rougier/nano-emacs")
  :pre-setq
  (nano-font-family-monospaced . "Rec Mono Duotone")
  (nano-font-family-proportional . "Fira Sans")
  :require nano-base-colors
  :require nano-faces
  :require nano-theme-dark
  :require nano-theme
  :require nano-layout
  :require nano-colors
  :require nano-minibuffer
  ;; :require nano-counsel
  ;; :require nano-mu4e
  ;; :require nano-agenda
  :config
  (nano-faces)
  (nano-theme))


;; Vertical window divider
(leaf frame
  :custom
  (window-divider-default-right-width . 12)
  (window-divider-default-bottom-width . 1)
  (window-divider-default-places . 'right-only)
  (window-divider-mode . t)
  :hook ('before-make-frame-hook . 'window-divider-mode))

(provide 'init-theme)
;;; init-theme.el ends here
