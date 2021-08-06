;;; init-theme.el --- setup emacs theme -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs theme

;;; Code:

(leaf svg-tag-mode
  :url "https://github.com/rougier/svg-tag-mode"
  :when window-system
  :straight (svg-tag-mode :host github :repo "rougier/svg-tag-mode"))

(leaf mini-frame
  :url "https://github.com/muffinmad/emacs-mini-frame"
  :when window-system
  :straight t)

;; (leaf nano
;;   :url "https://github.com/rougier/nano-emacs"
;;   :straight (nano :host github :repo "rougier/nano-emacs")
;;   :pre-setq
;;   (nano-font-family-monospaced . "Rec Mono Duotone")
;;   (nano-font-family-proportional . "Fira Sans")
;;   :require nano-base-colors
;;   :require nano-colors
;;   :require nano-faces
;;   ;; :require nano-theme-dark
;;   ;; :require nano-theme
;;   :require nano-layout
;;   ;; :require nano-modeline
;;   ;; :require nano-colors
;;   :require nano-minibuffer
;;   ;; :require nano-counsel
;;   ;; :require nano-mu4e
;;   ;; :require nano-agenda
;;   :config
;;   ;; (nano-theme)
;;   (nano-faces))

;; (leaf bespoke-themes
;;   :url "https://github.com/mclear-tools/bespoke-themes"
;;   :straight (bespoke-themes :host github :repo "mclear-tools/bespoke-themes")
;;   :pre-setq
;;   ;; Set header line
;;   (bespoke-set-mode-line . 'nil) ;; not working correctly
;;   ;; Set mode line height
;;   (bespoke-set-mode-line-size . 3)
;;   ;; Show diff lines in modeline
;;   (bespoke-set-git-diff-mode-line . t)
;;   ;; Set mode-line cleaner
;;   (bespoke-set-mode-line-cleaner . t)
;;   ;; Set evil cursor colors
;;   (bespoke-set-evil-cursors . t)
;;   ;; Use mode line visual bell
;;   (bespoke-set-visual-bell . t)
;;   ;; Set use of italics
;;   (bespoke-set-italic-comments . t)
;;   (bespoke-set-italic-keywords . t)
;;   ;; Set variable pitch
;;   (bespoke-set-variable-pitch . t)
;;   ;; Set initial theme variant
;;   (bespoke-set-theme 'dark)
;;   :config (load-theme 'bespoke t))

;; Vertical window divider
(leaf frame
  :tag "builtin"
  :custom
  (window-divider-default-right-width . 12)
  (window-divider-default-bottom-width . 1)
  (window-divider-default-places . 'right-only)
  (window-divider-mode . t)
  :hook ('before-make-frame-hook . 'window-divider-mode))

(leaf hl-todo
  :url "https://github.com/tarsius/hl-todo"
  :straight t
  :global-minor-mode global-hl-todo-mode)

;; (leaf nano-theme
;;   :url "https://github.com/rougier/nano-theme"
;;   :straight (nano-theme :host github :repo "rougier/nano-theme")
;;   ;; :require nano-theme
;;   ;; :config (nano-dark)
;;   :config (load-theme 'nano t))

;; (leaf nano-modeline
;;   :url "https://github.com/rougier/nano-modeline"
;;   :straight (nano-modeline :host github :repo "rougier/nano-modeline")
;;   :custom
;;   (nano-modeline-position . 'bottom)
;;   :require (nano-modeline))

(leaf nord-theme
  :url "https://github.com/arcticicestudio/nord-emacs"
  :straight t
  :init
  (add-to-list 'custom-safe-themes "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b")
  :custom
  (nord-region-highlight . 'frost)
  (nord-uniform-mode-lines . t)
  :config (load-theme 'nord))

(leaf doom-modeline
  :url "https://github.com/seagle0128/doom-modeline"
  :straight t
  :custom
  (doom-modeline-minor-modes . t)
  (doom-modeline-enable-word-count . t)
  (doom-modeline-mu4e . t)
  (doom-modeline-irc . t)
  :global-minor-mode t)

(provide 'init-theme)
;;; init-theme.el ends here
