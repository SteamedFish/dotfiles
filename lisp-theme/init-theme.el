;;; init-theme.el --- setup emacs theme -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs theme

;;; Code:

(leaf startup
  :tag "builtin"
  :when IS-GUI
  :hook (window-setup-hook . toggle-frame-maximized)
  :hook (window-setup-hook . (lambda () (modify-frame-parameters nil (list (cons 'alpha 80)))))
  :init
  (defun my-toggle-transparency ()
    "toggle frame transparency between 80 and 100"
    (interactive)
    (let* ((oldalpha (frame-parameter nil 'alpha))
           ;; oldalpha may be nil
           (oldalpha (if oldalpha oldalpha 100)))
      (if (eq oldalpha 100)
          (modify-frame-parameters nil (list (cons 'alpha 80)))
        (modify-frame-parameters nil (list (cons 'alpha 100))))))
  :bind ("s-u" . my-toggle-transparency))


(leaf display-line-numbers
  :tag "builtin"
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (conf-mode-hook . display-line-numbers-mode)
  (text-mode-hook . display-line-numbers-mode))

(leaf simple
  :tag "builtin"
  :global-minor-mode column-number-mode)

(leaf hl-line
  :tag "builtin"
  :global-minor-mode global-hl-line-mode)

(leaf svg-tag-mode
  :url "https://github.com/rougier/svg-tag-mode"
  :when IS-GUI
  :straight (svg-tag-mode :host github :repo "rougier/svg-tag-mode"))

(leaf nano
  :url "https://github.com/rougier/nano-emacs"
  :straight (nano :host github :repo "rougier/nano-emacs")
  :disabled t
  :pre-setq
  (nano-font-family-monospaced . "Rec Mono Duotone")
  (nano-font-family-proportional . "Fira Sans")
  :require nano-base-colors
  :require nano-colors
  :require nano-faces
  ;; :require nano-theme-dark
  ;; :require nano-theme
  :require nano-layout
  ;; :require nano-modeline
  ;; :require nano-colors
  :require nano-minibuffer
  ;; :require nano-counsel
  ;; :require nano-mu4e
  ;; :require nano-agenda
  :config
  ;; (nano-theme)
  (nano-faces))

(leaf bespoke-themes
  :url "https://github.com/mclear-tools/bespoke-themes"
  :straight (bespoke-themes :host github :repo "mclear-tools/bespoke-themes")
  :disabled t
  :pre-setq
  ;; Set header line
  (bespoke-set-mode-line . 'nil) ;; not working correctly
  ;; Set mode line height
  (bespoke-set-mode-line-size . 3)
  ;; Show diff lines in modeline
  (bespoke-set-git-diff-mode-line . t)
  ;; Set mode-line cleaner
  (bespoke-set-mode-line-cleaner . t)
  ;; Set evil cursor colors
  (bespoke-set-evil-cursors . t)
  ;; Use mode line visual bell
  (bespoke-set-visual-bell . t)
  ;; Set use of italics
  (bespoke-set-italic-comments . t)
  (bespoke-set-italic-keywords . t)
  ;; Set variable pitch
  (bespoke-set-variable-pitch . t)
  ;; Set initial theme variant
  (bespoke-set-theme 'dark)
  :config (load-theme 'bespoke t))

;; Vertical window divider
(leaf frame
  :tag "builtin"
  :disabled t
  :global-minor-mode window-divider-mode
  :setq
  (window-divider-default-right-width . 12)
  (window-divider-default-bottom-width . 1)
  (window-divider-default-places . 'right-only))

(leaf hl-todo
  :url "https://github.com/tarsius/hl-todo"
  :straight t
  :global-minor-mode global-hl-todo-mode)

(leaf nano-theme
  :url "https://github.com/rougier/nano-theme"
  :straight (nano-theme :host github :repo "rougier/nano-theme")
  :disabled t
  ;; :require nano-theme
  ;; :config (nano-dark)
  :config (load-theme 'nano t))

(leaf nano-modeline
  :url "https://github.com/rougier/nano-modeline"
  :straight (nano-modeline :host github :repo "rougier/nano-modeline")
  :disabled t
  :setq
  (nano-modeline-position . 'bottom)
  :require (nano-modeline))

(leaf nord-theme
  :url "https://github.com/arcticicestudio/nord-emacs"
  :straight t
  :init
  (add-to-list 'custom-safe-themes "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b")
  :setq
  (nord-region-highlight . 'frost)
  (nord-uniform-mode-lines . t)
  :config (load-theme 'nord))

(leaf doom-modeline
  :url "https://github.com/seagle0128/doom-modeline"
  :straight t
  :setq
  (doom-modeline-minor-modes . t)
  (doom-modeline-enable-word-count . t)
  (doom-modeline-mu4e . t)
  (doom-modeline-irc . t)
  :global-minor-mode t)

(leaf hl-fill-column
  :url "https://github.com/laishulu/hl-fill-column"
  :straight t
  :hook (prog-mode-hook . hl-fill-column-mode))
  ;; :global-minor-mode global-hl-fill-column-mode)

(leaf mode-icons
  :url https://github.com/ryuslash/mode-icons
  :straight t
  :when IS-GUI
  :global-minor-mode mode-icons-mode)

(provide 'init-theme)
;;; init-theme.el ends here
