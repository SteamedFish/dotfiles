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

(leaf fringe
  :tag "builtin"
  :config
  (fringe-mode '(8 . 8)))

(leaf display-line-numbers
  :tag "builtin"
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (conf-mode-hook . display-line-numbers-mode)
  (text-mode-hook . display-line-numbers-mode))

(leaf highlight-numbers
  :url https://github.com/Fanael/highlight-numbers
  :straight t
  :hook
  (prog-mode-hook . highlight-numbers-mode)
  (conf-mode-hook . highlight-numbers-mode))

(leaf simple
  :tag "builtin"
  :hook
  (text-mode-hook . visual-line-mode)
  :setq
  (visual-line-fringe-indicators . '(left-curly-arrow right-curly-arrow))
  :blackout visual-line-mode
  :global-minor-mode size-indication-mode
  :global-minor-mode column-number-mode)

(leaf rainbow-delimiters
  :url https://github.com/Fanael/rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf hl-line
  :tag "builtin"
  :global-minor-mode global-hl-line-mode)

(leaf svg-tag-mode
  :url "https://github.com/rougier/svg-tag-mode"
  :tag "TODO"
  :when IS-GUI
  ;; :global-minor-mode global-svg-tag-mode
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
  (bespoke-set-mode-line           . 'nil) ;; not working correctly
  ;; Set mode line height
  (bespoke-set-mode-line-size      . 3)
  ;; Show diff lines in modeline
  (bespoke-set-git-diff-mode-line  . t)
  ;; Set mode-line cleaner
  (bespoke-set-mode-line-cleaner   . t)
  ;; Set evil cursor colors
  (bespoke-set-evil-cursors        . t)
  ;; Use mode line visual bell
  (bespoke-set-visual-bell         . t)
  ;; Set use of italics
  (bespoke-set-italic-comments     . t)
  (bespoke-set-italic-keywords     . t)
  ;; Set variable pitch
  (bespoke-set-variable-pitch      . t)
  ;; Set initial theme variant
  (bespoke-set-theme 'dark)
  :config (load-theme 'bespoke t))

;; Vertical window divider
(leaf frame
  :tag "builtin"
  :disabled t
  :global-minor-mode window-divider-mode
  :setq
  (window-divider-default-right-width   . 12)
  (window-divider-default-bottom-width  . 1)
  (window-divider-default-places        . 'right-only))

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
  :disabled t
  :setq
  (nord-region-highlight   . 'frost)
  (nord-uniform-mode-lines . t)
  :config (load-theme 'nord t))

(leaf doom-themes
  :url https://github.com/hlissner/emacs-doom-themes
  :straight t
  :pre-setq
  (doom-themes-enable-bold      . t)
  (doom-themes-enable-italic    . t)
  (doom-themes-treemacs-theme   . "doom-colors")
  :setq
  (doom-nord-brighter-comments  . t)
  (doom-one-brighter-comments   . t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(leaf doom-modeline
  :url "https://github.com/seagle0128/doom-modeline"
  :straight t
  :setq
  (doom-modeline-minor-modes        . t)
  (doom-modeline-enable-word-count  . t)
  (doom-modeline-mu4e               . t)
  (doom-modeline-irc                . t)
  :global-minor-mode t)

(leaf hl-fill-column
  :url "https://github.com/laishulu/hl-fill-column"
  :straight t
  ;; :global-minor-mode global-hl-fill-column-mode)
  :hook (prog-mode-hook . hl-fill-column-mode))

(leaf mode-icons
  :url https://github.com/ryuslash/mode-icons
  :straight t
  :when IS-GUI
  :global-minor-mode mode-icons-mode)

(leaf page-break-lines
  :url https://github.com/purcell/page-break-lines
  :doc "turn page break symbol ^L into horizontal rules"
  :straight t
  ;; :global-minor-mode global-page-break-lines-mode
  :hook
  (text-mode-hook . page-break-lines-mode))

(leaf writeroom-mode
  :url https://github.com/joostkremers/writeroom-mode
  :doc "implements a distraction-free writing mode"
  :straight t
  :hook
  (writeroom-mode-hook . mixed-pitch-mode)
  :setq
  (writeroom-restore-window-config . t))

(leaf olivetti
  :url https://github.com/rnkn/olivetti
  :doc "A simple Emacs minor mode for a nice writing environment."
  :straight t
  :hook
  (olivetti-mode-hook . mixed-pitch-mode))

(leaf darkroom
  :url https://github.com/joaotavora/darkroom
  :doc "Remove visual distractions and focus on writing."
  :straight t
  :hook
  (darkroom-mode-hook . mixed-pitch-mode))

(leaf centered-window
  :url https://github.com/anler/centered-window-mode
  :doc "Global minor mode that centers the text of the window."
  :straight t
  :setq
  (cwm-centered-window-width  . 300)
  (cwm-use-vertical-padding   . t)
  (cwm-frame-internal-border  . 100))

(leaf solaire-mode
  :url https://github.com/hlissner/emacs-solaire-mode
  :straight t
  :global-minor-mode solaire-global-mode)

(provide 'init-theme)
;;; init-theme.el ends here
