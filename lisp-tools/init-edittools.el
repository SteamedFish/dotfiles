;;; init-edittools.el --- editting tools -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  editting tools

;;; Code:

(leaf indent
  :tag "builtin"
  :setq (tab-always-indent . 'complete))

(leaf files
  :tag "builtin"
  :setq (require-final-newline . t)
  ;; :hook (before-save-hook . delete-trailing-whitespace)
  :config
  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat my-data-dir
                                                   "backups"))))))

(leaf whitespace-cleanup-mode
  :url https://github.com/purcell/whitespace-cleanup-mode
  :straight t
  :blackout t
  :global-minor-mode global-whitespace-cleanup-mode)

(leaf yasnippet
  :url http://joaotavora.github.io/yasnippet/
  :straight t
  :require t
  :global-minor-mode yas-global-mode)

(leaf yasnippet-snippets
  :url https://github.com/AndreaCrotti/yasnippet-snippets
  :straight t
  :config
  (yas-reload-all))

(leaf paren
  :tag "builtin"
  :global-minor-mode show-paren-mode)

(leaf mouse
  :tag "builtin"
  :setq
  (mouse-yank-at-point . t))

(leaf ediff-wind
  :tag "builtin"
  :setq (ediff-choose-window-setup-function . 'ediff-setup-windows-plain))

(leaf undo-fu
  :url https://gitlab.com/ideasman42/emacs-undo-fu
  :straight t)

(leaf parinfer-rust-mode
  :url "https://github.com/justinbarclay/parinfer-rust-mode"
  :straight t
  :when (fboundp 'module-load)
  :hook emacs-lisp-mode-hook
  :hook lisp-interaction-mode-hook
  :hook clojure-mode-hook
  :hook scheme-mode-hook
  :hook lisp-mode-hook
  :hook racket-mode-hook
  :hook hy-mode-hook
  :blackout t
  :pre-setq
  (parinfer-rust-auto-download . t)
  `(parinfer-rust-library . ,(concat my-data-dir "data/parinfer-rust/"
                               (cond
                                 (IS-MAC "parinfer-rust-darwin.so")
                                 (IS-LINUX "parinfer-rust-linux.so")
                                 (IS-WINDOWS "parinfer-rust-windows.dll")))))

(leaf parinfer
  :url "https://github.com/DogLooksGood/parinfer-mode"
  :straight t
  :unless (fboundp 'module-load)
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          clojure-mode-hook
          scheme-mode-hook
          lisp-mode-hook
          racket-mode-hook
          hy-mode-hook) . parinfer-mode)
  :blackout t
  :setq (parinfer-extensions . '(defaults pretty-parens smart-tab smart-yank evil)))

(leaf autorevert
  :tag "builtin"
  :blackout t
  :global-minor-mode global-auto-revert-mode)

(leaf aggressive-indent
  :url https://github.com/Malabarba/aggressive-indent-mode
  :straight t
  :blackout (global-aggressive-indent-mode aggressive-indent-mode)
  :global-minor-mode global-aggressive-indent-mode)

(leaf tree-sitter
  :url https://emacs-tree-sitter.github.io/
  :straight t
  :require t
  :blackout t
  :when (functionp 'module-load)
  :ensure-system-package tree-sitter
  :global-minor-mode global-tree-sitter-mode
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :url https://github.com/emacs-tree-sitter/tree-sitter-langs
  :straight t
  :require t
  :when (functionp 'module-load))

(provide 'init-edittools)
;;; init-edittools.el ends here
