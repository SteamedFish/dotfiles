;;; init-edittools.el --- editting tools -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  editting tools

;;; Code:

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

(provide 'init-edittools)
;;; init-edittools.el ends here
