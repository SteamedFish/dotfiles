;;; init-elisp.el --- emacs lisp config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  emacs lisp config

;;; Code:

(leaf eldoc
  :tag "builtin"
  :blackout t)

(leaf eldoc-box
  :url https://github.com/casouri/eldoc-box
  :straight t
  :when IS-GUI
  :disabled t
  :blackout (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :hook (emacs-lisp-mode-hook . eldoc-box-hover-at-point-mode))

(leaf highlight-defined
  :url "https://github.com/Fanael/highlight-defined"
  :straight t
  :hook (emacs-lisp-mode-hook . highlight-defined-mode)
  :pre-setq (highlight-defined-face-use-itself . t)
  :config
  (custom-theme-set-faces 'user
                          '(highlight-defined-function-name-face ((t (:slant italic :inherit font-lock-function-name-face))))
                          '(highlight-defined-variable-name-face ((t (:slant italic :inherit font-lock-variable-name-face))))))

(leaf highlight-quoted
  :url https://github.com/Fanael/highlight-quoted
  :straight t
  :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

(leaf macrostep
  :url https://github.com/joddie/macrostep
  :doc "interactive macro-expander"
  :straight t
  :bind ("C-c e" . macrostep-expand))

(leaf elisp-autofmt
  :url https://codeberg.org/ideasman42/emacs-elisp-autofmt
  :straight t
  :hook
  (emacs-lisp-mode-hook . elisp-autofmt-mode))


(provide 'init-elisp)
;;; init-elisp.el ends here
