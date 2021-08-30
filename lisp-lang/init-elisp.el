;;; init-elisp.el --- emacs lisp config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  emacs lisp config

;;; Code:

(leaf eldoc
  :tag "builtin"
  :blackout t)

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


(provide 'init-elisp)
;;; init-elisp.el ends here
