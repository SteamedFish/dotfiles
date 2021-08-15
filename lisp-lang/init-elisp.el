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
  :pre-setq (highlight-defined-face-use-itself . t))

(leaf highlight-quoted
  :url https://github.com/Fanael/highlight-quoted
  :straight t
  :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

(leaf macrostep
  :url https://github.com/joddie/macrostep
  :doc "interactive macro-expander"
  :straight t
  :bind ("C-c e" . macrostep-expand)
  :hook (emacs-lisp-mode-hook . macrostep-mode))


(provide 'init-elisp)
;;; init-elisp.el ends here
