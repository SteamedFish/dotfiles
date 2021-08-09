;;; init-fonts.el --- setup emacs fonts -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs fonts

;;; Code:

(leaf mac-win
  :tag "builtin"
  :when (eq window-system 'mac)
  :global-minor-mode mac-auto-operator-composition-mode)

(provide 'init-fonts)
;;; init-fonts.el ends here
