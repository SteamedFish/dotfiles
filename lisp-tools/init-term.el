;;; init-smalltools.el --- term -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; term

;;; Code:

(leaf vterm
  :url https://github.com/akermu/emacs-libvterm
  :straight t
  :when (fboundp 'module-load)
  :hook
  (vterm-mode-hook . evil-insert-state)
  :config
  (unless (executable-find "vterm-ctrl")
    (system-packages-ensure "libvterm")))

(provide 'init-term)
;;; init-term.el ends here
