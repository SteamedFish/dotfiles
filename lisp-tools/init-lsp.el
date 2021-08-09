;;; init-lsp.el --- Language Server Protocal -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  Language Server Protocal

;;; Code:

(leaf lsp-mode
  :url https://emacs-lsp.github.io/lsp-mode/
  :straight t
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                    (lsp-deferred))))
   (lsp-mode . (lambda ()
                 (lsp-enable-which-key-integration)
                 (add-hook 'before-save-hook #'lsp-format-buffer t t)
                 (add-hook 'before-save-hook #'lsp-organize-imports t t)))))

(leaf lsp-ui
  :url https://emacs-lsp.github.io/lsp-ui/
  :straight t)

(provide 'init-lsp)
;;; init-lsp.el ends here
