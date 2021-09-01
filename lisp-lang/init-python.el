;;; init-python.el --- python mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  python mode

;;; Code:

(leaf python
  :tag "builtin"
  :hook
  ;; emacs-lsp by default will use pylsp as language server
  ;; we may also enable lsp-python-ms or pyright
  ;; TODO: check if pylsp has installed
  (python-mode-hook . lsp))

(leaf lsp-python-ms
  :url https://github.com/emacs-lsp/lsp-python-ms
  ;; this plugin will automatically install the language server
  :straight t
  :disabled t)

(leaf lsp-pyright
  :url https://github.com/emacs-lsp/lsp-pyright
  :straight t
  :disabled t)

(leaf anaconda-mode
  :url https://github.com/pythonic-emacs/anaconda-mode
  :straight t
  :blackout t
  :hook
  (python-mode-hook . anaconda-mode)
  (python-mode-hook . anaconda-eldoc-mode))

(leaf company-anaconda
  :url https://github.com/pythonic-emacs/company-anaconda
  :straight t
  :hook
  (python-mode-hook . anaconda-mode)
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(provide 'init-python)
;;; init-python.el ends here
