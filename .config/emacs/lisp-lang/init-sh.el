;;; init-sh.el --- sh mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  sh mode

;;; Code:

(leaf sh-script
  :tag "builtin"
  :hook
  (sh-mode-hook . lsp)
  :ensure-system-package shellcheck
  :config
  (unless (file-exists-p
           (concat lsp-server-install-dir
                   "npm/bash-language-server/bin/bash-language-server"))
    (lsp-install-server nil 'bash-ls)))

(leaf company-shell
  :url https://github.com/Alexander-Miller/company-shell
  :straight t
  :disabled t
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

(provide 'init-sh)
;;; init-sh.el ends here
