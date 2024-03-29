;;; init-lsp.el --- Language Server Protocal -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  Language Server Protocal

;;; Code:

(leaf lsp-mode
  :url https://emacs-lsp.github.io/lsp-mode/
  :straight t
  :setq
  ;; TODO: change lsp-keymap-prefix
  (lsp-restart                       . 'auto-restart)
  (lsp-enable-file-watchers          . nil)
  (lsp-eldoc-enable-hover            . nil)
  (lsp-eldoc-render-all              . t)
  (lsp-enable-semantic-highlighting  . t)
  (lsp-completion-provider           . :capf)
  (lsp-enable-dap-auto-configure     . t)
  (lsp-headerline-breadcrumb-enable  . nil)
  (lsp-dired-mode                    . t)
  :hook
  (lsp-mode-hook . (lambda ()
                     (lsp-enable-which-key-integration)
                     (add-hook 'before-save-hook #'lsp-format-buffer t t)
                     (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(leaf lsp-ui
  :url https://emacs-lsp.github.io/lsp-ui/
  :straight t
  :setq
  (lsp-ui-doc-position               . 'bottom)
  (lsp-ui-sideline-ignore-duplicate  . t)
  (lsp-ui-sideline-update-mode       . 'point)
  (lsp-ui-doc-enable                 . t)
  (lsp-ui-imenu-auto-refresh         . t))

(leaf company-lsp
  :url https://github.com/tigersoldier/company-lsp
  :straight t
  :disabled t ;; lsp nologer support company-lsp
  :after company
  :require t
  :config
  (push 'company-lsp company-backends))

(leaf lsp-ivy
  :url https://github.com/emacs-lsp/lsp-ivy
  :straight t
  :bind
  (:lsp-mode-map
   ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)))

(leaf dap-mode
  :url https://emacs-lsp.github.io/dap-mode/
  :straight t)

(leaf lsp-treemacs
  :url https://github.com/emacs-lsp/lsp-treemacs
  :after treemacs
  :global-minor-mode lsp-treemacs-sync-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
