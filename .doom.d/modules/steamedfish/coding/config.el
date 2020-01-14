;;; steamedfish/coding/config.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-restart 'auto-restart
        ;; file watchers watches new files.
        ;; lsp would crash watching huge number of files
        lsp-enable-file-watchers nil
        lsp-eldoc-render-all t
        ;; experimental sementic highlighting
        lsp-enable-semantic-highlighting t))

(after! lsp-ui
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable t)

  (if (featurep 'xwidget-internal)
      (setq lsp-ui-doc-use-webkit t)))

(use-package! symbol-overlay
  :commands (symbol-overlay-put
             symbol-overlay-switch-forward
             symbol-overlay-switch-backward
             symbol-overlay-mode
             symbol-overlay-remove-all))

(map! :g "M-i" 'symbol-overlay-put
      :g "M-n" 'symbol-overlay-switch-forward
      :g "M-p" 'symbol-overlay-switch-backward
      :g "<f7>" 'symbol-overlay-mode
      :g "<f8>" 'symbol-overlay-remove-all)

(use-package! ssh-deploy
  :init
  (push '(ssh-deploy-on-explicit-save . t)
        safe-local-variable-values))
