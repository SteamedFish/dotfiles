;;; steamedfish/imenu-list/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +imenu-list/toggle
    (interactive)
    ;; TODO detect if lsp is enabled
    (imenu-list-smart-toggle))
