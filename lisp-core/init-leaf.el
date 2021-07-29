;;; init-leaf.el --- setup for leaf.el -*- lexical-binding: t; -*-

;;; Commentary:

;; setup for leaf.el

;;; Code:

(unless (fboundp 'leaf)
  (if (fboundp 'straight-use-package)
    (progn
      (straight-use-package 'leaf)
      (straight-use-package 'leaf-keywords))
    (error "We need straight.el installed on the system")))

(leaf leaf-keywords
  :ensure nil
  :config
  ;; initialize leaf-keywords.el
  (leaf-keywords-init))

(provide 'init-leaf)
;;; init-leaf.el ends here
