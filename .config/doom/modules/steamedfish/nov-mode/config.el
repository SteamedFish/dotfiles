;;; steamedfish/nov-mode/config.el  -*- lexical-binding: t; -*-

(use-package! nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
