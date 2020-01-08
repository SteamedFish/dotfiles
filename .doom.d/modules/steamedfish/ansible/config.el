;;; steamedfish/ansible/config.el -*- lexical-binding: t; -*-

(use-package! ansible
  :after yaml-mode
  :init
  (add-hook! 'yaml-mode-hook #'+ansible/ansible-enable-maybe))
