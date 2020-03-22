;;; steamedfish/tools/config.el -*- lexical-binding: t; -*-

;; gpg 可以读取在 emacs 中输入的密码
(use-package! pinentry
  :config (pinentry-start))
