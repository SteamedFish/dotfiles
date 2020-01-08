;;; steamedfish/beancount/config.el -*- lexical-binding: t; -*-


(load! "beancount.el")
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
