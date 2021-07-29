;;; init-gcmh.el --- setup for gcmh -*- lexical-binding: t; -*-

;;; Commentary:

;; setup for straight.el

;;; Code:

(leaf gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode))

(provide 'init-gcmh)
;;; init-gcmh.el ends here
