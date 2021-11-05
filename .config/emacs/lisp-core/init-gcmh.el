;;; init-gcmh.el --- setup for gcmh -*- lexical-binding: t; -*-

;;; Commentary:

;; setup for straight.el

;;; Code:

(leaf gcmh
  :url "https://gitlab.com/koral/gcmh"
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :setq
  (gcmh-idle-delay              . 1)
  (garbage-collection-messages  . t)
  `(gcmh-high-cons-threshold    . ,(* 32 1024 1024))
  :blackout t)

(provide 'init-gcmh)
;;; init-gcmh.el ends here
